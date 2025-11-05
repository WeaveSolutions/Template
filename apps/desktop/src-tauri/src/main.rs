// Prevents additional console window on Windows in release, DO NOT REMOVE!!
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Mutex;
use std::time::Duration;
use tauri::{command, generate_handler, App, Manager, Window};
use tauri::api::dialog;
use tauri::api::notification::Notification;
use tokio::time::sleep;
use uuid::Uuid;

#[derive(Debug, Serialize, Deserialize, Clone)]
struct AppState {
    connection_status: String,
    user: Option<User>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
struct User {
    id: String,
    name: String,
    email: String,
}

#[derive(Debug, Serialize, Deserialize)]
struct ProgressPayload {
    progress: f64,
    message: String,
}

#[derive(Debug, Serialize, Deserialize)]
struct NotificationPayload {
    title: String,
    body: String,
    notification_type: String,
}

// State wrapper for thread-safe access
struct AppStateWrapper(Mutex<AppState>);

// Learn more about Tauri commands at https://tauri.app/v1/guides/features/command
#[tauri::command]
fn greet(name: &str) -> String {
    format!("Hello, {}! You've been greeted from Rust!", name)
}

// Command to authenticate user
#[tauri::command]
async fn authenticate(
    email: String,
    password: String,
    state: State<'_, AppStateWrapper>,
    app_handle: tauri::AppHandle,
) -> Result<User, String> {
    // Emit authentication started event
    app_handle.emit_all("auth:started", ()).unwrap();
    
    // Simulate authentication process
    tokio::time::sleep(Duration::from_millis(1000)).await;
    
    // In real app, you would validate credentials
    let user = User {
        id: uuid::Uuid::new_v4().to_string(),
        name: "Test User".to_string(),
        email: email.clone(),
    };
    
    // Update state
    let mut app_state = state.0.lock().unwrap();
    app_state.user = Some(user.clone());
    app_state.connection_status = "authenticated".to_string();
    
    // Emit authentication success event
    app_handle.emit_all("auth:success", &user).unwrap();
    
    Ok(user)
}

// Command to logout
#[tauri::command]
fn logout(state: State<'_, AppStateWrapper>, app_handle: tauri::AppHandle) -> Result<(), String> {
    let mut app_state = state.0.lock().unwrap();
    app_state.user = None;
    app_state.connection_status = "disconnected".to_string();
    
    // Emit logout event
    app_handle.emit_all("auth:logout", ()).unwrap();
    
    Ok(())
}

// Command to get current user
#[tauri::command]
fn get_current_user(state: State<'_, AppStateWrapper>) -> Result<Option<User>, String> {
    let app_state = state.0.lock().unwrap();
    Ok(app_state.user.clone())
}

// Command to simulate a long-running process with progress updates
#[tauri::command]
async fn process_data(
    data: String,
    app_handle: tauri::AppHandle,
) -> Result<String, String> {
    // Emit process started event
    app_handle.emit_all("process:started", &data).unwrap();
    
    // Simulate processing with progress updates
    for i in 0..=10 {
        let progress = (i as f64) / 10.0;
        let payload = ProgressPayload {
            progress,
            message: format!("Processing step {} of 10", i),
        };
        
        app_handle.emit_all("process:progress", &payload).unwrap();
        tokio::time::sleep(Duration::from_millis(500)).await;
    }
    
    // Emit process completed event
    app_handle.emit_all("process:completed", &data).unwrap();
    
    Ok(format!("Processed: {}", data))
}

// Command to send a notification
#[tauri::command]
fn send_notification(
    title: String,
    body: String,
    notification_type: String,
    app_handle: tauri::AppHandle,
) -> Result<(), String> {
    let payload = NotificationPayload {
        title,
        body,
        notification_type,
    };
    
    // Emit notification event
    app_handle.emit_all("notification:show", &payload)
        .map_err(|e| e.to_string())?;
    
    Ok(())
}

// Background task that emits periodic system status
async fn emit_system_status(app_handle: tauri::AppHandle) {
    let mut interval = tokio::time::interval(Duration::from_secs(5));
    
    loop {
        interval.tick().await;
        
        #[derive(Serialize)]
        struct SystemStatus {
            timestamp: String,
            cpu_usage: f64,
            memory_usage: f64,
        }
        
        let status = SystemStatus {
            timestamp: chrono::Utc::now().to_rfc3339(),
            cpu_usage: rand::random::<f64>() * 100.0,
            memory_usage: rand::random::<f64>() * 100.0,
        };
        
        let _ = app_handle.emit_all("system:status", &status);
    }
}

// Desktop-specific commands

#[derive(Debug, Serialize, Deserialize)]
struct SystemInfo {
    os: String,
    arch: String,
    version: String,
}

#[tauri::command]
fn get_system_info() -> SystemInfo {
    SystemInfo {
        os: std::env::consts::OS.to_string(),
        arch: std::env::consts::ARCH.to_string(),
        version: "1.0.0".to_string(),
    }
}

#[tauri::command]
async fn open_file_dialog(window: Window) -> Result<Option<String>, String> {
    let file = dialog::FileDialogBuilder::new()
        .set_title("Select a file")
        .pick_file()
        .await;
    
    match file {
        Some(path) => Ok(Some(path.to_string_lossy().to_string())),
        None => Ok(None),
    }
}

#[tauri::command]
async fn save_file_dialog(window: Window) -> Result<Option<String>, String> {
    let file = dialog::FileDialogBuilder::new()
        .set_title("Save file")
        .save_file()
        .await;
    
    match file {
        Some(path) => Ok(Some(path.to_string_lossy().to_string())),
        None => Ok(None),
    }
}

#[tauri::command]
async fn show_native_notification(title: String, body: String) -> Result<(), String> {
    Notification::new("nexpo-desktop")
        .title(&title)
        .body(&body)
        .show()
        .map_err(|e| e.to_string())?;
    
    Ok(())
}

#[tauri::command]
async fn check_for_updates() -> Result<String, String> {
    // Simulate update check
    tokio::time::sleep(Duration::from_secs(2)).await;
    
    // In a real app, you would check for updates here
    Ok("No updates available. You're running the latest version.".to_string())
}

#[tauri::command]
async fn get_app_version() -> Result<String, String> {
    Ok(env!("CARGO_PKG_VERSION").to_string())
}

fn main() {
    tauri::Builder::default()
        .manage(AppStateWrapper(Mutex::new(AppState {
            connection_status: "disconnected".to_string(),
            user: None,
        })))
        .invoke_handler(tauri::generate_handler![
            greet,
            authenticate,
            logout,
            get_current_user,
            process_data,
            send_notification,
            get_system_info,
            open_file_dialog,
            save_file_dialog,
            show_native_notification,
            check_for_updates,
            get_app_version,
        ])
        .setup(|app| {
            let app_handle = app.handle();
            
            // Start background task for system status
            tauri::async_runtime::spawn(async move {
                emit_system_status(app_handle).await;
            });
            
            // Emit app ready event
            app.emit_all("app:ready", ()).unwrap();
            
            Ok(())
        })
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}

// Add rand dependency for demo purposes
use rand;
