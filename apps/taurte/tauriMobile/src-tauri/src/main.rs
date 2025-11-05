// Prevents additional console window on Windows in release, DO NOT REMOVE!!
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use tauri::{AppHandle, Emitter, Manager, State};
use tokio::sync::Mutex;
use uuid::Uuid;

// Mobile-specific imports
#[cfg(mobile)]
use tauri_plugin_notification::NotificationExt;

#[derive(Debug, Serialize, Deserialize, Clone)]
struct User {
    id: String,
    name: String,
    email: String,
    avatar_url: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
struct LoginRequest {
    email: String,
    password: String,
}

#[derive(Debug, Serialize, Deserialize)]
struct ApiResponse<T> {
    success: bool,
    data: Option<T>,
    error: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
struct AppState {
    user: Option<User>,
    is_authenticated: bool,
    settings: HashMap<String, String>,
}

type AppStateType = Arc<Mutex<AppState>>;
use std::sync::Arc;

// Tauri commands
#[tauri::command]
async fn get_app_info() -> Result<HashMap<String, String>, String> {
    let mut info = HashMap::new();
    info.insert("name".to_string(), "Nexpo Mobile".to_string());
    info.insert("version".to_string(), env!("CARGO_PKG_VERSION").to_string());
    info.insert("platform".to_string(), get_platform());
    Ok(info)
}

#[tauri::command]
fn get_platform() -> String {
    #[cfg(target_os = "android")]
    return "android".to_string();
    
    #[cfg(target_os = "ios")]
    return "ios".to_string();
    
    #[cfg(target_os = "windows")]
    return "windows".to_string();
    
    #[cfg(target_os = "macos")]
    return "macos".to_string();
    
    #[cfg(target_os = "linux")]
    return "linux".to_string();
    
    "unknown".to_string()
}

#[tauri::command]
async fn authenticate_user(
    email: String,
    password: String,
    state: State<'_, AppStateType>,
    app: AppHandle,
) -> Result<ApiResponse<User>, String> {
    // Simulate authentication logic
    if email.is_empty() || password.is_empty() {
        return Ok(ApiResponse {
            success: false,
            data: None,
            error: Some("Email and password are required".to_string()),
        });
    }

    // Mock user for demo purposes
    let user = User {
        id: Uuid::new_v4().to_string(),
        name: email.split('@').next().unwrap_or("User").to_string(),
        email: email.clone(),
        avatar_url: None,
    };

    // Update app state
    let mut app_state = state.lock().await;
    app_state.user = Some(user.clone());
    app_state.is_authenticated = true;
    drop(app_state);

    // Emit authentication event
    app.emit("auth-changed", &user)
        .map_err(|e| e.to_string())?;

    // Send notification on mobile platforms
    #[cfg(mobile)]
    {
        if let Err(e) = app
            .notification()
            .builder()
            .title("Welcome!")
            .body(&format!("Welcome back, {}!", user.name))
            .show()
        {
            eprintln!("Failed to show notification: {}", e);
        }
    }

    Ok(ApiResponse {
        success: true,
        data: Some(user),
        error: None,
    })
}

#[tauri::command]
async fn logout_user(
    state: State<'_, AppStateType>,
    app: AppHandle,
) -> Result<ApiResponse<()>, String> {
    let mut app_state = state.lock().await;
    app_state.user = None;
    app_state.is_authenticated = false;
    drop(app_state);

    // Emit logout event
    app.emit("auth-changed", serde_json::Value::Null)
        .map_err(|e| e.to_string())?;

    Ok(ApiResponse {
        success: true,
        data: Some(()),
        error: None,
    })
}

#[tauri::command]
async fn get_user_profile(state: State<'_, AppStateType>) -> Result<ApiResponse<User>, String> {
    let app_state = state.lock().await;
    
    if let Some(user) = &app_state.user {
        Ok(ApiResponse {
            success: true,
            data: Some(user.clone()),
            error: None,
        })
    } else {
        Ok(ApiResponse {
            success: false,
            data: None,
            error: Some("User not authenticated".to_string()),
        })
    }
}

#[tauri::command]
async fn update_setting(
    key: String,
    value: String,
    state: State<'_, AppStateType>,
    app: AppHandle,
) -> Result<ApiResponse<()>, String> {
    let mut app_state = state.lock().await;
    app_state.settings.insert(key.clone(), value.clone());
    drop(app_state);

    // Emit settings change event
    let mut settings_change = HashMap::new();
    settings_change.insert(key, value);
    app.emit("settings-changed", &settings_change)
        .map_err(|e| e.to_string())?;

    Ok(ApiResponse {
        success: true,
        data: Some(()),
        error: None,
    })
}

#[tauri::command]
async fn get_settings(state: State<'_, AppStateType>) -> Result<HashMap<String, String>, String> {
    let app_state = state.lock().await;
    Ok(app_state.settings.clone())
}

#[tauri::command]
async fn send_notification(
    title: String,
    body: String,
    app: AppHandle,
) -> Result<ApiResponse<()>, String> {
    #[cfg(mobile)]
    {
        app.notification()
            .builder()
            .title(&title)
            .body(&body)
            .show()
            .map_err(|e| format!("Failed to send notification: {}", e))?;
    }

    Ok(ApiResponse {
        success: true,
        data: Some(()),
        error: None,
    })
}

// Background task for periodic updates
async fn background_task(app: AppHandle) {
    let mut interval = tokio::time::interval(tokio::time::Duration::from_secs(30));
    
    loop {
        interval.tick().await;
        
        // Emit periodic update event
        if let Err(e) = app.emit("background-update", "ping") {
            eprintln!("Failed to emit background update: {}", e);
        }
    }
}

#[cfg_attr(mobile, tauri::mobile_entry_point)]
pub fn run() {
    let initial_state = AppState {
        user: None,
        is_authenticated: false,
        settings: HashMap::new(),
    };

    tauri::Builder::default()
        .manage(Arc::new(Mutex::new(initial_state)))
        .plugin(tauri_plugin_shell::init())
        .plugin(tauri_plugin_os::init())
        .plugin(tauri_plugin_fs::init())
        .plugin(tauri_plugin_dialog::init())
        .plugin(tauri_plugin_http::init())
        .plugin(tauri_plugin_notification::init())
        .setup(|app| {
            let app_handle = app.handle().clone();
            
            // Start background task
            tokio::spawn(background_task(app_handle));
            
            #[cfg(mobile)]
            {
                // Request notification permissions on mobile
                if let Err(e) = app.notification().request_permission() {
                    eprintln!("Failed to request notification permissions: {}", e);
                }
            }
            
            Ok(())
        })
        .invoke_handler(tauri::generate_handler![
            get_app_info,
            get_platform,
            authenticate_user,
            logout_user,
            get_user_profile,
            update_setting,
            get_settings,
            send_notification
        ])
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}

fn main() {
    run();
}
