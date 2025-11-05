Rust [https://www.rust-lang.org/](Programming Language)

Actix [https://actix.rs/](Web Framework)
Quinn for QUIC [https://docs.rs/quinn/latest/quinn/](QUIC Protocol)
WebTransport [https://docs.rs/webtransport-quinn/latest/webtransport_quinn/](WebTransport Protocol)

# Rust Installation Guide (Latest Version)

## Overview

- Official Rust site: [https://www.rust-lang.org/learn/get-started]
- Rust is a systems programming language focused on safety, speed, and concurrency.
- Current stable version (as of July 2025): Rust 1.79.x
- Package manager: **Cargo** (built-in, no alternatives needed)
- Excellent for APIs, web services, and high-performance applications

---

## Windows Installation

### Step 1: Download Rustup

- Visit: [https://rustup.rs/]
- Download the `rustup-init.exe` installer
- Run the installer and follow the prompts
- **Important**: Choose "1) Proceed with installation (default)"

### Step 2: Restart Terminal and Verify

```powershell
# Close and reopen PowerShell/Command Prompt
rustc --version
cargo --version
rustup --version
```

### Step 3: Update Rust (Optional)

```powershell
rustup update
```

### Step 4: Create First Rust Project

```powershell
cargo new my-rust-project
cd my-rust-project
cargo build
cargo run
```

---

## macOS Installation

### Step 1: Install Rust via Rustup

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

### Step 2: Source the Environment

```bash
source ~/.cargo/env
```

### Step 3: Verify Installation

```bash
rustc --version
cargo --version
rustup --version
```

### Step 4: Create First Rust Project

```bash
cargo new my-rust-project
cd my-rust-project
cargo build
cargo run
```

---

## Ubuntu / Debian Linux Installation

### Step 1: Install Dependencies

```bash
sudo apt update
sudo apt install curl build-essential
```

### Step 2: Install Rust via Rustup

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

### Step 3: Source the Environment

```bash
source ~/.cargo/env
```

### Step 4: Verify Installation

```bash
rustc --version
cargo --version
rustup --version
```

### Step 5: Create First Rust Project

```bash
cargo new my-rust-project
cd my-rust-project
cargo build
cargo run
```

---

## Quick Start: Actix Web Framework

### Step 1: Create Actix Web Project

```bash
cargo new actix-web-api
cd actix-web-api
```

### Step 2: Add Actix Dependencies

Update `Cargo.toml`:
```toml
[dependencies]
actix-web = "4.4"
tokio = { version = "1.0", features = ["full"] }
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
```

### Step 3: Create Simple API

Replace `src/main.rs`:
```rust
use actix_web::{web, App, HttpResponse, HttpServer, Result};
use serde::{Deserialize, Serialize};

#[derive(Serialize)]
struct ApiResponse {
    message: String,
    status: String,
    port: u16,
}

#[derive(Serialize)]
struct User {
    user_id: String,
    name: String,
    platform: String,
}

async fn health() -> Result<HttpResponse> {
    let response = ApiResponse {
        message: "Hello from Rust Actix API!".to_string(),
        status: "success".to_string(),
        port: 8050,
    };
    Ok(HttpResponse::Ok().json(response))
}

async fn get_user(path: web::Path<String>) -> Result<HttpResponse> {
    let user_id = path.into_inner();
    let user = User {
        user_id: user_id.clone(),
        name: format!("User {}", user_id),
        platform: "Rust".to_string(),
    };
    Ok(HttpResponse::Ok().json(user))
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    println!("🚀 Rust Actix API starting on http://localhost:8050");
    
    HttpServer::new(|| {
        App::new()
            .route("/health", web::get().to(health))
            .route("/api/users/{id}", web::get().to(get_user))
    })
    .bind("127.0.0.1:8050")?
    .run()
    .await
}
```

### Step 4: Run Development Server

```bash
cargo run
```

Visit `http://localhost:8050/health` to see your Rust Actix API.

---

## Cargo Package Management

### Common Cargo Commands

```bash
# Create new project
cargo new my_project

# Build project
cargo build

# Run project
cargo run

# Run tests
cargo test

# Add dependency
cargo add serde

# Update dependencies
cargo update

# Check code without building
cargo check

# Format code
cargo fmt

# Lint code
cargo clippy
```

---

## Summary Table

| Platform        | Install Method    | Rust Command | Package Manager | Project Init        |
|-----------------|-------------------|--------------|-----------------|--------------------|
| Windows         | rustup-init.exe   | `rustc`      | `cargo`         | `cargo new`        |
| macOS           | rustup.rs script  | `rustc`      | `cargo`         | `cargo new`        |
| Ubuntu/Debian   | rustup.rs script  | `rustc`      | `cargo`         | `cargo new`        |

---

## Why Rust for APIs?

- **Memory safety** without garbage collection
- **Zero-cost abstractions** for performance
- **Excellent concurrency** with async/await
- **Strong type system** prevents runtime errors
- **Fast compilation** and execution
- **Growing ecosystem** for web development

Actix is a web framework for Rust that provides features for routing, templating, authentication, and creating API backends. You can use it to build a performant server-side API that handles data processing and logic in Rust.
