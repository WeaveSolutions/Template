# Tauri Desktop Application

This is the Tauri-based desktop application for Taurte, featuring a **consolidated architecture** with two complementary modes:

1. **Next.js Mode (Primary)**: Shared Next.js web application with desktop-specific components
2. **Svelte Standalone Mode**: Cross-platform Svelte frontend sharing resources with Tauri Mobile

## Consolidated Architecture Overview

The Tauri Desktop application has been architected to provide maximum code reuse and cross-platform consistency:

- **Primary Experience**: Next.js mode leverages the full web application with desktop enhancements
- **Unified Mobile/Desktop**: Svelte standalone mode shares components, stores, and authentication with Tauri Mobile
- **Cross-Platform Authentication**: CRA (Central Rank Authority) authentication system unified across all platforms
- **Shared Resources**: Components, stores, and utilities shared between desktop and mobile Tauri apps

## Architecture Modes

### Next.js Mode (Primary Desktop Experience)
- **Frontend**: Shared Next.js application with desktop-specific components
- **Backend**: Rust (Tauri) with desktop integration commands
- **Communication**: Tauri Commands and Event System
- **Port**: 3000 (Next.js dev server)
- **Features**: Full web app parity + desktop enhancements (file system, notifications, system integration)
- **Use Case**: Primary desktop experience with complete feature set

### Svelte Standalone Mode (Cross-Platform Unified)
- **Frontend**: Svelte 5+ with TypeScript, sharing resources with Tauri Mobile
- **Backend**: Rust (Tauri) with unified command set matching mobile
- **Communication**: Tauri Commands and Event System
- **Port**: 1420 (Vite dev server)
- **Shared Resources**: Components, stores, authentication from `apps/tauri-mobile`
- **Features**: Cross-platform UI consistency, shared authentication, unified user experience
- **Use Case**: Development testing, cross-platform consistency validation, lightweight desktop option

## Development Setup

### Prerequisites

1. **Rust**: Install from [rustup.rs](https://rustup.rs/)
2. **Windows**: Visual Studio C++ Build Tools
3. **macOS**: Xcode Command Line Tools
4. **Linux**: Various system dependencies (see Tauri docs)

### Installation

```bash
# From the project root
pnpm install

# Navigate to Tauri app
cd apps/tauri

# Install Rust dependencies
cd src-tauri
cargo build
```

### Running in Development

#### Next.js Mode (Primary - Default)
```bash
# From apps/tauri directory
pnpm dev
# or explicitly
pnpm dev:next-mode
```

This will:
1. Start Next.js dev server on http://localhost:3000
2. Launch Tauri in development mode connecting to the Next.js server
3. Provide full web app features with desktop enhancements

#### Svelte Standalone Mode (Cross-Platform Unified)
```bash
# From apps/tauri directory
pnpm desktop
# or explicitly
pnpm dev:svelte-mode
```

This will:
1. Start Vite dev server on http://localhost:1420
2. Launch Tauri in development mode with Svelte frontend
3. Load shared resources from the shared package (`packages/shared-tauri`)
4. Provide cross-platform consistent UI and authentication

### Building for Production

#### Next.js Mode (Primary)
```bash
# From apps/tauri directory
pnpm build
# or explicitly
pnpm build:next-mode
```

This will:
1. Build Next.js as a static export
2. Build Tauri application with the exported Next.js frontend
3. Create platform-specific installers

#### Svelte Standalone Mode
```bash
# From apps/tauri directory
pnpm build:svelte-mode
```

This will:
1. Build Svelte app with Vite
2. Build Tauri application with the built Svelte frontend
3. Create lightweight platform-specific installers

### Cross-Platform Build Targets

```bash
# Windows
pnpm tauri build --target x86_64-pc-windows-msvc

# macOS Intel
pnpm tauri build --target x86_64-apple-darwin

# macOS Apple Silicon
pnpm tauri build --target aarch64-apple-darwin

# Linux
pnpm tauri build --target x86_64-unknown-linux-gnu
2. Build Tauri application with the static files embedded

## Tauri Event System

The app uses Tauri's event system for real-time communication between the Rust backend and Next.js frontend.

### Available Events

#### Authentication Events
- `auth:started` - Login process initiated
- `auth:success` - Login successful (includes user data)
- `auth:logout` - User logged out

#### Process Events
- `process:started` - Long-running process started
- `process:progress` - Progress update (includes progress percentage and message)
- `process:completed` - Process finished

#### System Events
- `system:status` - Periodic system status updates (CPU/memory usage)
- `app:ready` - Application fully loaded

#### Notification Events
- `notification:show` - Display notification to user

### Usage in React Components

```tsx
import { useTauriEvent, tauriCommands, TAURI_EVENTS } from '@shared/utils/tauri';

// Listen to events
useTauriEvent(TAURI_EVENTS.AUTH_SUCCESS, (user) => {
  console.log('User logged in:', user);
});

// Call commands
const result = await tauriCommands.authenticate(email, password);
```

## Project Structure

```
apps/tauri/
├── src-tauri/           # Rust backend
│   ├── src/
│   │   └── main.rs      # Main application logic
│   ├── Cargo.toml       # Rust dependencies
│   ├── build.rs         # Build script
│   └── tauri.conf.json  # Tauri configuration
├── package.json         # Node.js dependencies
└── README.md           # This file
```

## Security

Tauri provides better security than Electron by:
- Running web content in the OS native webview (not Chromium)
- Sandboxing by default
- Explicit permission system for native APIs
- Smaller attack surface

## Performance

Benefits over Electron:
- 90% smaller bundle size (~20MB vs ~200MB)
- 60% less memory usage
- Faster startup times
- Native OS integration

## Deployment

### Windows
- Produces `.msi` and `.exe` installers
- Code signing supported

### macOS
- Produces `.dmg` and `.app` bundles
- Notarization supported

### Linux
- Produces `.deb`, `.rpm`, and `.AppImage` packages

## Troubleshooting

### Common Issues

1. **Rust not found**: Install Rust from rustup.rs
2. **Build tools missing**: Install platform-specific build tools
3. **Next.js export fails**: Check for dynamic routes or SSR-only features
4. **Icons missing**: Generate icons using `pnpm tauri icon`

### Debug Mode

Enable debug logging:
```bash
RUST_LOG=debug pnpm dev
```

## Resources

- [Tauri Documentation](https://tauri.app)
- [Tauri + Next.js Guide](https://tauri.app/v1/guides/getting-started/setup/next-js)
- [Rust Book](https://doc.rust-lang.org/book/)
