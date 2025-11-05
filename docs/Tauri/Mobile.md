# Tauri Mobile with Svelte

## Overview

Nexpo's Tauri mobile application combines the power of Rust's performance with Svelte's reactive frontend to create lightweight, cross-platform mobile applications. Built on Tauri 2.0+, this implementation supports both iOS and Android platforms while maintaining the efficiency and security that Tauri is known for.

### Key Features

- **Ultra-Lightweight**: Apps as small as 600KB thanks to Rust's efficiency
- **Cross-Platform**: Single codebase for iOS, Android, and desktop
- **Modern Frontend**: Svelte 5+ with TypeScript for reactive UI development
- **Secure by Default**: Rust-powered backend with sandboxed frontend
- **Native Performance**: Direct system API access through Rust
- **Hot Reload**: Fast development cycle with Vite integration

### Architecture

```
┌─────────────────────────────────────┐
│           Svelte Frontend           │
│        (TypeScript + Vite)          │
├─────────────────────────────────────┤
│          Tauri API Layer            │
│     (Commands & Events Bridge)      │
├─────────────────────────────────────┤
│           Rust Backend              │
│   (Authentication, Notifications,   │
│    File System, HTTP Requests)      │
├─────────────────────────────────────┤
│        Platform Native APIs        │
│      (iOS/Android/Desktop)          │
└─────────────────────────────────────┘
```

## Shared Resources with Tauri Desktop

### Cross-Platform Resource Sharing

The Nexpo Tauri Mobile application is designed to share components, stores, and authentication with Tauri Desktop's Svelte standalone mode, enabling unified cross-platform development:

**Shared Resources Architecture:**
```
┌─────────────────────────────────────────────────────────────────┐
│                    Tauri Desktop (Svelte Mode)                 │
│                        Port 1420                               │
└─────────────────┬───────────────────────────┬───────────────────┘
                  │                           │
                  │    Shared Resources       │
                  │                           │
┌─────────────────▼───────────────────────────▼───────────────────┐
│  Components/    │    Stores/     │    Utils/     │   Auth/      │
│  - AuthPanel    │  - authStore   │  - helpers    │  - CRA       │
│  - UserProfile  │  - settings    │  - validators │  - JWT       │
│  - Settings     │  - theme       │  - formatters │  - tokens    │
│  - Notifications│  - cache       │  - constants  │  - sessions  │
└─────────────────┼───────────────────────────┼───────────────────┘
                  │                           │
                  │                           │
┌─────────────────▼───────────────────────────▼───────────────────┐
│                    Tauri Mobile                                 │
│                   Port 19000                                    │
└─────────────────────────────────────────────────────────────────┘
```

### Shared Component Library

The following Svelte components are shared between mobile and desktop:

- **AuthPanel.svelte**: Unified authentication interface
- **UserProfile.svelte**: User profile management and display
- **SettingsPanel.svelte**: Application settings with persistence
- **NotificationPanel.svelte**: Push notification management

**Import Path in Desktop:**
```typescript
// In Tauri Desktop Svelte mode (apps/tauri/src/App.svelte)
import AuthPanel from '$mobile-components/AuthPanel.svelte';
import UserProfile from '$mobile-components/UserProfile.svelte';
import SettingsPanel from '$mobile-components/SettingsPanel.svelte';
import NotificationPanel from '$mobile-components/NotificationPanel.svelte';
```

### Shared State Management

Svelte stores are shared between platforms for consistent state management:

**Auth Store Sharing:**
```typescript
// In Tauri Desktop (apps/tauri/src/App.svelte)
import { authStore } from '$mobile-stores/authStore';
import { settingsStore } from '$mobile-stores/settingsStore';

// Reactive authentication state across platforms
$: if ($authStore.isAuthenticated) {
  // Handle authenticated state consistently
}
```

**Shared Store Features:**
- **authStore**: Cross-platform authentication state
- **settingsStore**: Synchronized app preferences
- **themeStore**: Unified dark/light mode
- **cacheStore**: Shared data caching strategies

### Central Rank Authority (CRA) Integration

Both mobile and desktop platforms share the same CRA authentication system:

**Unified Authentication Flow:**
1. **Single Sign-On**: Login once, authenticated across all platforms
2. **Token Sharing**: JWT tokens synchronized via secure storage
3. **Session Management**: Consistent session handling
4. **Role-Based Access**: Unified permissions across platforms

**Configuration:**
```bash
# Environment variables for shared auth
TAURI_SHARED_AUTH=true
TAURII_CRA_INTEGRATION=true
TAURI_SHARED_STORES=true
TAURI_SHARED_COMPONENTS=true
```

### Development Workflow

**Cross-Platform Development:**
```bash
# Start mobile development
cd apps/tauri-mobile
pnpm tauri:dev

# Start desktop Svelte mode (shares mobile resources)
cd ../tauri
pnpm desktop  # or pnpm dev:svelte-mode
```

**Resource Synchronization:**
- Changes to shared components automatically reflect in both platforms
- Store updates are synchronized in real-time during development
- Authentication state persists across platform switches
- Settings and preferences remain consistent

### Benefits of Shared Architecture

1. **Code Reuse**: ~80% component reuse between mobile and desktop
2. **Consistent UX**: Unified user interface across platforms
3. **Simplified Maintenance**: Single source of truth for shared logic
4. **Faster Development**: Write once, deploy everywhere approach
5. **Unified Authentication**: Single login for all platforms
6. **Synchronized State**: Consistent app behavior across devices

### Path Aliases Configuration

Tauri Desktop is configured with path aliases to access mobile resources:

```json
// apps/tauri/tsconfig.json
{
  "compilerOptions": {
    "paths": {
      "$mobile": ["../tauri-mobile/src"],
      "$mobile-stores": ["../tauri-mobile/src/stores"],
      "$mobile-components": ["../tauri-mobile/src/components"]
    }
  }
}
```

```javascript
// apps/tauri/vite.config.ts
export default defineConfig({
  resolve: {
    alias: {
      '$mobile': path.resolve(__dirname, '../tauri-mobile/src'),
      '$mobile-stores': path.resolve(__dirname, '../tauri-mobile/src/stores'),
      '$mobile-components': path.resolve(__dirname, '../tauri-mobile/src/components'),
    },
  },
});
```

## Quick Start

### Prerequisites

1. **Rust Toolchain**
   ```bash
   # Install Rust
   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
   source ~/.cargo/env
   ```

2. **Tauri CLI**
   ```bash
   cargo install tauri-cli
   ```

3. **Node.js & pnpm**
   ```bash
   # Already installed in Nexpo monorepo
   pnpm --version
   ```

4. **Mobile Development Tools**
   - **iOS**: Xcode 12+ (macOS only)
   - **Android**: Android Studio with SDK 24+

### Installation

1. Navigate to the Tauri mobile app directory:
   ```bash
   cd apps/tauri-mobile
   ```

2. Install dependencies:
   ```bash
   pnpm install
   ```

3. Initialize mobile targets (first time only):
   ```bash
   pnpm tauri mobile init
   ```

## Development

### Desktop Development

Start the development server for desktop testing:

```bash
# Start Vite dev server + Tauri desktop window
pnpm tauri:dev

# Alternative: Start just the frontend
pnpm dev
```

The app will open in a desktop window on your development machine, allowing you to test all functionality before deploying to mobile devices.

### Mobile Development

#### iOS Development (macOS only)

```bash
# Start iOS simulator
pnpm tauri:ios

# Build for iOS device
pnpm tauri:ios:build
```

#### Android Development

```bash
# Start Android emulator
pnpm tauri:android

# Build APK for Android
pnpm tauri:android:build
```

### Hot Reload

The development setup supports hot reload for both desktop and mobile:

- **Frontend changes**: Automatically reflected via Vite
- **Rust backend changes**: Require app restart (automatic with `tauri dev`)
- **Configuration changes**: Require manual restart

## Project Structure

```
apps/tauri-mobile/
├── src/                          # Svelte frontend source
│   ├── components/               # Svelte components
│   │   ├── AuthPanel.svelte     # Authentication UI
│   │   ├── UserProfile.svelte   # User profile management
│   │   ├── SettingsPanel.svelte # App settings
│   │   └── NotificationPanel.svelte # Push notifications
│   ├── stores/                   # Svelte stores
│   │   ├── authStore.ts         # Authentication state
│   │   └── settingsStore.ts     # App settings state
│   ├── App.svelte               # Main application component
│   ├── main.ts                  # Frontend entry point
│   └── app.css                  # Global styles
├── src-tauri/                   # Rust backend source
│   ├── src/
│   │   └── main.rs              # Rust application entry
│   ├── Cargo.toml               # Rust dependencies
│   ├── tauri.conf.json          # Tauri configuration
│   └── build.rs                 # Build script
├── package.json                 # Frontend dependencies
├── vite.config.ts               # Vite configuration
├── tsconfig.json                # TypeScript configuration
└── index.html                   # HTML template
```

## Core Features Implementation

### Authentication System

The app includes a complete authentication system with state management:

```typescript
// Frontend: authStore.ts
import { writable } from 'svelte/store';

export interface User {
  id: string;
  name: string;
  email: string;
  avatar_url?: string;
}

const authStore = writable({
  user: null,
  isAuthenticated: false,
  isLoading: false
});
```

```rust
// Backend: main.rs
#[tauri::command]
async fn authenticate_user(
    email: String,
    password: String,
    app: AppHandle,
) -> Result<ApiResponse<User>, String> {
    // Authentication logic
    let user = User {
        id: Uuid::new_v4().to_string(),
        name: email.split('@').next().unwrap_or("User").to_string(),
        email: email.clone(),
        avatar_url: None,
    };
    
    // Emit authentication event
    app.emit("auth-changed", &user)?;
    
    Ok(ApiResponse {
        success: true,
        data: Some(user),
        error: None,
    })
}
```

### Push Notifications

Cross-platform notification system:

```rust
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
            .map_err(|e| format!("Failed to send notification: {}", e))?
    }
    
    Ok(ApiResponse {
        success: true,
        data: Some(()),
        error: None,
    })
}
```

### Settings Management

Persistent app settings with real-time sync:

```svelte
<!-- SettingsPanel.svelte -->
<script lang="ts">
  import { invoke } from '@tauri-apps/api/core';
  import { settingsStore } from '../stores/settingsStore';
  
  const handleSettingChange = async (key: string, value: any) => {
    settingsStore.updateSetting(key, value);
    await invoke('update_setting', { key, value });
  };
</script>

<select bind:value={settings.theme} on:change={() => handleSettingChange('theme', settings.theme)}>
  <option value="light">Light</option>
  <option value="dark">Dark</option>
  <option value="auto">Auto (System)</option>
</select>
```

## Building for Production

### Desktop Build

```bash
# Build for current platform
pnpm tauri:build

# Build for specific targets
cargo tauri build --target x86_64-apple-darwin    # macOS Intel
cargo tauri build --target aarch64-apple-darwin   # macOS ARM
cargo tauri build --target x86_64-pc-windows-msvc # Windows
cargo tauri build --target x86_64-unknown-linux-gnu # Linux
```

### Mobile Build

#### iOS Production Build

```bash
# Build for App Store
pnpm tauri:ios:build --release

# Build for device testing
cargo tauri ios build --open
```

#### Android Production Build

```bash
# Build APK for distribution
pnpm tauri:android:build --release

# Build AAB for Google Play Store
cargo tauri android build --target aab
```

## Configuration

### Tauri Configuration

The `src-tauri/tauri.conf.json` file controls app behavior:

```json
{
  "productName": "Nexpo Mobile",
  "version": "1.0.0",
  "identifier": "com.nexpo.mobile",
  "build": {
    "frontendDist": "../dist",
    "devUrl": "http://localhost:5173"
  },
  "mobile": {
    "android": {
      "packageName": "com.nexpo.mobile",
      "permissions": [
        "android.permission.INTERNET",
        "android.permission.WRITE_EXTERNAL_STORAGE"
      ]
    },
    "ios": {
      "bundleIdentifier": "com.nexpo.mobile",
      "permissions": [
        "NSCameraUsageDescription",
        "NSMicrophoneUsageDescription"
      ]
    }
  }
}
```

### Permissions

#### Android Permissions

Common permissions needed for mobile functionality:

```xml
<!-- Automatically added to AndroidManifest.xml -->
<uses-permission android:name="android.permission.INTERNET" />
<uses-permission android:name="android.permission.WRITE_EXTERNAL_STORAGE" />
<uses-permission android:name="android.permission.CAMERA" />
<uses-permission android:name="android.permission.RECORD_AUDIO" />
```

#### iOS Permissions

Permissions are defined in `tauri.conf.json` and automatically added to Info.plist:

```json
"permissions": [
  "NSCameraUsageDescription": "This app needs camera access to take photos",
  "NSMicrophoneUsageDescription": "This app needs microphone access for voice features",
  "NSLocationWhenInUseUsageDescription": "This app needs location access for location-based features"
]
```

## Deployment

### iOS App Store Deployment

1. **Code Signing Setup**:
   - Configure Apple Developer account
   - Create App Store Connect app entry
   - Set up provisioning profiles

2. **Build and Archive**:
   ```bash
   cargo tauri ios build --release
   ```

3. **Upload to App Store**:
   - Open Xcode
   - Archive and upload via Organizer
   - Submit for review

### Google Play Store Deployment

1. **App Bundle Creation**:
   ```bash
   cargo tauri android build --target aab --release
   ```

2. **Play Console Upload**:
   - Create app entry in Play Console
   - Upload AAB file
   - Configure store listing
   - Submit for review

## Performance Optimization

### Bundle Size Optimization

1. **Rust Optimizations**:
   ```toml
   # Cargo.toml
   [profile.release]
   opt-level = "z"     # Optimize for size
   lto = true          # Link-time optimization
   codegen-units = 1   # Better optimization
   panic = "abort"     # Smaller binary
   strip = true        # Remove debug symbols
   ```

2. **Frontend Optimizations**:
   ```typescript
   // vite.config.ts
   export default defineConfig({
     build: {
       minify: 'terser',
       rollupOptions: {
         output: {
           manualChunks: {
             vendor: ['svelte']
           }
         }
       }
     }
   });
   ```

### Runtime Performance

- **Async Operations**: All Tauri commands are async by default
- **Event-Driven**: Use events for real-time updates
- **Memory Management**: Rust handles memory automatically
- **Native APIs**: Direct system API access for performance

## Troubleshooting

### Common Issues

1. **Tauri CLI Not Found**:
   ```bash
   cargo install tauri-cli
   ```

2. **Mobile Build Fails**:
   ```bash
   # Reinstall mobile targets
   rustup target add aarch64-apple-ios x86_64-apple-ios aarch64-apple-ios-sim
   rustup target add aarch64-linux-android armv7-linux-androideabi i686-linux-android x86_64-linux-android
   ```

3. **Android Studio Issues**:
   - Ensure Android SDK 24+ is installed
   - Set ANDROID_HOME environment variable
   - Accept all SDK licenses

4. **iOS Build Issues**:
   - Xcode 12+ required
   - Valid Apple Developer account
   - Proper code signing setup

### Debug Mode

Enable debug logging for development:

```bash
# Set environment variable
export RUST_LOG=debug

# Start with debug output
pnpm tauri:dev
```

## Security Considerations

### Secure Communication

- All frontend-backend communication is type-safe
- Commands require explicit declaration in `main.rs`
- No eval() or arbitrary code execution
- CSP (Content Security Policy) enforced

### Data Protection

- Sensitive data stored in platform keychain
- All HTTP requests go through Rust backend
- No direct DOM manipulation from backend
- Automatic input sanitization

## Next Steps

1. **Customize the UI**: Modify Svelte components to match your brand
2. **Add Features**: Implement additional Tauri commands as needed
3. **Integration**: Connect to your backend APIs
4. **Testing**: Set up automated testing pipeline
5. **Distribution**: Prepare for app store deployment

## Resources

- [Tauri Documentation](https://tauri.app/)
- [Svelte Documentation](https://svelte.dev/)
- [Tauri Mobile Guide](https://tauri.app/v1/guides/building/mobile)
- [Nexpo Monorepo Structure](../../README.md)

---

*This Tauri mobile implementation provides a solid foundation for cross-platform mobile development with the performance of Rust and the developer experience of Svelte.*