# BI Dashboard Implementation Checklist - Taurte Desktop (Tauri + Svelte)

## Platform: Tauri Desktop with Svelte Frontend
**Target:** Windows, macOS, Linux Desktop Applications
**Framework:** Tauri + Svelte
**Languages:** Rust (Backend) + TypeScript/Svelte (Frontend)

---

## Implementation Notes

**Taurte Desktop** leverages Svelte's lightweight reactivity with Tauri's native Rust backend for optimal desktop performance.

---

## 1. Prerequisites

### Dependencies
- [ ] All Taurte Web dependencies
- [ ] Tauri CLI: `npm install -D @tauri-apps/cli`
- [ ] Tauri API: `npm install @tauri-apps/api`
- [ ] Rust toolchain installed

### Rust Dependencies (Cargo.toml)
```toml
[dependencies]
tauri = { version = "1.5", features = ["shell-open", "dialog-all", "fs-all"] }
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
reqwest = { version = "0.11", features = ["json", "blocking"] }
tokio = { version = "1", features = ["full"] }
csv = "1.2" # For CSV export
```

---

## 2. Tauri Configuration

### tauri.conf.json Setup
```json
{
  "build": {
    "beforeDevCommand": "npm run dev",
    "beforeBuildCommand": "npm run build",
    "devPath": "http://localhost:5173",
    "distDir": "../build"
  },
  "package": {
    "productName": "BI Dashboard",
    "version": "1.0.0"
  },
  "tauri": {
    "allowlist": {
      "all": false,
      "shell": {
        "open": true
      },
      "dialog": {
        "all": true
      },
      "fs": {
        "scope": ["$APP/**", "$DOCUMENT/**"]
      }
    },
    "windows": [
      {
        "title": "Business Intelligence Dashboard",
        "width": 1440,
        "height": 900,
        "minWidth": 1280,
        "minHeight": 800,
        "resizable": true,
        "center": true
      }
    ],
    "security": {
      "csp": "default-src 'self'; connect-src 'self' https://*.supabase.co https://*.firebaseapp.com https://lookerstudio.google.com"
    }
  }
}
```

---

## 3. Rust Backend Commands

### Core Tauri Commands
- [ ] `fetch_bi_metrics` - Get KPI data from database
- [ ] `export_dashboard_csv` - Export data to CSV
- [ ] `export_dashboard_pdf` - Export dashboard as PDF
- [ ] `save_preferences` - Store user preferences
- [ ] `load_preferences` - Load user preferences
- [ ] `check_for_updates` - Check for app updates

**Example Commands (src-tauri/src/main.rs):**
```rust
use tauri::Manager;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
struct BiMetrics {
    mrr: f64,
    arr: f64,
    total_users: i32,
    active_users: i32,
}

#[tauri::command]
async fn fetch_bi_metrics(
    api_url: String,
    auth_token: String
) -> Result<BiMetrics, String> {
    let client = reqwest::Client::new();
    let response = client
        .get(api_url)
        .header("Authorization", format!("Bearer {}", auth_token))
        .send()
        .await
        .map_err(|e| e.to_string())?;
    
    response.json::<BiMetrics>()
        .await
        .map_err(|e| e.to_string())
}

#[tauri::command]
async fn export_dashboard_csv(
    data: String,
    filename: String
) -> Result<String, String> {
    use std::fs::File;
    use std::io::Write;
    
    let documents_dir = tauri::api::path::document_dir()
        .ok_or("Could not find documents directory")?;
    
    let file_path = documents_dir.join(&filename);
    let mut file = File::create(&file_path)
        .map_err(|e| e.to_string())?;
    
    file.write_all(data.as_bytes())
        .map_err(|e| e.to_string())?;
    
    Ok(file_path.to_string_lossy().to_string())
}

fn main() {
    tauri::Builder::default()
        .invoke_handler(tauri::generate_handler![
            fetch_bi_metrics,
            export_dashboard_csv
        ])
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
```

---

## 4. Svelte Frontend Integration

### Call Tauri Commands from Svelte
```svelte
<script lang="ts">
  import { invoke } from '@tauri-apps/api/tauri';
  import { writable } from 'svelte/store';
  
  let metrics = writable(null);
  let loading = writable(false);
  
  async function fetchMetrics() {
    loading.set(true);
    try {
      const data = await invoke('fetch_bi_metrics', {
        apiUrl: 'https://api.example.com/metrics',
        authToken: 'your-token'
      });
      metrics.set(data);
    } catch (error) {
      console.error('Failed to fetch metrics:', error);
    } finally {
      loading.set(false);
    }
  }
  
  async function exportToCsv() {
    const csvData = convertToCsv($metrics);
    const filePath = await invoke('export_dashboard_csv', {
      data: csvData,
      filename: 'bi-dashboard-export.csv'
    });
    alert(`Exported to: ${filePath}`);
  }
</script>

<button on:click={fetchMetrics}>Refresh Data</button>
<button on:click={exportToCsv}>Export CSV</button>
```

---

## 5. Desktop-Specific Features

### Native Menu Bar
- [ ] Create custom menu using Tauri's menu API
- [ ] Add "File" menu (Export, Preferences, Quit)
- [ ] Add "View" menu (Refresh, Fullscreen)
- [ ] Add "Help" menu (About, Check for Updates)

**Menu Implementation:**
```rust
use tauri::{CustomMenuItem, Menu, MenuItem, Submenu};

let quit = CustomMenuItem::new("quit".to_string(), "Quit");
let export = CustomMenuItem::new("export".to_string(), "Export Dashboard");
let refresh = CustomMenuItem::new("refresh".to_string(), "Refresh Data");

let file_menu = Submenu::new("File", Menu::new()
    .add_item(export)
    .add_native_item(MenuItem::Separator)
    .add_item(quit)
);

let view_menu = Submenu::new("View", Menu::new()
    .add_item(refresh)
);

let menu = Menu::new()
    .add_submenu(file_menu)
    .add_submenu(view_menu);

tauri::Builder::default()
    .menu(menu)
    .on_menu_event(|event| {
        match event.menu_item_id() {
            "quit" => {
                std::process::exit(0);
            }
            "refresh" => {
                // Emit event to refresh
                event.window().emit("refresh-data", {}).unwrap();
            }
            _ => {}
        }
    })
    .run(tauri::generate_context!())
    .expect("error while running tauri application");
```

### System Tray
- [ ] Add system tray icon
- [ ] Show quick metrics in tray menu
- [ ] Add "Show Dashboard" option
- [ ] Add "Quit" option

### Keyboard Shortcuts
- [ ] Cmd/Ctrl + R: Refresh data
- [ ] Cmd/Ctrl + E: Export dashboard
- [ ] Cmd/Ctrl + ,: Open preferences
- [ ] Cmd/Ctrl + Q: Quit app
- [ ] F11: Toggle fullscreen

---

## 6. Local Data Storage

### Tauri Store Plugin
```bash
cargo add tauri-plugin-store
```

**Usage in Svelte:**
```typescript
import { Store } from 'tauri-plugin-store-api';

const store = new Store('.bi-dashboard.dat');

// Save preferences
await store.set('theme', 'dark');
await store.set('defaultDateRange', '30d');
await store.save();

// Load preferences
const theme = await store.get('theme');
```

### Cache Management
- [ ] Cache last fetched KPI data
- [ ] Store user preferences
- [ ] Save dashboard layouts
- [ ] Persist filter selections
- [ ] Auto-clear old cache

---

## 7. Looker Studio Integration

### Desktop Embedding
```svelte
<script lang="ts">
  import { open } from '@tauri-apps/api/shell';
  
  const lookerUrl = 'https://lookerstudio.google.com/reporting/YOUR_ID';
  let embedInApp = true;
  
  async function openInBrowser() {
    await open(lookerUrl);
  }
</script>

{#if embedInApp}
  <iframe
    src={lookerUrl}
    width="100%"
    height="100%"
    style="border: 0;"
    allowfullscreen
    title="Looker Studio Dashboard"
  />
  <button on:click={openInBrowser}>Open in Browser</button>
{:else}
  <button on:click={openInBrowser}>
    View Full Dashboard in Browser
  </button>
{/if}
```

---

## 8. File System Operations

### Export Functionality
- [ ] Export dashboard as PDF
- [ ] Export data as CSV
- [ ] Export charts as PNG/SVG
- [ ] Save reports to custom location
- [ ] Use native file picker

**File Picker Example:**
```svelte
<script lang="ts">
  import { save } from '@tauri-apps/api/dialog';
  import { writeTextFile } from '@tauri-apps/api/fs';
  
  async function saveReport(data: string) {
    const filePath = await save({
      filters: [{
        name: 'CSV',
        extensions: ['csv']
      }]
    });
    
    if (filePath) {
      await writeTextFile(filePath, data);
    }
  }
</script>
```

---

## 9. Native Notifications

### Desktop Notifications
```svelte
<script lang="ts">
  import { isPermissionGranted, requestPermission, sendNotification } from '@tauri-apps/api/notification';
  
  async function showAlert(title: string, body: string) {
    let permissionGranted = await isPermissionGranted();
    
    if (!permissionGranted) {
      const permission = await requestPermission();
      permissionGranted = permission === 'granted';
    }
    
    if (permissionGranted) {
      sendNotification({ title, body });
    }
  }
  
  // Alert when MRR drops
  $: if ($metrics.mrr < lastMrr) {
    showAlert('MRR Alert', `MRR decreased to $${$metrics.mrr}`);
  }
</script>
```

---

## 10. Auto-Update

### Configure Tauri Updater
```json
// tauri.conf.json
{
  "tauri": {
    "updater": {
      "active": true,
      "endpoints": [
        "https://releases.yourdomain.com/{{target}}/{{current_version}}"
      ],
      "dialog": true,
      "pubkey": "YOUR_PUBLIC_KEY"
    }
  }
}
```

### Check for Updates
```svelte
<script lang="ts">
  import { checkUpdate, installUpdate } from '@tauri-apps/api/updater';
  import { relaunch } from '@tauri-apps/api/process';
  
  async function checkForUpdates() {
    try {
      const { shouldUpdate, manifest } = await checkUpdate();
      
      if (shouldUpdate) {
        const yes = confirm(
          `Update available: ${manifest?.version}. Install now?`
        );
        
        if (yes) {
          await installUpdate();
          await relaunch();
        }
      }
    } catch (error) {
      console.error('Update check failed:', error);
    }
  }
</script>
```

---

## 11. Platform-Specific Builds

### macOS
```bash
cargo tauri build --target x86_64-apple-darwin      # Intel
cargo tauri build --target aarch64-apple-darwin     # Apple Silicon
cargo tauri build --target universal-apple-darwin   # Universal
```

- [ ] Sign with Developer ID certificate
- [ ] Notarize app with Apple
- [ ] Create DMG installer
- [ ] Test on both Intel and Apple Silicon

### Windows
```bash
cargo tauri build --target x86_64-pc-windows-msvc
```

- [ ] Sign with code signing certificate
- [ ] Create MSI installer (optional)
- [ ] Create NSIS installer
- [ ] Test on Windows 10 and 11

### Linux
```bash
cargo tauri build --target x86_64-unknown-linux-gnu
```

- [ ] Create AppImage
- [ ] Create .deb package (Debian/Ubuntu)
- [ ] Create .rpm package (Fedora/RHEL)
- [ ] Test on major distributions

---

## 12. Testing

### Desktop Testing
- [ ] Test on all target operating systems
- [ ] Test window resizing and fullscreen
- [ ] Test system tray functionality
- [ ] Test menu bar actions
- [ ] Test keyboard shortcuts
- [ ] Test with multiple monitors
- [ ] Test offline mode
- [ ] Test auto-update flow
- [ ] Test file exports
- [ ] Test notifications

### Performance Testing
- [ ] Monitor memory usage
- [ ] Monitor CPU usage
- [ ] Test with large datasets
- [ ] Test startup time
- [ ] Test responsiveness

---

## 13. Distribution

### Release Process
- [ ] Build for all platforms
- [ ] Sign all builds
- [ ] Generate update manifests
- [ ] Create GitHub Release
- [ ] Upload platform-specific installers
- [ ] Update download links
- [ ] Announce release

### Update Server Setup
- [ ] Configure update endpoints
- [ ] Host platform-specific manifests
- [ ] Generate update signatures
- [ ] Test update flow

---

## Implementation Priority

### Phase 1: Core App (Week 1)
1. Tauri setup and configuration
2. Rust backend commands
3. Svelte UI ported from Taurte Web
4. Database integration

### Phase 2: Desktop Features (Week 2)
1. Menu bar integration
2. System tray
3. Keyboard shortcuts
4. Local storage
5. Native notifications

### Phase 3: Platform Builds (Week 3)
1. macOS builds and signing
2. Windows builds and signing
3. Linux builds
4. Test on all platforms

### Phase 4: Distribution (Week 4)
1. Auto-update setup
2. Create installers
3. Setup update server
4. Release to users

---

## Success Criteria

- [ ] Desktop app launches on macOS, Windows, Linux
- [ ] All BI features functional
- [ ] Native OS integration works
- [ ] Auto-update functional
- [ ] <40MB app size (smaller than Electron)
- [ ] <300MB RAM usage
- [ ] <1 second startup time
- [ ] Menu bar and shortcuts work
- [ ] System tray functional
- [ ] Exports work correctly
- [ ] Notifications display properly
- [ ] Smooth 60fps UI
