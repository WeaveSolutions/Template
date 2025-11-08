# BI Dashboard Implementation Checklist - Nexpo Desktop (Tauri + Next.js)

## Platform: Tauri Desktop with Next.js Frontend
**Target:** Windows, macOS, Linux Desktop Applications
**Framework:** Tauri + Next.js + React
**Languages:** Rust (Backend) + TypeScript (Frontend)

---

## Implementation Notes

**Nexpo Desktop** uses the same Next.js codebase as Nexpo Web with Tauri providing the native desktop wrapper and Rust backend capabilities.

---

## 1. Prerequisites

### Dependencies (Same as Nexpo Web)
- [ ] All Next.js dependencies from BI_Nexpo_Web.md
- [ ] Tauri CLI: `npm install -D @tauri-apps/cli`
- [ ] Tauri API: `npm install @tauri-apps/api`

### Rust Dependencies (Cargo.toml)
```toml
[dependencies]
tauri = { version = "1.5", features = ["shell-open"] }
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
reqwest = { version = "0.11", features = ["json", "blocking"] }
tokio = { version = "1", features = ["full"] }
```

---

## 2. Tauri Configuration

### Update tauri.conf.json
- [ ] Configure window settings for dashboard
- [ ] Set minimum window size (1280x800)
- [ ] Enable fullscreen capability
- [ ] Configure menu bar items
- [ ] Set app name and identifier

```json
{
  "tauri": {
    "windows": [
      {
        "title": "Business Intelligence Dashboard",
        "width": 1440,
        "height": 900,
        "minWidth": 1280,
        "minHeight": 800,
        "resizable": true,
        "fullscreen": false,
        "center": true
      }
    ]
  }
}
```

---

## 3. Desktop-Specific Features

### Native Menu Integration
- [ ] Create custom menu bar
- [ ] Add "View Dashboard" menu item (admin-only)
- [ ] Add "Export Report" menu option
- [ ] Add "Refresh Data" shortcut (Cmd/Ctrl + R)
- [ ] Add "Settings" menu

### System Tray Integration
- [ ] Add system tray icon
- [ ] Show quick metrics in tray tooltip
- [ ] Add "Show Dashboard" tray menu item
- [ ] Add notification indicators

### Keyboard Shortcuts
- [ ] Cmd/Ctrl + D: Open dashboard
- [ ] Cmd/Ctrl + R: Refresh data
- [ ] Cmd/Ctrl + E: Export current view
- [ ] Cmd/Ctrl + ,: Open settings
- [ ] F11: Toggle fullscreen

---

## 4. Rust Backend Commands

### Tauri Commands for BI Data
- [ ] `fetch_bi_metrics` - Get current KPIs
- [ ] `export_to_csv` - Export dashboard data
- [ ] `check_admin_role` - Verify user permissions
- [ ] `open_external_link` - Open Looker Studio in browser
- [ ] `save_dashboard_config` - Store user preferences

**Example: src-tauri/src/main.rs**
```rust
#[tauri::command]
async fn fetch_bi_metrics(auth_token: String) -> Result<BiMetrics, String> {
    // Call Firebase/Supabase API
    // Return structured data
}

#[tauri::command]
async fn export_to_csv(data: String, filename: String) -> Result<String, String> {
    // Save CSV to user's Documents folder
    // Return file path
}
```

---

## 5. Desktop-Optimized UI

### Performance Considerations
- [ ] Use native window controls
- [ ] Optimize for different screen DPIs
- [ ] Support multiple monitors
- [ ] Handle sleep/wake events
- [ ] Persist window position/size

### Desktop-Specific Components
- [ ] Native file picker for exports
- [ ] Native notifications for alerts
- [ ] Desktop-style context menus
- [ ] Draggable window regions
- [ ] Native title bar (optional)

---

## 6. Data Storage

### Local Caching
- [ ] Use Tauri's Store API for preferences
- [ ] Cache dashboard data locally
- [ ] Store authentication tokens securely
- [ ] Persist filter selections
- [ ] Save custom dashboard layouts

### Offline Mode
- [ ] Detect network status
- [ ] Show cached data when offline
- [ ] Queue actions for sync when online
- [ ] Display offline indicator
- [ ] Auto-sync on reconnection

---

## 7. Security

### Desktop Security Features
- [ ] Use Tauri's secure storage for tokens
- [ ] Implement auto-lock after inactivity
- [ ] Clear sensitive data on logout
- [ ] Validate all inter-process communication
- [ ] Sandbox renderer process

### Code Signing
- [ ] macOS: Sign with Developer ID
- [ ] Windows: Sign with certificate
- [ ] Linux: Create AppImage/deb/rpm

---

## 8. Looker Studio Integration

### Desktop Embedding Options

**Option 1: Embedded WebView (Recommended)**
- [ ] Embed Looker Studio iframe in Next.js pages
- [ ] Same implementation as Nexpo Web
- [ ] Full interactive dashboard in app

**Option 2: External Browser**
- [ ] Add "Open in Browser" button
- [ ] Use Tauri shell API to open Looker Studio
- [ ] Deep link back to app from browser

```typescript
import { open } from '@tauri-apps/api/shell';

async function openLookerInBrowser() {
  await open('https://lookerstudio.google.com/reporting/YOUR_REPORT_ID');
}
```

---

## 9. Native Features

### File System Access
- [ ] Export dashboard to PDF
- [ ] Export charts as PNG/SVG
- [ ] Save reports to custom location
- [ ] Import custom data files

### Notifications
- [ ] System notifications for alerts
- [ ] Badge count on dock/taskbar icon
- [ ] Sound alerts for critical metrics
- [ ] Rich notification actions

### Auto-Update
- [ ] Configure Tauri updater
- [ ] Check for updates on startup
- [ ] Download and install updates
- [ ] Notify user of new versions
- [ ] Auto-restart after update

---

## 10. Platform-Specific Builds

### macOS
- [ ] Build for Apple Silicon (arm64)
- [ ] Build for Intel (x86_64)
- [ ] Universal binary (recommended)
- [ ] DMG installer
- [ ] Notarize app

### Windows
- [ ] Build for x64
- [ ] NSIS installer
- [ ] MSI installer (optional)
- [ ] Add to Windows registry
- [ ] Sign executable

### Linux
- [ ] AppImage
- [ ] Debian package (.deb)
- [ ] RPM package
- [ ] Snap package (optional)
- [ ] Flatpak (optional)

---

## 11. Testing

### Desktop-Specific Tests
- [ ] Test on all target OS versions
- [ ] Test multi-monitor setups
- [ ] Test different screen resolutions
- [ ] Test with different system themes (dark/light)
- [ ] Test offline mode
- [ ] Test auto-update flow
- [ ] Test system tray functionality
- [ ] Test keyboard shortcuts

---

## 12. Distribution

### Release Checklist
- [ ] Build for all platforms
- [ ] Sign all builds
- [ ] Create release notes
- [ ] Upload to GitHub Releases
- [ ] Configure auto-update endpoints
- [ ] Update download page
- [ ] Test installation on clean systems

### Update Strategy
- [ ] Configure update check interval
- [ ] Set minimum supported version
- [ ] Plan rollback strategy
- [ ] Monitor update adoption

---

## Implementation Priority

### Phase 1: Core Desktop App (Week 1)
1. Tauri configuration
2. Rust backend commands
3. Port Next.js dashboard
4. Test basic functionality

### Phase 2: Desktop Features (Week 2)
1. System tray integration
2. Keyboard shortcuts
3. Local data caching
4. Native notifications

### Phase 3: Platform Builds (Week 3)
1. macOS build and signing
2. Windows build and signing
3. Linux builds
4. Test on all platforms

### Phase 4: Distribution (Week 4)
1. Auto-update setup
2. Create installers
3. Release to users
4. Monitor feedback

---

## Success Criteria

- [ ] Desktop app launches on all platforms
- [ ] Same functionality as Nexpo Web
- [ ] Native OS integration works
- [ ] Auto-update functional
- [ ] <50MB app size
- [ ] <500MB RAM usage
- [ ] <2 second startup time
- [ ] All keyboard shortcuts work
- [ ] System tray functional
- [ ] Offline mode works
