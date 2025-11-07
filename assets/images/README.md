# Weave Logo Assets

## WeaveLogo.png Placement Instructions

The Weave logo (`WeaveLogo.png`) needs to be copied to multiple locations for use across all platforms.

### Required Locations:

1. **Shared Assets** (Reference copy)
   ```
   assets/images/WeaveLogo.png
   ```

2. **Taurte Web** (Svelte - Port 5173)
   ```
   apps/taurte/svelteWeb/public/WeaveLogo.png
   ```

3. **Nexpo Web** (Next.js - Port 3000)
   ```
   apps/nexpo/nextWeb/public/WeaveLogo.png
   ```

4. **Taurte Desktop** (Tauri Desktop - Port 1420)
   ```
   apps/desktop/public/WeaveLogo.png
   ```

5. **Taurte Mobile** (Tauri Mobile - Port 19000)
   ```
   apps/taurte/tauriMobile/public/WeaveLogo.png
   ```

### Quick Copy Commands (PowerShell):

```powershell
# From the root of the Weave repository

# Copy to all platforms
Copy-Item "assets/images/WeaveLogo.png" "apps/taurte/svelteWeb/public/WeaveLogo.png"
Copy-Item "assets/images/WeaveLogo.png" "apps/nexpo/nextWeb/public/WeaveLogo.png"
Copy-Item "assets/images/WeaveLogo.png" "apps/desktop/public/WeaveLogo.png"
Copy-Item "assets/images/WeaveLogo.png" "apps/taurte/tauriMobile/public/WeaveLogo.png"
```

### Logo Specifications:

- **Format**: PNG with transparency
- **Dimensions**: Square (recommended 512x512px or higher)
- **Usage**: Navigation bars, headers, and branding across all platforms
- **Colors**: Vibrant gradient (purple, pink, orange, yellow, cyan)

### Updated Components:

All components have been updated to reference `/WeaveLogo.png`:

- ✅ `apps/taurte/svelteWeb/src/components/Navigation.svelte`
- ✅ `apps/nexpo/nextWeb/components/Navigation.tsx`
- ✅ `apps/desktop/src/App.svelte`
- ✅ `apps/taurte/tauriMobile/src/App.svelte`

### Display Sizes:

- **Web Navigation**: 40px × 40px
- **Desktop Navigation**: 40px × 40px
- **Mobile Header**: 36px × 36px

All logos use `object-fit: contain` to maintain aspect ratio.
