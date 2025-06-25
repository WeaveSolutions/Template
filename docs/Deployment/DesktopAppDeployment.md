# Electron App Deployment Guide

## Overview

The Nexpo Electron app is configured for automated cross-platform deployment using GitHub Actions and Electron Forge. This guide covers local development, building, and deployment processes.

## Local Development

### Prerequisites
- Node.js 20+
- pnpm 8+
- Platform-specific build tools:
  - **Windows**: Visual Studio Build Tools or Visual Studio Community
  - **macOS**: Xcode Command Line Tools
  - **Linux**: build-essential, rpm, fakeroot, dpkg

### Quick Start

1. **Using Development Scripts**:
   ```bash
   # Windows PowerShell
   .\apps\electron\scripts\dev.ps1
   
   # macOS/Linux
   ./apps/electron/scripts/dev.sh
   ```

2. **Manual Development**:
   ```bash
   # From monorepo root
   pnpm install
   pnpm --filter "@nexpo/*" build
   cd apps/electron
   pnpm start
   ```

### Environment Configuration

Ensure your `.env` file includes:
```env
ENABLE_ELECTRON_PLATFORM=true
```

## Building

### Local Build

Build for your current platform:
```bash
cd apps/electron
pnpm run make
```

Build for specific platforms:
```bash
# Windows (from Windows)
pnpm run make -- --platform=win32

# macOS (from macOS)
pnpm run make -- --platform=darwin

# Linux (from Linux)
pnpm run make -- --platform=linux
```

### Build Output

Built applications are located in:
- `apps/electron/out/make/` - Platform-specific installers
- `apps/electron/out/` - Unpacked application

## Automated Deployment

### GitHub Actions Workflow

The deployment workflow (`.github/workflows/deploy-electron.yml`) automatically builds and publishes releases when:

1. **Tag Push**: Creating a version tag (e.g., `v1.0.0`)
   ```bash
   git tag v1.0.0
   git push origin v1.0.0
   ```

2. **Manual Trigger**: Via GitHub Actions UI
   - Navigate to Actions → Deploy Electron App
   - Click "Run workflow"
   - Select platform(s) to build

### Platform-Specific Artifacts

Each platform produces different installer formats:

- **Windows**:
  - `.exe` - Squirrel installer
  - `.nupkg` - Update packages
  - `RELEASES` - Update manifest

- **macOS**:
  - `.dmg` - Disk image installer
  - `.zip` - Archive for direct distribution

- **Linux**:
  - `.deb` - Debian/Ubuntu installer
  - `.rpm` - Red Hat/Fedora installer
  - `.AppImage` - Universal Linux package

## Code Signing (Optional)

### macOS Code Signing

1. **Generate Certificates**:
   - Developer ID Application certificate from Apple Developer Portal
   - Export as `.p12` file

2. **Add GitHub Secrets**:
   - `MACOS_CERTIFICATE`: Base64-encoded `.p12` file
   - `MACOS_CERTIFICATE_PWD`: Certificate password
   - `APPLE_ID`: Your Apple ID
   - `APPLE_ID_PASSWORD`: App-specific password

3. **Enable in Workflow**:
   Uncomment the code signing sections in `deploy-electron.yml`

### Windows Code Signing

1. **Obtain Certificate**:
   - Code signing certificate from trusted CA
   - Convert to `.pfx` format

2. **Add GitHub Secrets**:
   - `WINDOWS_CERTIFICATE`: Base64-encoded `.pfx` file
   - `WINDOWS_CERTIFICATE_PWD`: Certificate password

3. **Update Forge Config**:
   Add Windows signing configuration to `forge.config.ts`

## Auto-Updates

The Electron app includes auto-update functionality:

1. **Configuration**:
   - Updates are checked on app startup
   - GitHub Releases serve as the update source

2. **Update Process**:
   - App checks for updates against GitHub Releases
   - Downloads updates in background
   - Prompts user to restart when ready

3. **Version Management**:
   - Update `package.json` version before release
   - Use semantic versioning (MAJOR.MINOR.PATCH)

## Deployment Checklist

Before deploying a new version:

- [ ] Update version in `apps/electron/package.json`
- [ ] Test on all target platforms locally
- [ ] Update changelog/release notes
- [ ] Ensure all shared packages are built
- [ ] Verify environment configurations
- [ ] Create and push version tag
- [ ] Monitor GitHub Actions workflow
- [ ] Test auto-update functionality
- [ ] Verify published artifacts

## Troubleshooting

### Common Issues

1. **Build Failures**:
   - Ensure all native dependencies are rebuilt: `pnpm electron-rebuild`
   - Clear webpack cache: `rm -rf .webpack`
   - Verify platform-specific build tools are installed

2. **Missing Shared Packages**:
   - Build shared packages first: `pnpm --filter "@nexpo/*" build`
   - Check workspace dependencies in `package.json`

3. **Auto-Update Issues**:
   - Verify GitHub token permissions
   - Check release asset URLs
   - Ensure version numbers follow semver

### Debug Mode

Run Electron with debug output:
```bash
# Windows
set DEBUG=electron-forge:* && pnpm start

# macOS/Linux
DEBUG=electron-forge:* pnpm start
```

## Advanced Configuration

### Custom Installers

Modify `forge.config.ts` to customize installer behavior:

```typescript
// Example: Custom Windows installer
{
  name: '@electron-forge/maker-squirrel',
  config: {
    name: 'NexpoDesktop',
    authors: 'Nexpo Team',
    exe: 'nexpo.exe',
    setupIcon: './src/assets/icon.ico',
    loadingGif: './src/assets/loading.gif'
  }
}
```

### Platform-Specific Features

Use platform detection in your app:

```typescript
// src/main/index.ts
import { platform } from 'os';

if (platform() === 'darwin') {
  // macOS-specific code
} else if (platform() === 'win32') {
  // Windows-specific code
} else {
  // Linux-specific code
}
```

### Environment-Specific Builds

Create different builds for different environments:

```json
// package.json scripts
{
  "make:dev": "electron-forge make --config forge.dev.config.ts",
  "make:prod": "electron-forge make --config forge.prod.config.ts"
}
```

## Security Considerations

1. **Context Isolation**: Always enabled in renderer processes
2. **Node Integration**: Disabled by default
3. **Preload Scripts**: Secure IPC communication only
4. **CSP Headers**: Configured in index.html
5. **Remote Content**: Avoid loading untrusted remote content

## Support

For deployment issues:
1. Check GitHub Actions logs
2. Review Electron Forge documentation
3. Consult platform-specific build guides
4. Open issues in the repository

---

Remember to keep this documentation updated as deployment processes evolve!
