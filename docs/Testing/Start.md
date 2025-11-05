# Starting Nexpo Development

Comprehensive guide to starting Nexpo across all platforms and environments.

## 🚀 Quick Start (Most Common)

**Complete Development Stack** - starts web frontend, all enabled backends, and MindsDB:
```bash
pnpm run dev
# Runs: Web + API Backends + MindsDB (complete stack)
# Console shows: WEB (cyan), API (yellow), DB (green)
```

## 🎯 Platform-Specific Development

### Next.js Web Application
```bash
# Web frontend only
pnpm run dev:web           # Next.js development server (port 3000)
```

### Expo Mobile Application
```bash
# Using pnpm scripts (recommended)
pnpm run dev:mobile         # Expo development server

# Using Expo CLI directly
npx expo start              # Start development server
npx expo start --android    # Start with Android simulator
npx expo start --ios        # Start with iOS simulator
```

### Tauri Desktop Application
```bash
# Desktop development
pnpm run dev:desktop        # Tauri desktop app

# Alternative: navigate to app directory
cd apps/tauri
pnpm run desktop            # Standalone React mode
pnpm run dev:next-mode      # Next.js integration mode
```

## 🌍 Environment-Specific Development

**Development Environment** (default colors):
```bash
pnpm run dev:development    # NODE_ENV=development, cyan/yellow colors
```

**Staging Environment** (distinct colors):
```bash
pnpm run dev:staging        # NODE_ENV=staging, blue/green colors
```

**Production Environment** (warning colors):
```bash
pnpm run dev:production     # NODE_ENV=production, red/magenta colors
```

## 🔧 Backend-Only Development

**API Microservices** (polyglot backends on ports 8020-8110):
```bash
pnpm run dev:backends       # All enabled API backends
pnpm run dev:backend        # Same as above (alias)
pnpm run dev:backend-only   # Same as above (alias)
```

**MindsDB AI/ML Database**:
```bash
pnpm run start:mindsdb      # Start MindsDB server (port 4040)
pnpm run stop:mindsdb       # Stop MindsDB server
pnpm run restart:mindsdb    # Restart MindsDB server
pnpm run logs:mindsdb       # View MindsDB logs
```

## 🏗️ Build Commands

### Next.js Web Build
```bash
# Production build
cd apps/next
pnpm run build
pnpm run start              # Serve production build
```

### Expo Mobile Build
```bash
# Development builds
cd apps/expo
npx expo run:android        # Build for Android
npx expo run:ios            # Build for iOS
```

### Tauri Desktop Build
```bash
cd apps/tauri

# Current Operating System
pnpm run build:desktop

# Cross-platform builds
pnpm tauri build --target x86_64-pc-windows-msvc      # Windows 64-bit
pnpm tauri build --target x86_64-apple-darwin         # macOS Intel
pnpm tauri build --target aarch64-apple-darwin        # macOS Apple Silicon
pnpm tauri build --target x86_64-unknown-linux-gnu    # Linux 64-bit
```

## 🧪 Testing & Quality

**Testing Commands**:
```bash
pnpm run test               # Run all tests
pnpm run test:integration   # Integration tests with external APIs
pnpm run test:workflows     # n8n workflow tests
pnpm run test:data-quality  # Data quality tests
```

**Code Quality**:
```bash
pnpm run lint               # Lint all packages
pnpm run build              # Build all packages
```

## 🛠️ Troubleshooting & Maintenance

### Complete Reset
```bash
# Complete dependency reset (Windows PowerShell)
pnpm clean
Remove-Item -Recurse -Force node_modules
Remove-Item -Recurse -Force apps/*/node_modules
Remove-Item -Recurse -Force packages/*/node_modules
pnpm install

# Complete dependency reset (Unix/Linux/macOS)
pnpm clean
rm -rf node_modules
rm -rf apps/*/node_modules
rm -rf packages/*/node_modules
pnpm install
```

### Common Issues

**Native Binding Errors** (TailwindCSS v4, PostCSS):
```bash
# Clear and reinstall native dependencies
pnpm clean
Remove-Item -Recurse -Force node_modules  # Windows
# rm -rf node_modules                     # Unix/Linux/macOS
pnpm install
```

**Port Conflicts**:
[docs\Ports\Ports.md](docs\Ports\Ports.md)

- Web: 3000 (Next.js)
- API Gateway: 8000 (Kong)
- Backends: 8020-8110 (Various languages)
- MindsDB: 4040 (AI/ML database)

### Command Quick Reference

| Command | Purpose | Output |
|---------|---------|--------|
| `pnpm run dev` | Complete stack | Web + APIs + DB |
| `pnpm run dev:web` | Web only | Next.js frontend |
| `pnpm run dev:mobile` | Mobile only | Expo development |
| `pnpm run dev:desktop` | Desktop only | Tauri application |
| `pnpm run dev:backends` | APIs only | All backend services |
| `pnpm run dev:development` | Dev environment | Complete stack (dev config) |
| `pnpm run dev:staging` | Staging environment | Complete stack (staging config) |
| `pnpm run dev:production` | Production environment | Complete stack (prod config) |
```