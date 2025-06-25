# Starting Nexpo

How to start Nexpo on each platform.

# Expo
Start mobile application
```bash
# Expo only
pnpm dev:mobile   

# Start mobile application
npx expo start

# Start mobile application on Android
npx expo start --android

# Start mobile application on iOS
npx expo start --ios
```

# Next.js
Start server command:
```bash
# Start server
pnpm run dev

# Unit tests
pnpm run test

# Integration tests with external APIs
pnpm run test:integration

# n8n workflow tests
pnpm run test:workflows

# Data quality tests
pnpm run test:data-quality
```

# Electron
Build for specific platforms:
```bash
# Current Operating System
cd apps/electron; pnpm start

# Windows (from Windows)
pnpm run make -- --platform=win32

# macOS (from macOS)
pnpm run make -- --platform=darwin

# Linux (from Linux)
pnpm run make -- --platform=linux
```

# Backend services
pnpm dev:backend  # Start microservices
pnpm start:mindsdb # Start MindsDB

# Testing
pnpm test         # Run all tests
pnpm lint         # Lint all packages

### Clean and Reinstall
```bash
# Complete reset
pnpm clean
rm -rf node_modules
rm -rf apps/*/node_modules
rm -rf packages/*/node_modules
pnpm install
```