#!/bin/bash

# Electron Development Script for Unix-like systems (macOS/Linux)

echo -e "\033[0;36m🚀 Starting Nexpo Electron Development Environment...\033[0m"

# Get script directory
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ELECTRON_ROOT="$(dirname "$SCRIPT_DIR")"
MONOREPO_ROOT="$(dirname "$(dirname "$ELECTRON_ROOT")")"

# Check if pnpm is installed
if ! command -v pnpm &> /dev/null; then
    echo -e "\033[0;31m❌ pnpm is not installed. Please install it first: npm install -g pnpm\033[0m"
    exit 1
fi

PNPM_VERSION=$(pnpm --version)
echo -e "\033[0;32m✓ pnpm version: $PNPM_VERSION\033[0m"

# Check platform feature flag
ENV_FILE="$MONOREPO_ROOT/.env"
if [ -f "$ENV_FILE" ]; then
    if ! grep -q "ENABLE_ELECTRON_PLATFORM=true" "$ENV_FILE"; then
        echo -e "\033[0;33m⚠️  Warning: ENABLE_ELECTRON_PLATFORM is not set to true in .env\033[0m"
    fi
fi

# Navigate to monorepo root
cd "$MONOREPO_ROOT" || exit 1
echo -e "\033[0;90m📁 Working directory: $MONOREPO_ROOT\033[0m"

# Install dependencies if needed
if [ ! -d "node_modules" ]; then
    echo -e "\033[0;33m📦 Installing dependencies...\033[0m"
    pnpm install
fi

# Build shared packages
echo -e "\033[0;33m🔨 Building shared packages...\033[0m"
SHARED_PACKAGES=("shared" "ui" "utils")
for pkg in "${SHARED_PACKAGES[@]}"; do
    PKG_PATH="$MONOREPO_ROOT/packages/$pkg"
    if [ -d "$PKG_PATH" ]; then
        echo -e "\033[0;90m  Building @nexpo/$pkg...\033[0m"
        pnpm --filter "@nexpo/$pkg" build
    fi
done

# Navigate to Electron app
cd "$ELECTRON_ROOT" || exit 1
echo -e "\033[0;36m📱 Starting Electron app...\033[0m"

# Start Electron
pnpm start
