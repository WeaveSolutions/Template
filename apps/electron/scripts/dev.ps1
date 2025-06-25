# Electron Development Script for Windows PowerShell

Write-Host "🚀 Starting Nexpo Electron Development Environment..." -ForegroundColor Cyan

# Check if running from correct directory
$scriptPath = Split-Path -Parent $MyInvocation.MyCommand.Path
$electronRoot = Split-Path -Parent $scriptPath
$monorepoRoot = Split-Path -Parent (Split-Path -Parent $electronRoot)

# Check if pnpm is installed
try {
    $pnpmVersion = pnpm --version
    Write-Host "✓ pnpm version: $pnpmVersion" -ForegroundColor Green
} catch {
    Write-Host "❌ pnpm is not installed. Please install it first: npm install -g pnpm" -ForegroundColor Red
    exit 1
}

# Check platform feature flag
$envPath = Join-Path $monorepoRoot ".env"
if (Test-Path $envPath) {
    $electronEnabled = (Get-Content $envPath | Select-String "ENABLE_ELECTRON_PLATFORM=true")
    if (-not $electronEnabled) {
        Write-Host "⚠️  Warning: ENABLE_ELECTRON_PLATFORM is not set to true in .env" -ForegroundColor Yellow
    }
}

# Navigate to monorepo root
Set-Location $monorepoRoot
Write-Host "📁 Working directory: $monorepoRoot" -ForegroundColor Gray

# Install dependencies if needed
if (-not (Test-Path "node_modules")) {
    Write-Host "📦 Installing dependencies..." -ForegroundColor Yellow
    pnpm install
}

# Build shared packages
Write-Host "🔨 Building shared packages..." -ForegroundColor Yellow
$sharedPackages = @("shared", "ui", "utils")
foreach ($pkg in $sharedPackages) {
    $pkgPath = Join-Path $monorepoRoot "packages\$pkg"
    if (Test-Path $pkgPath) {
        Write-Host "  Building @nexpo/$pkg..." -ForegroundColor Gray
        pnpm --filter "@nexpo/$pkg" build
    }
}

# Navigate to Electron app
Set-Location $electronRoot
Write-Host "📱 Starting Electron app..." -ForegroundColor Cyan

# Start Electron
pnpm start

# Return to original directory
Set-Location $PSScriptRoot
