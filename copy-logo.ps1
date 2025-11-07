# Quick script to copy WeaveLogo.png to all platforms
# Run this after saving WeaveLogo.png to assets/images/

$source = "assets\images\WeaveLogo.png"

if (-not (Test-Path $source)) {
    Write-Host "ERROR: WeaveLogo.png not found at $source" -ForegroundColor Red
    Write-Host ""
    Write-Host "QUICK SETUP:" -ForegroundColor Cyan
    Write-Host "1. Right-click the Weave logo image in the chat" -ForegroundColor Yellow
    Write-Host "2. Select 'Save Image As...'" -ForegroundColor Yellow
    Write-Host "3. Save to: $((Get-Location).Path)\assets\images\WeaveLogo.png" -ForegroundColor Yellow
    Write-Host "4. Run this script again: .\copy-logo.ps1" -ForegroundColor Yellow
    Write-Host ""
    Write-Host "TIP: The logo is the colorful gradient ring in the chat messages above" -ForegroundColor Green
    
    # Try to open the assets/images folder to make it easier
    $targetDir = Join-Path (Get-Location).Path "assets\images"
    if (Test-Path $targetDir) {
        Write-Host ""
        Write-Host "Opening destination folder..." -ForegroundColor Cyan
        Start-Process explorer.exe $targetDir
    }
    
    exit 1
}

Write-Host "Found logo at: $source" -ForegroundColor Green
Write-Host ""
Write-Host "Copying to all platforms..." -ForegroundColor Cyan

$destinations = @(
    "apps\nexpo\nextWeb\public\WeaveLogo.png",
    "apps\taurte\svelteWeb\public\WeaveLogo.png",
    "apps\desktop\public\WeaveLogo.png",
    "apps\taurte\tauriMobile\public\WeaveLogo.png"
)

foreach ($dest in $destinations) {
    Copy-Item $source $dest -Force
    Write-Host "Copied to: $dest" -ForegroundColor Green
}

Write-Host ""
Write-Host "Logo copied to all 4 platforms!" -ForegroundColor Green
Write-Host "Refresh your browser to see the changes." -ForegroundColor Cyan
