# Quick script to copy WeaveLogo.png to all platforms
# Run this after saving WeaveLogo.png to assets/images/

$source = "assets\images\WeaveLogo.png"

if (-not (Test-Path $source)) {
    Write-Host "ERROR: WeaveLogo.png not found at $source" -ForegroundColor Red
    Write-Host "Please save the logo image to assets\images\WeaveLogo.png" -ForegroundColor Yellow
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
