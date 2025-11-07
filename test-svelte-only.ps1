# Quick test to start Svelte Web directly and see any errors
Write-Host "Testing Svelte Web startup on port 5173..." -ForegroundColor Cyan
Write-Host ""

Set-Location "apps\taurte\svelteWeb"

Write-Host "Running: pnpm dev" -ForegroundColor Yellow
Write-Host "Press Ctrl+C to stop" -ForegroundColor Gray
Write-Host ""

pnpm dev
