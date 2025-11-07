# Kill processes running on common dev ports
# Run this before starting `pnpm dev` to avoid EADDRINUSE errors

$ports = @(3000, 5173, 1420, 7000, 19000, 47335, 47336)

Write-Host "Killing processes on dev ports..." -ForegroundColor Cyan
Write-Host ""

foreach ($port in $ports) {
    $connections = Get-NetTCPConnection -LocalPort $port -ErrorAction SilentlyContinue
    
    if ($connections) {
        foreach ($conn in $connections) {
            $processId = $conn.OwningProcess
            $process = Get-Process -Id $processId -ErrorAction SilentlyContinue
            
            if ($process) {
                Write-Host "Port $port - Killing $($process.ProcessName) (PID: $processId)" -ForegroundColor Yellow
                Stop-Process -Id $processId -Force -ErrorAction SilentlyContinue
            }
        }
    } else {
        Write-Host "Port $port - Available" -ForegroundColor Green
    }
}

Write-Host ""
Write-Host "Port cleanup complete!" -ForegroundColor Green
Write-Host "You can now run: pnpm dev" -ForegroundColor Cyan
