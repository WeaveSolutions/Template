# Kill processes running on common dev ports and dev executables
# Run this before starting `pnpm dev` to avoid EADDRINUSE and file lock errors

$ports = @(3000, 5173, 1420, 7000, 19000, 47335, 47336)
$processNames = @("taurte", "node", "vite")

Write-Host "Killing dev processes and cleaning ports..." -ForegroundColor Cyan
Write-Host ""

# Kill known dev processes that can cause locks
Write-Host "Cleaning dev processes:" -ForegroundColor Cyan
foreach ($procName in $processNames) {
    $processes = Get-Process -Name $procName -ErrorAction SilentlyContinue
    if ($processes) {
        foreach ($proc in $processes) {
            Write-Host "  Killing $procName (PID: $($proc.Id))" -ForegroundColor Yellow
            Stop-Process -Id $proc.Id -Force -ErrorAction SilentlyContinue
        }
    }
}

Write-Host ""
Write-Host "Cleaning ports:" -ForegroundColor Cyan

foreach ($port in $ports) {
    $connections = Get-NetTCPConnection -LocalPort $port -ErrorAction SilentlyContinue
    
    if ($connections) {
        foreach ($conn in $connections) {
            $processId = $conn.OwningProcess
            $process = Get-Process -Id $processId -ErrorAction SilentlyContinue
            
            if ($process) {
                Write-Host "  Port $port - Killing $($process.ProcessName) (PID: $processId)" -ForegroundColor Yellow
                Stop-Process -Id $processId -Force -ErrorAction SilentlyContinue
            }
        }
    } else {
        Write-Host "  Port $port - Available" -ForegroundColor Green
    }
}

Write-Host ""
Write-Host "Cleanup complete!" -ForegroundColor Green
Write-Host "You can now run: pnpm dev" -ForegroundColor Cyan
