# MindsDB Environment Isolation Wrapper
# This script completely removes DEBUG and LOG_LEVEL variables before starting MindsDB

# COMPLETELY remove DEBUG-related variables using Remove-Item
Remove-Item Env:\DEBUG -ErrorAction SilentlyContinue
Remove-Item Env:\debug -ErrorAction SilentlyContinue
Remove-Item Env:\LOG_LEVEL -ErrorAction SilentlyContinue
Remove-Item Env:\log_level -ErrorAction SilentlyContinue
Remove-Item Env:\VITE_DEBUG -ErrorAction SilentlyContinue

# Set MindsDB-safe variables
$env:NUMEXPR_MAX_THREADS = "16"
$env:PYTHONWARNINGS = "ignore::UserWarning,ignore::DeprecationWarning"

# Get arguments passed from Node.js
$configPath = $args[0]

# Find Python - try multiple common locations
$pythonPaths = @(
    "python",
    "python.exe",
    "$env:USERPROFILE\anaconda3\python.exe",
    "C:\Users\$env:USERNAME\anaconda3\python.exe",
    "$env:USERPROFILE\miniconda3\python.exe",
    "C:\Python312\python.exe",
    "C:\Python311\python.exe",
    "C:\Python310\python.exe"
)

$pythonCmd = $null
foreach ($path in $pythonPaths) {
    if (Get-Command $path -ErrorAction SilentlyContinue) {
        $pythonCmd = $path
        Write-Host "Found Python at: $pythonCmd"
        break
    }
}

if (-not $pythonCmd) {
    Write-Error "Python not found. Please ensure Python/Anaconda is installed and in PATH."
    exit 1
}

# Start MindsDB with clean environment
& $pythonCmd -m mindsdb --api http,mysql --config $configPath
