# MindsDB Environment Isolation Wrapper
# This script creates a completely clean environment for MindsDB

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

# Create a minimal environment for MindsDB
# Only include essential system variables, EXCLUDE debug/log variables
$cleanEnv = @{
    'PATH' = $env:PATH
    'SYSTEMROOT' = $env:SYSTEMROOT
    'TEMP' = $env:TEMP
    'TMP' = $env:TMP
    'USERPROFILE' = $env:USERPROFILE
    'NUMEXPR_MAX_THREADS' = '16'
    'PYTHONWARNINGS' = 'ignore::UserWarning,ignore::DeprecationWarning'
}

# Start MindsDB in a new process with clean environment
$psi = New-Object System.Diagnostics.ProcessStartInfo
$psi.FileName = $pythonCmd
$psi.Arguments = "-m mindsdb --api http,mysql --config `"$configPath`""
$psi.UseShellExecute = $false
$psi.CreateNoWindow = $false

# Clear default environment and add only our clean variables
$psi.EnvironmentVariables.Clear()
foreach ($key in $cleanEnv.Keys) {
    $psi.EnvironmentVariables[$key] = $cleanEnv[$key]
}

$process = [System.Diagnostics.Process]::Start($psi)
$process.WaitForExit()
