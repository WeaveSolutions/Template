#!/usr/bin/env pwsh
<#
.SYNOPSIS
    Official MindsDB installation script for the Nexpo template
.DESCRIPTION
    This script installs and configures MindsDB using the official Docker commands
    and sets up integration with the multi-cloud infrastructure.
.PARAMETER DataDir
    Path where MindsDB data will be stored
.PARAMETER EnableAuth
    Switch to enable authentication
.PARAMETER Username
    MindsDB username (if authentication is enabled)
.PARAMETER Password
    MindsDB password (if authentication is enabled)
.PARAMETER EnableApis
    Comma-separated list of APIs to enable (http,mysql,mongodb,postgres)
.EXAMPLE
    ./install_mindsdb.ps1
    Installs MindsDB with default settings
.EXAMPLE
    ./install_mindsdb.ps1 -EnableAuth -Username "admin" -Password "secure_password"
    Installs MindsDB with authentication enabled
.EXAMPLE
    ./install_mindsdb.ps1 -EnableApis "http,mysql,mongodb"
    Installs MindsDB with specific APIs enabled
#>

param (
    [string]$DataDir = "./mdb_data",
    [switch]$EnableAuth = $false,
    [string]$Username = "admin",
    [System.Security.SecureString]$Password = (ConvertTo-SecureString "password" -AsPlainText -Force),
    [string]$EnableApis = "http,mysql"
)

# Verify Docker is installed
if (-not (Get-Command "docker" -ErrorAction SilentlyContinue)) {
    Write-Host "Error: Docker is not installed or not in PATH." -ForegroundColor Red
    Write-Host "Please install Docker first: https://www.docker.com/products/docker-desktop/" -ForegroundColor Red
    exit 1
}

# Create data directory if it doesn't exist
if (-not (Test-Path -Path $DataDir)) {
    Write-Host "Creating MindsDB data directory at $DataDir..." -ForegroundColor Cyan
    New-Item -ItemType Directory -Path $DataDir | Out-Null
}

# Build the Docker command
$dockerCmd = "docker run --name mindsdb_container"

# Add environment variables
if ($EnableAuth) {
    $dockerCmd += " -e MINDSDB_USERNAME='$Username' -e MINDSDB_PASSWORD='$Password'"
}

# Add APIs
$dockerCmd += " -e MINDSDB_APIS='$EnableApis'"

# Add MKL optimization to avoid training issues
$dockerCmd += " -e MKL_SERVICE_FORCE_INTEL=1"

# Add port mappings based on enabled APIs
$dockerCmd += " -p 47334:47334" # HTTP API and GUI (always needed)

if ($EnableApis.Contains("mysql")) {
    $dockerCmd += " -p 47335:47335" # MySQL API
}

if ($EnableApis.Contains("mongodb")) {
    $dockerCmd += " -p 47336:47336" # MongoDB API
}

if ($EnableApis.Contains("postgres")) {
    $dockerCmd += " -p 55432:55432" # PostgreSQL API
}

# Add volume mapping for data persistence
$absDataPath = Resolve-Path $DataDir -ErrorAction SilentlyContinue
if (-not $absDataPath) {
    $absDataPath = Join-Path (Get-Location) $DataDir
}
$dockerCmd += " -v '$absDataPath':/root/mdb_storage"

# Add detach flag to run in background
$dockerCmd += " -d"

# Add image name
$dockerCmd += " mindsdb/mindsdb:v25.6.2.0"

# Check if container already exists
$containerExists = docker ps -a --filter "name=mindsdb_container" --format "{{.Names}}" | Out-String
if ($containerExists.Trim() -eq "mindsdb_container") {
    Write-Host "MindsDB container already exists. Removing it..." -ForegroundColor Yellow
    docker rm -f mindsdb_container | Out-Null
}

# Run the Docker command
Write-Host "Installing MindsDB using official Docker image..." -ForegroundColor Cyan
Write-Host "Command: $dockerCmd" -ForegroundColor Gray
Invoke-Expression $dockerCmd

# Wait for MindsDB to start
Write-Host "Waiting for MindsDB to start..." -ForegroundColor Cyan
$maxRetries = 30
$retryCount = 0
$mindsdbReady = $false

while (-not $mindsdbReady -and $retryCount -lt $maxRetries) {
    Start-Sleep -Seconds 5
    $retryCount++
    
    try {
        $logs = docker logs mindsdb_container --tail 50 2>&1
        if ($logs -match "MindsDB API Server listening on") {
            $mindsdbReady = $true
        }
    }
    catch {
        Write-Host "Waiting for MindsDB to initialize... (Attempt $retryCount/$maxRetries)" -ForegroundColor Yellow
    }
}

if ($mindsdbReady) {
    Write-Host "MindsDB has been successfully installed and is running!" -ForegroundColor Green
    Write-Host "You can access MindsDB at http://localhost:47334" -ForegroundColor Green
    
    if ($EnableAuth) {
        Write-Host "Authentication is enabled with username: $Username" -ForegroundColor Green
    }
    
    # Display available APIs
    Write-Host "Enabled APIs:" -ForegroundColor Cyan
    if ($EnableApis.Contains("http")) {
        Write-Host "  - HTTP API and GUI: http://localhost:47334" -ForegroundColor White
    }
    if ($EnableApis.Contains("mysql")) {
        Write-Host "  - MySQL API: localhost:47335" -ForegroundColor White
    }
    if ($EnableApis.Contains("mongodb")) {
        Write-Host "  - MongoDB API: localhost:47336" -ForegroundColor White
    }
    if ($EnableApis.Contains("postgres")) {
        Write-Host "  - PostgreSQL API: localhost:55432" -ForegroundColor White
    }
    
    # Initialize MindsDB with cloud providers if init_mindsdb.py exists
    $initScript = Join-Path (Split-Path -Parent $PSScriptRoot) "mindsdb-main/init_mindsdb.py"
    if (Test-Path $initScript) {
        Write-Host "`nTo initialize MindsDB with your cloud providers, run:" -ForegroundColor Cyan
        Write-Host "python $initScript" -ForegroundColor White
    }
} else {
    Write-Host "MindsDB installation timed out. Check docker logs for details:" -ForegroundColor Red
    Write-Host "docker logs mindsdb_container" -ForegroundColor White
}
