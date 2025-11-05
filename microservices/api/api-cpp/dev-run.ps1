# C++ Drogon API Development Runner (Windows PowerShell)
# This script builds and runs the C++ API locally for development

# Set error action preference
$ErrorActionPreference = "Stop"

try {
    Write-Host "🔧 Building C++ Drogon API..." -ForegroundColor Cyan
    
    # Validate 64-bit architecture
    if ($env:PROCESSOR_ARCHITECTURE -ne "AMD64" -and $env:PROCESSOR_ARCHITEW6432 -ne "AMD64") {
        Write-Host "⚠️ Warning: Non-64-bit architecture detected. This may cause issues." -ForegroundColor Yellow
        Write-Host "Architecture: $($env:PROCESSOR_ARCHITECTURE)" -ForegroundColor Gray
    } else {
        Write-Host "✅ 64-bit architecture confirmed" -ForegroundColor Green
    }
    
    # Check for required tools
    if (!(Get-Command cmake -ErrorAction SilentlyContinue)) {
        Write-Host "❌ CMake not found. Please install CMake first." -ForegroundColor Red
        exit 1
    }
    
    if (!(Get-Command make -ErrorAction SilentlyContinue)) {
        Write-Host "❌ Make not found. Please install MinGW or Visual Studio Build Tools." -ForegroundColor Red
        exit 1
    }
    
    # Create build directory
    if (!(Test-Path "build")) {
        Write-Host "📁 Creating build directory..." -ForegroundColor Yellow
        New-Item -ItemType Directory -Path "build" | Out-Null
    }
    
    Set-Location "build"
    
    # Configure with CMake
    Write-Host "⚙️ Configuring with CMake..." -ForegroundColor Yellow
    cmake ..
    if ($LASTEXITCODE -ne 0) {
        throw "CMake configuration failed"
    }
    
    # Build the project (using number of processors)
    $cores = (Get-WmiObject -Class Win32_ComputerSystem).NumberOfLogicalProcessors
    Write-Host "🔨 Building with $cores cores..." -ForegroundColor Yellow
    make -j $cores
    if ($LASTEXITCODE -ne 0) {
        throw "Build failed"
    }
    
    Write-Host "✅ Build completed successfully!" -ForegroundColor Green
    Write-Host "🚀 Starting C++ API server on port 8110..." -ForegroundColor Green
    Write-Host "📡 Health check: http://localhost:8110/health" -ForegroundColor Cyan
    Write-Host "📚 API docs: http://localhost:8110/api/v1/cpp" -ForegroundColor Cyan
    Write-Host "Press Ctrl+C to stop the server" -ForegroundColor Gray
    Write-Host ""
    
    # Run the server
    .\nexpo-api-cpp.exe
    
} catch {
    Write-Host "❌ Error: $($_.Exception.Message)" -ForegroundColor Red
    exit 1
} finally {
    # Return to original directory
    if (Test-Path "..") {
        Set-Location ".."
    }
}
