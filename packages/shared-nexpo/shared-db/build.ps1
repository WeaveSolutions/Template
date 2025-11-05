# Build script for shared-db package with multi-cloud database compatibility
# This script handles generating Prisma schema and client based on current cloud provider

# Set error action preference
$ErrorActionPreference = "Stop"

Write-Host "🛠️ Building shared-db package for multi-cloud compatibility..." -ForegroundColor Cyan

# Ensure the directory exists
$ScriptPath = $PSScriptRoot
$PrismaDir = Join-Path -Path $ScriptPath -ChildPath "prisma"

# Check if we need to generate the schema
$SchemaGenerator = Join-Path -Path $PrismaDir -ChildPath "schema.ts"
if (Test-Path -Path $SchemaGenerator) {
    Write-Host "📄 Generating Prisma schema from active provider..." -ForegroundColor Yellow
    try {
        npx ts-node $SchemaGenerator
        if ($LASTEXITCODE -ne 0) {
            throw "Failed to generate schema"
        }
    }
    catch {
        Write-Host "❌ Error generating schema: $_" -ForegroundColor Red
        Write-Host "Using default provider (postgres)..." -ForegroundColor Yellow
        
        # Copy the postgres schema as a fallback
        $PostgresSchema = Join-Path -Path $PrismaDir -ChildPath "providers\postgres.prisma"
        $TargetSchema = Join-Path -Path $PrismaDir -ChildPath "schema.prisma"
        Copy-Item -Path $PostgresSchema -Destination $TargetSchema -Force
    }
}
else {
    Write-Host "⚠️ Schema generator not found, using default provider (postgres)" -ForegroundColor Yellow
    # Copy the postgres schema as a fallback
    $PostgresSchema = Join-Path -Path $PrismaDir -ChildPath "providers\postgres.prisma"
    $TargetSchema = Join-Path -Path $PrismaDir -ChildPath "schema.prisma"
    Copy-Item -Path $PostgresSchema -Destination $TargetSchema -Force
}

# Generate Prisma client
Write-Host "🔄 Generating Prisma client..." -ForegroundColor Yellow
npx prisma generate
if ($LASTEXITCODE -ne 0) {
    Write-Host "❌ Failed to generate Prisma client. Continuing with build without it." -ForegroundColor Red
}
else {
    Write-Host "✅ Prisma client generated successfully" -ForegroundColor Green
}

# Compile TypeScript code
Write-Host "📦 Compiling TypeScript code..." -ForegroundColor Yellow
pnpm run build
if ($LASTEXITCODE -ne 0) {
    Write-Host "❌ TypeScript compilation failed" -ForegroundColor Red
    exit 1
}

Write-Host "✅ Build completed successfully!" -ForegroundColor Green
