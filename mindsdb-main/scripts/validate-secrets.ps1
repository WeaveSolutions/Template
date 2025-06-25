# GitHub Secrets Validation Script (PowerShell)
# This script reads your .env file and shows required secrets for enabled providers

Write-Host "🔐 GitHub Secrets Validation for Next-Solito-Expo" -ForegroundColor Cyan
Write-Host "==================================================" -ForegroundColor Cyan

# Function to load .env file
function Import-EnvFile {
    param([string]$Path)
    
    if (Test-Path $Path) {
        Get-Content $Path | ForEach-Object {
            if ($_ -match "^([^#][^=]+)=(.*)$") {
                [Environment]::SetEnvironmentVariable($matches[1], $matches[2], "Process")
            }
        }
        Write-Host "✅ Loaded .env file" -ForegroundColor Green
        return $true
    }
    return $false
}

# Load environment variables from .env
if (-not (Import-EnvFile ".env")) {
    Write-Host "❌ No .env file found!" -ForegroundColor Red
    Write-Host "   Please copy .env.example to .env and configure your providers" -ForegroundColor Yellow
    Write-Host "   Command: Copy-Item .env.example .env" -ForegroundColor Yellow
    exit 1
}

Write-Host ""
Write-Host "🔍 Checking enabled providers from .env..." -ForegroundColor Yellow

# Function to check if a provider is enabled
function Test-ProviderEnabled {
    param([string]$ProviderName, [string]$VarName)
    
    $value = [Environment]::GetEnvironmentVariable($VarName)
    if ($value -eq "true") {
        Write-Host "✅ $ProviderName is ENABLED" -ForegroundColor Green
        return $true
    } else {
        Write-Host "⚪ $ProviderName is disabled" -ForegroundColor Gray
        return $false
    }
}

# Function to show required secrets
function Show-RequiredSecrets {
    param([string]$Provider, [string[]]$Secrets)
    
    Write-Host "  📋 Required GitHub secrets for ${Provider}:" -ForegroundColor Cyan
    foreach ($secret in $Secrets) {
        Write-Host "    - $secret" -ForegroundColor White
    }
    Write-Host ""
}

Write-Host ""
Write-Host "📋 Provider Status & Required GitHub Secrets:" -ForegroundColor Cyan
Write-Host "=============================================" -ForegroundColor Cyan

$enabledProviders = @()

# Check each provider
if (Test-ProviderEnabled "AWS" "ENABLE_AWS") {
    $enabledProviders += "AWS"
    Show-RequiredSecrets "AWS" @("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_REGION")
}

if (Test-ProviderEnabled "GCP" "ENABLE_GCP") {
    $enabledProviders += "GCP"
    Show-RequiredSecrets "GCP" @("GCP_PROJECT_ID", "GCP_CREDENTIALS", "GCP_REGION")
}

if (Test-ProviderEnabled "Azure" "ENABLE_AZURE") {
    $enabledProviders += "Azure"
    Show-RequiredSecrets "Azure" @("AZURE_CLIENT_ID", "AZURE_CLIENT_SECRET", "AZURE_SUBSCRIPTION_ID", "AZURE_TENANT_ID")
}

if (Test-ProviderEnabled "OCI" "ENABLE_OCI") {
    $enabledProviders += "OCI"
    Show-RequiredSecrets "OCI" @("OCI_TENANCY_OCID", "OCI_USER_OCID", "OCI_FINGERPRINT", "OCI_PRIVATE_KEY", "OCI_REGION")
}

if (Test-ProviderEnabled "IBM Cloud" "ENABLE_IBM") {
    $enabledProviders += "IBM"
    Show-RequiredSecrets "IBM Cloud" @("IBM_CLOUD_API_KEY", "IBM_REGION", "IBM_RESOURCE_GROUP")
}

if (Test-ProviderEnabled "DigitalOcean" "ENABLE_DIGITALOCEAN") {
    $enabledProviders += "DigitalOcean"
    Show-RequiredSecrets "DigitalOcean" @("DIGITALOCEAN_TOKEN", "DIGITALOCEAN_REGION")
}

if (Test-ProviderEnabled "Cloudflare" "ENABLE_CLOUDFLARE") {
    $enabledProviders += "Cloudflare"
    Show-RequiredSecrets "Cloudflare" @("CLOUDFLARE_API_TOKEN", "CLOUDFLARE_ZONE_ID", "CLOUDFLARE_ACCOUNT_ID")
}

if (Test-ProviderEnabled "HashiCorp Nomad" "ENABLE_HASHICORP_NOMAD") {
    $enabledProviders += "Nomad"
    Show-RequiredSecrets "HashiCorp Nomad" @("NOMAD_ADDR", "NOMAD_TOKEN", "NOMAD_REGION")
}

if (Test-ProviderEnabled "HashiCorp Vault" "ENABLE_HASHICORP_VAULT") {
    $enabledProviders += "Vault"
    Show-RequiredSecrets "HashiCorp Vault" @("VAULT_ADDR", "VAULT_TOKEN", "VAULT_NAMESPACE")
}

if (Test-ProviderEnabled "HashiCorp Consul" "ENABLE_HASHICORP_CONSUL") {
    $enabledProviders += "Consul"
    Show-RequiredSecrets "HashiCorp Consul" @("CONSUL_HTTP_ADDR", "CONSUL_HTTP_TOKEN", "CONSUL_DATACENTER")
}

if (Test-ProviderEnabled "Heroku" "ENABLE_HEROKU") {
    $enabledProviders += "Heroku"
    Show-RequiredSecrets "Heroku" @("HEROKU_API_KEY", "HEROKU_EMAIL")
}

if (Test-ProviderEnabled "Auth0" "ENABLE_AUTH0") {
    $enabledProviders += "Auth0"
    Show-RequiredSecrets "Auth0" @("AUTH0_DOMAIN", "AUTH0_CLIENT_ID", "AUTH0_CLIENT_SECRET")
}

Write-Host "🏗️ Core Infrastructure Secrets (Always Required):" -ForegroundColor Cyan
Write-Host "=================================================" -ForegroundColor Cyan
Show-RequiredSecrets "Core Infrastructure" @("TF_VAR_environment", "TF_VAR_project_name", "TF_VAR_domain_name")

# Summary
Write-Host "📊 Summary:" -ForegroundColor Cyan
Write-Host "===========" -ForegroundColor Cyan
Write-Host "Enabled providers: $($enabledProviders.Count)" -ForegroundColor Yellow
foreach ($provider in $enabledProviders) {
    Write-Host "  ✅ $provider" -ForegroundColor Green
}

Write-Host ""
Write-Host "🚀 Next Steps:" -ForegroundColor Cyan
Write-Host "=============" -ForegroundColor Cyan
Write-Host "1. Go to GitHub Repository → Settings → Secrets and variables → Actions" -ForegroundColor White
Write-Host "2. Add the secrets listed above for your enabled providers" -ForegroundColor White
Write-Host "3. Use 'GitHub CLI' for bulk secret addition (see README.md)" -ForegroundColor White
Write-Host "4. Test deployment with: git push origin main" -ForegroundColor White
Write-Host ""
Write-Host "📖 For detailed setup: docs/GitHub-Secrets-Setup.md" -ForegroundColor Yellow
Write-Host "✅ Validation complete!" -ForegroundColor Green
