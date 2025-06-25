#!/usr/bin/env pwsh
<#
.SYNOPSIS
    A script to run Terraform commands for multi-cloud deployment
.DESCRIPTION
    This script handles terraform init, plan, apply, and destroy commands with appropriate
    variables and configurations for multi-cloud deployments.
.PARAMETER Plan
    Switch parameter to run terraform plan
.PARAMETER Apply
    Switch parameter to run terraform apply
.PARAMETER Destroy
    Switch parameter to run terraform destroy
.PARAMETER Init
    Switch parameter to run terraform init
.PARAMETER OutputPath
    Path to save terraform output results
.EXAMPLE
    ./run_terraform.ps1 -Init
    Initializes the terraform environment
.EXAMPLE
    ./run_terraform.ps1 -Plan
    Runs terraform plan to show what would be changed
.EXAMPLE
    ./run_terraform.ps1 -Apply
    Applies the terraform configuration
#>

param (
    [switch]$Plan = $false,
    [switch]$Apply = $false,
    [switch]$Destroy = $false,
    [switch]$Init = $false,
    [string]$OutputPath = "./terraform-output.json"
)

# Validate at least one action is specified
if (-not ($Plan -or $Apply -or $Destroy -or $Init)) {
    Write-Host "You must specify at least one action: -Plan, -Apply, -Destroy, or -Init" -ForegroundColor Red
    exit 1
}

# Default values
$outputPath = "./terraform-output.json"
$terraformDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$rootDir = Split-Path -Parent $terraformDir
$tfvarsFile = Join-Path $terraformDir "terraform.tfvars"
$envFile = Join-Path -Path $rootDir -ChildPath ".env"

# Load environment variables from .env file if it exists
if (Test-Path -Path $envFile) {
    Write-Host "Loading environment variables from .env file..." -ForegroundColor Cyan
    Get-Content $envFile | ForEach-Object {
        if ($_ -match '^([^#].+?)=(.*)$') {
            $name = $matches[1].Trim()
            $value = $matches[2].Trim()
            
            # Strip quotes if present and remove trailing comments
            if ($value -match '^["''](.+)["'']$') {
                $value = $matches[1]
            }
            # Remove comments after the value
            if ($value -match '(.+?)\s*#.*') {
                $value = $matches[1].Trim()
            }
            
            # Handle PROJECT_NAME specially
            if ($name -eq "PROJECT_NAME") {
                [System.Environment]::SetEnvironmentVariable("TF_VAR_project_name", $value, [System.EnvironmentVariableTarget]::Process)
                Write-Host "  Set TF_VAR_project_name from PROJECT_NAME environment variable" -ForegroundColor Gray
            } else {
                # Export as TF_VAR_* environment variable for other variables
                [System.Environment]::SetEnvironmentVariable("TF_VAR_$name", $value, [System.EnvironmentVariableTarget]::Process)
                Write-Host "  Set TF_VAR_$name environment variable" -ForegroundColor Gray
            }
        }
    }
    Write-Host "Environment variables loaded" -ForegroundColor Green
}

# Check if terraform is installed
if (-not (Get-Command "terraform" -ErrorAction SilentlyContinue)) {
    Write-Host "Terraform is not installed or not in PATH. Please install Terraform first." -ForegroundColor Red
    exit 1
}

# Check if terraform.tfvars exists
if (-not (Test-Path -Path $tfvarsFile)) {
    Write-Host "Warning: terraform.tfvars file not found. Creating from example file..." -ForegroundColor Yellow
    
    $exampleFile = Join-Path -Path $terraformDir -ChildPath "terraform.tfvars.example"
    if (Test-Path -Path $exampleFile) {
        Copy-Item -Path $exampleFile -Destination $tfvarsFile
        Write-Host "Created terraform.tfvars from example. Please review and modify if needed." -ForegroundColor Yellow
    } else {
        Write-Host "Error: terraform.tfvars.example not found. Please create terraform.tfvars manually." -ForegroundColor Red
        exit 1
    }
}

# Change to terraform directory
Push-Location -Path $terraformDir

try {
    # Initialize if requested
    if ($Init) {
        Write-Host "Initializing Terraform..." -ForegroundColor Cyan
        terraform init
        if ($LASTEXITCODE -ne 0) {
            Write-Host "Terraform init failed" -ForegroundColor Red
            exit $LASTEXITCODE
        }
        Write-Host "Terraform initialization complete" -ForegroundColor Green
    }

    # Plan if requested
    if ($Plan) {
        Write-Host "Running Terraform plan..." -ForegroundColor Cyan
        terraform plan -var-file="$tfvarsFile" -out="terraform.tfplan"
        if ($LASTEXITCODE -ne 0) {
            Write-Host "Terraform plan failed" -ForegroundColor Red
            exit $LASTEXITCODE
        }
        Write-Host "Terraform plan complete" -ForegroundColor Green
    }

    # Apply if requested
    if ($Apply) {
        Write-Host "Running Terraform apply..." -ForegroundColor Cyan
        
        # Check if we have a plan file
        if (Test-Path -Path "terraform.tfplan") {
            terraform apply "terraform.tfplan"
        } else {
            # Ask for confirmation when applying without a plan
            $confirmation = Read-Host "No plan file found. Do you want to apply without a plan? (y/n)"
            if ($confirmation -eq 'y') {
                terraform apply -var-file="$tfvarsFile" -auto-approve
            } else {
                Write-Host "Apply canceled" -ForegroundColor Yellow
                exit 0
            }
        }
        
        if ($LASTEXITCODE -ne 0) {
            Write-Host "Terraform apply failed" -ForegroundColor Red
            exit $LASTEXITCODE
        }
        
        # Save output to file if apply was successful
        terraform output -json | Out-File -FilePath $OutputPath
        Write-Host "Terraform output saved to $OutputPath" -ForegroundColor Green
        Write-Host "Terraform apply complete" -ForegroundColor Green
    }

    # Destroy if requested
    if ($Destroy) {
        Write-Host "WARNING: This will destroy all resources managed by Terraform" -ForegroundColor Red
        $confirmation = Read-Host "Are you sure you want to destroy? Type 'yes' to confirm"
        
        if ($confirmation -eq 'yes') {
            Write-Host "Running Terraform destroy..." -ForegroundColor Cyan
            terraform destroy -var-file="$tfvarsFile" -auto-approve
            
            if ($LASTEXITCODE -ne 0) {
                Write-Host "Terraform destroy failed" -ForegroundColor Red
                exit $LASTEXITCODE
            }
            Write-Host "Terraform destroy complete" -ForegroundColor Green
        } else {
            Write-Host "Destroy canceled" -ForegroundColor Yellow
        }
    }
}
finally {
    # Return to original directory
    Pop-Location
}
