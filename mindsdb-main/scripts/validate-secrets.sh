#!/bin/bash

# GitHub Secrets Validation Script
# This script reads your .env file and shows required secrets for enabled providers
# Run this before setting up GitHub repository secrets

set -e

echo "🔐 GitHub Secrets Validation for Next-Solito-Expo"
echo "=================================================="

# Load environment variables from .env
if [ -f ".env" ]; then
    # Export variables from .env file
    export $(grep -v '^#' .env | xargs)
    echo "✅ Loaded .env file"
else
    echo "❌ No .env file found!"
    echo "   Please copy .env.example to .env and configure your providers"
    echo "   Command: cp .env.example .env"
    exit 1
fi

echo ""
echo "🔍 Checking enabled providers from .env..."

# Function to check if a provider is enabled
check_provider() {
    local provider=$1
    local var_name=$2
    
    if [ "${!var_name}" = "true" ]; then
        echo "✅ $provider is ENABLED"
        return 0
    else
        echo "⚪ $provider is disabled"
        return 1
    fi
}

# Function to validate secret requirements
validate_secrets() {
    local provider=$1
    shift
    local secrets=("$@")
    
    echo "  📋 Required GitHub secrets for $provider:"
    for secret in "${secrets[@]}"; do
        echo "    - $secret"
    done
    echo ""
}

echo ""
echo "📋 Provider Status & Required GitHub Secrets:"
echo "============================================="

# Track enabled providers
enabled_providers=()

# AWS
if check_provider "AWS" "ENABLE_AWS"; then
    enabled_providers+=("AWS")
    validate_secrets "AWS" \
        "AWS_ACCESS_KEY_ID" \
        "AWS_SECRET_ACCESS_KEY" \
        "AWS_REGION"
fi

# GCP
if check_provider "GCP" "ENABLE_GCP"; then
    enabled_providers+=("GCP")
    validate_secrets "GCP" \
        "GCP_PROJECT_ID" \
        "GCP_CREDENTIALS" \
        "GCP_REGION"
fi

# Azure
if check_provider "Azure" "ENABLE_AZURE"; then
    enabled_providers+=("Azure")
    validate_secrets "Azure" \
        "AZURE_CLIENT_ID" \
        "AZURE_CLIENT_SECRET" \
        "AZURE_SUBSCRIPTION_ID" \
        "AZURE_TENANT_ID"
fi

# OCI
if check_provider "OCI" "ENABLE_OCI"; then
    enabled_providers+=("OCI")
    validate_secrets "OCI" \
        "OCI_TENANCY_OCID" \
        "OCI_USER_OCID" \
        "OCI_FINGERPRINT" \
        "OCI_PRIVATE_KEY" \
        "OCI_REGION"
fi

# IBM Cloud
if check_provider "IBM Cloud" "ENABLE_IBM"; then
    enabled_providers+=("IBM")
    validate_secrets "IBM Cloud" \
        "IBM_CLOUD_API_KEY" \
        "IBM_REGION" \
        "IBM_RESOURCE_GROUP"
fi

# DigitalOcean
if check_provider "DigitalOcean" "ENABLE_DIGITALOCEAN"; then
    enabled_providers+=("DigitalOcean")
    validate_secrets "DigitalOcean" \
        "DIGITALOCEAN_TOKEN" \
        "DIGITALOCEAN_REGION"
fi

# Cloudflare
if check_provider "Cloudflare" "ENABLE_CLOUDFLARE"; then
    enabled_providers+=("Cloudflare")
    validate_secrets "Cloudflare" \
        "CLOUDFLARE_API_TOKEN" \
        "CLOUDFLARE_ZONE_ID" \
        "CLOUDFLARE_ACCOUNT_ID"
fi

# HashiCorp Nomad
if check_provider "HashiCorp Nomad" "ENABLE_HASHICORP_NOMAD"; then
    enabled_providers+=("Nomad")
    validate_secrets "HashiCorp Nomad" \
        "NOMAD_ADDR" \
        "NOMAD_TOKEN" \
        "NOMAD_REGION"
fi

# HashiCorp Vault
if check_provider "HashiCorp Vault" "ENABLE_HASHICORP_VAULT"; then
    enabled_providers+=("Vault")
    validate_secrets "HashiCorp Vault" \
        "VAULT_ADDR" \
        "VAULT_TOKEN" \
        "VAULT_NAMESPACE"
fi

# HashiCorp Consul
if check_provider "HashiCorp Consul" "ENABLE_HASHICORP_CONSUL"; then
    enabled_providers+=("Consul")
    validate_secrets "HashiCorp Consul" \
        "CONSUL_HTTP_ADDR" \
        "CONSUL_HTTP_TOKEN" \
        "CONSUL_DATACENTER"
fi

# Heroku
if check_provider "Heroku" "ENABLE_HEROKU"; then
    enabled_providers+=("Heroku")
    validate_secrets "Heroku" \
        "HEROKU_API_KEY" \
        "HEROKU_EMAIL"
fi

# Auth0
if check_provider "Auth0" "ENABLE_AUTH0"; then
    enabled_providers+=("Auth0")
    validate_secrets "Auth0" \
        "AUTH0_DOMAIN" \
        "AUTH0_CLIENT_ID" \
        "AUTH0_CLIENT_SECRET"
fi

echo "🏗️ Core Infrastructure Secrets (Always Required):"
echo "================================================="
validate_secrets "Core Infrastructure" \
    "TF_VAR_environment" \
    "TF_VAR_project_name" \
    "TF_VAR_domain_name"

# Summary
echo "📊 Summary:"
echo "==========="
echo "Enabled providers: ${#enabled_providers[@]}"
for provider in "${enabled_providers[@]}"; do
    echo "  ✅ $provider"
done

echo ""
echo "🚀 Next Steps:"
echo "============="
echo "1. Go to GitHub Repository → Settings → Secrets and variables → Actions"
echo "2. Add the secrets listed above for your enabled providers"
echo "3. Use 'GitHub CLI' for bulk secret addition (see README.md)"
echo "4. Test deployment with: git push origin main"
echo ""
echo "📖 For detailed setup: docs/GitHub-Secrets-Setup.md"
echo "✅ Validation complete!"
