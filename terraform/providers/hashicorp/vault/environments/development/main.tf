# HashiCorp Vault Infrastructure - Development Environment

terraform {
  required_version = ">= 1.5"
  required_providers {
    vault = {
      source  = "hashicorp/vault"
      version = "~> 4.0"
    }
  }
  
  backend "s3" {
    # Backend configuration will be provided during terraform init
    # bucket = "your-terraform-state-bucket"
    # key    = "vault/development/terraform.tfstate"
    # region = "us-east-1"
  }
}

# Configure the Vault Provider
provider "vault" {
  address = var.vault_address
  token   = var.vault_token
}

# Local values
locals {
  environment = "development"
  project_name = "next-solito-expo"
  path_prefix = "${local.project_name}/${local.environment}"
}

# Mount KV v2 secret engine for application secrets
resource "vault_mount" "app_secrets" {
  path        = "secret"
  type        = "kv"
  options     = { version = "2" }
  description = "Application secrets for ${local.project_name}"
}

# Mount database secret engine for dynamic credentials
resource "vault_mount" "database" {
  count = var.enable_database_secrets ? 1 : 0
  
  path        = "database"
  type        = "database"
  description = "Database dynamic credentials"
}

# Configure PostgreSQL database connection
resource "vault_database_secret_backend_connection" "postgres" {
  count = var.enable_database_secrets ? 1 : 0
  
  backend     = vault_mount.database[0].path
  name        = "postgres"
  plugin_name = "postgresql-database-plugin"
  
  connection_url = var.database_connection_url
  
  allowed_roles = ["app-role"]
  
  postgresql {
    username = var.database_username
    password = var.database_password
  }
}

# Create database role for application
resource "vault_database_secret_backend_role" "app_role" {
  count = var.enable_database_secrets ? 1 : 0
  
  backend             = vault_mount.database[0].path
  name                = "app-role"
  db_name             = vault_database_secret_backend_connection.postgres[0].name
  creation_statements = [
    "CREATE ROLE \"{{name}}\" WITH LOGIN PASSWORD '{{password}}' VALID UNTIL '{{expiration}}';",
    "GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA public TO \"{{name}}\";"
  ]
  default_ttl = 3600
  max_ttl     = 7200
}

# Enable JWT auth method for application authentication
resource "vault_auth_backend" "jwt" {
  count = var.enable_jwt_auth ? 1 : 0
  
  type = "jwt"
  path = "jwt"
}

# Configure JWT auth method
resource "vault_jwt_auth_backend" "app_jwt" {
  count = var.enable_jwt_auth ? 1 : 0
  
  backend         = vault_auth_backend.jwt[0].path
  oidc_discovery_url = var.jwt_discovery_url
  bound_issuer    = var.jwt_issuer
}

# Create JWT auth role for application
resource "vault_jwt_auth_backend_role" "app_role" {
  count = var.enable_jwt_auth ? 1 : 0
  
  backend           = vault_auth_backend.jwt[0].path
  role_name         = "app-role"
  token_policies    = [vault_policy.app_policy.name]
  
  bound_audiences   = [var.jwt_audience]
  bound_claims = {
    sub = var.jwt_subject
  }
  
  user_claim      = "sub"
  role_type       = "jwt"
  token_ttl       = 3600
  token_max_ttl   = 7200
}

# Application policy for reading secrets
resource "vault_policy" "app_policy" {
  name = "${local.path_prefix}-app-policy"
  
  policy = <<EOT
# Read application secrets
path "secret/data/${local.path_prefix}/*" {
  capabilities = ["read"]
}

# Generate database credentials
path "database/creds/app-role" {
  capabilities = ["read"]
}

# Read own token info
path "auth/token/lookup-self" {
  capabilities = ["read"]
}
EOT
}

# Developer policy for managing secrets
resource "vault_policy" "developer_policy" {
  name = "${local.path_prefix}-developer-policy"
  
  policy = <<EOT
# Manage application secrets
path "secret/data/${local.path_prefix}/*" {
  capabilities = ["create", "read", "update", "delete", "list"]
}

# Manage secret metadata
path "secret/metadata/${local.path_prefix}/*" {
  capabilities = ["create", "read", "update", "delete", "list"]
}

# Generate database credentials
path "database/creds/app-role" {
  capabilities = ["read"]
}

# Read database connection info
path "database/config/*" {
  capabilities = ["read"]
}
EOT
}

# Store application secrets
resource "vault_kv_secret_v2" "app_config" {
  mount = vault_mount.app_secrets.path
  name  = "${local.path_prefix}/config"
  
  data_json = jsonencode({
    database_name = var.database_name
    api_version   = "v1"
    environment   = local.environment
    debug_mode    = true
    log_level     = "debug"
    cors_origin   = "*"
    jwt_issuer    = var.jwt_issuer
  })
}

# Store sensitive configuration
resource "vault_kv_secret_v2" "app_secrets" {
  mount = vault_mount.app_secrets.path
  name  = "${local.path_prefix}/secrets"
  
  data_json = jsonencode({
    jwt_secret     = var.jwt_secret
    api_key        = var.api_key
    encryption_key = var.encryption_key
    webhook_secret = var.webhook_secret
  })
}

# Enable userpass auth for developers (development only)
resource "vault_auth_backend" "userpass" {
  count = var.enable_userpass_auth ? 1 : 0
  
  type = "userpass"
  path = "userpass"
}

# Create developer user
resource "vault_generic_endpoint" "developer_user" {
  count = var.enable_userpass_auth ? 1 : 0
  
  depends_on           = [vault_auth_backend.userpass]
  path                 = "auth/userpass/users/developer"
  ignore_absent_fields = true
  
  data_json = jsonencode({
    policies = [vault_policy.developer_policy.name]
    password = var.developer_password
  })
}

# Enable audit logging to file
resource "vault_audit" "file" {
  count = var.enable_audit_logging ? 1 : 0
  
  type = "file"
  
  options = {
    file_path = "/vault/logs/audit.log"
  }
}
