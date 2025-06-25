# HashiCorp Vault Development Environment Outputs

output "vault_address" {
  description = "Vault server address"
  value       = var.vault_address
}

output "app_secrets_mount_path" {
  description = "Mount path for application secrets"
  value       = vault_mount.app_secrets.path
}

output "database_mount_path" {
  description = "Mount path for database secrets engine"
  value       = var.enable_database_secrets ? vault_mount.database[0].path : null
}

output "app_policy_name" {
  description = "Name of the application policy"
  value       = vault_policy.app_policy.name
}

output "developer_policy_name" {
  description = "Name of the developer policy"
  value       = vault_policy.developer_policy.name
}

output "jwt_auth_path" {
  description = "JWT authentication path"
  value       = var.enable_jwt_auth ? vault_auth_backend.jwt[0].path : null
}

output "userpass_auth_path" {
  description = "Userpass authentication path"
  value       = var.enable_userpass_auth ? vault_auth_backend.userpass[0].path : null
}

output "app_config_secret_path" {
  description = "Path to application configuration secrets"
  value       = "${vault_mount.app_secrets.path}/data/${local.path_prefix}/config"
}

output "app_secrets_secret_path" {
  description = "Path to application sensitive secrets"
  value       = "${vault_mount.app_secrets.path}/data/${local.path_prefix}/secrets"
}

output "database_role_name" {
  description = "Database role name for dynamic credentials"
  value       = var.enable_database_secrets ? vault_database_secret_backend_role.app_role[0].name : null
}

output "database_credentials_path" {
  description = "Path to generate database credentials"
  value       = var.enable_database_secrets ? "${vault_mount.database[0].path}/creds/${vault_database_secret_backend_role.app_role[0].name}" : null
}

output "jwt_role_name" {
  description = "JWT authentication role name"
  value       = var.enable_jwt_auth ? vault_jwt_auth_backend_role.app_role[0].role_name : null
}

output "environment_info" {
  description = "Development environment information"
  value = {
    environment = local.environment
    project     = local.project_name
    path_prefix = local.path_prefix
    features = {
      database_secrets = var.enable_database_secrets
      jwt_auth        = var.enable_jwt_auth
      userpass_auth   = var.enable_userpass_auth
      audit_logging   = var.enable_audit_logging
    }
    auth_methods = {
      jwt      = var.enable_jwt_auth ? vault_auth_backend.jwt[0].path : null
      userpass = var.enable_userpass_auth ? vault_auth_backend.userpass[0].path : null
    }
    secret_engines = {
      kv       = vault_mount.app_secrets.path
      database = var.enable_database_secrets ? vault_mount.database[0].path : null
    }
  }
}
