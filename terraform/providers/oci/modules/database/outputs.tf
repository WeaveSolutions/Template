output "database_id" {
  description = "Autonomous Database OCID"
  value       = oci_database_autonomous_database.main.id
}

output "database_name" {
  description = "Database name"
  value       = var.db_name
}

output "connection_urls" {
  description = "Database connection URLs"
  value       = local.connection_urls
}

output "connection_string" {
  description = "Database connection string"
  value       = local.connection_string
  sensitive   = true
}

output "jdbc_url" {
  description = "JDBC connection URL"
  value       = local.jdbc_url
  sensitive   = true
}

output "private_endpoint" {
  description = "Private endpoint hostname"
  value       = oci_database_autonomous_database.main.private_endpoint
}

output "wallet_location" {
  description = "Local wallet location"
  value       = abspath("${path.module}/wallet_${var.environment}")
}

output "db_config_file" {
  description = "Database configuration file path"
  value       = local_file.db_config.filename
}

output "admin_username" {
  description = "Database admin username"
  value       = "ADMIN"
}

output "app_username" {
  description = "Application username"
  value       = var.app_username
}

output "connection_url" {
  description = "Full database connection URL for applications"
  value       = "oracle://${var.app_username}:${var.admin_password}@${local.connection_string}"
  sensitive   = true
}

output "backup_id" {
  description = "Initial backup OCID"
  value       = var.create_initial_backup ? oci_database_autonomous_database_backup.manual[0].id : null
}
