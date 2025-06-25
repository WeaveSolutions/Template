output "db_instance_name" {
  description = "Name of the database instance"
  value       = google_sql_database_instance.main.name
}

output "db_instance_connection_name" {
  description = "Connection name for the database instance"
  value       = google_sql_database_instance.main.connection_name
}

output "db_name" {
  description = "Name of the database"
  value       = google_sql_database.main.name
}

output "db_username" {
  description = "Database username"
  value       = google_sql_user.main.name
}

output "db_password_secret_id" {
  description = "Secret Manager secret ID for database password"
  value       = google_secret_manager_secret.db_password.secret_id
}

output "db_service_account_email" {
  description = "Email of the database service account"
  value       = google_service_account.db_user.email
}

output "firestore_database_name" {
  description = "Name of the Firestore database"
  value       = google_firestore_database.sessions.name
}

output "read_replica_connection_name" {
  description = "Connection name for read replica"
  value       = var.enable_read_replica && var.environment == "prod" ? google_sql_database_instance.read_replica[0].connection_name : null
}
