# Outputs from Database Module

output "db_instance_id" {
  description = "ID of the RDS instance"
  value       = aws_db_instance.main.id
}

output "db_instance_endpoint" {
  description = "Connection endpoint for the RDS instance"
  value       = aws_db_instance.main.endpoint
}

output "db_instance_address" {
  description = "Address of the RDS instance"
  value       = aws_db_instance.main.address
}

output "db_instance_port" {
  description = "Port of the RDS instance"
  value       = aws_db_instance.main.port
}

output "db_name" {
  description = "Name of the database"
  value       = aws_db_instance.main.db_name
}

output "db_credentials_secret_arn" {
  description = "ARN of the secret containing database credentials"
  value       = aws_secretsmanager_secret.db_credentials.arn
}

output "db_credentials_secret_name" {
  description = "Name of the secret containing database credentials"
  value       = aws_secretsmanager_secret.db_credentials.name
}

output "database_url" {
  description = "Full database connection URL"
  value       = "postgresql://${aws_db_instance.main.username}:${nonsensitive(random_password.db_password.result)}@${aws_db_instance.main.address}:${aws_db_instance.main.port}/${aws_db_instance.main.db_name}"
  sensitive   = true
}

output "sessions_table_name" {
  description = "Name of the DynamoDB sessions table"
  value       = aws_dynamodb_table.sessions.name
}

output "sessions_table_arn" {
  description = "ARN of the DynamoDB sessions table"
  value       = aws_dynamodb_table.sessions.arn
}

output "cache_table_name" {
  description = "Name of the DynamoDB cache table"
  value       = aws_dynamodb_table.cache.name
}

output "cache_table_arn" {
  description = "ARN of the DynamoDB cache table"
  value       = aws_dynamodb_table.cache.arn
}

output "read_replica_endpoint" {
  description = "The connection endpoint for the read replica"
  value       = var.enable_read_replica ? aws_db_instance.read_replica[0].endpoint : null
}
