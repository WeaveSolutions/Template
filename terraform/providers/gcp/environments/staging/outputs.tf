output "network_name" {
  description = "The name of the VPC network"
  value       = module.networking.network_name
}

output "subnet_names" {
  description = "The names of the subnets"
  value       = module.networking.subnet_names
}

output "db_instance_name" {
  description = "The name of the database instance"
  value       = module.database.instance_name
}

output "db_connection_name" {
  description = "The connection name of the database instance"
  value       = module.database.connection_name
}

output "db_database_name" {
  description = "The name of the database"
  value       = module.database.database_name
}

output "storage_bucket_name" {
  description = "The name of the storage bucket"
  value       = module.storage.bucket_name
}

output "storage_bucket_url" {
  description = "The URL of the storage bucket"
  value       = module.storage.bucket_url
}

output "service_url" {
  description = "The URL of the deployed service"
  value       = module.compute.service_url
}

output "service_name" {
  description = "The name of the deployed service"
  value       = module.compute.service_name
}
