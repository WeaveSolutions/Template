output "resource_group_name" {
  description = "The name of the resource group"
  value       = "${var.project_name}-${var.environment}-rg"
}

output "app_service_name" {
  description = "The name of the app service"
  value       = module.compute.app_service_name
}

output "app_service_url" {
  description = "The URL of the app service"
  value       = module.compute.app_service_url
}

output "database_server_name" {
  description = "The name of the database server"
  value       = module.database.server_name
}

output "database_name" {
  description = "The name of the database"
  value       = module.database.database_name
}

output "storage_account_name" {
  description = "The name of the storage account"
  value       = module.storage.storage_account_name
}

output "storage_account_primary_blob_endpoint" {
  description = "The primary blob endpoint of the storage account"
  value       = module.storage.storage_account_primary_blob_endpoint
}

output "virtual_network_name" {
  description = "The name of the virtual network"
  value       = module.networking.virtual_network_name
}

output "subnet_ids" {
  description = "Map of subnet names and IDs"
  value       = module.networking.subnet_ids
}
