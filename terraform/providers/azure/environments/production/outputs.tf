output "resource_group_name" {
  description = "The name of the resource group"
  value       = module.networking.resource_group_name
}

output "resource_group_id" {
  description = "The ID of the resource group"
  value       = module.networking.resource_group_id
}

output "virtual_network_name" {
  description = "The name of the virtual network"
  value       = module.networking.virtual_network_name
}

output "virtual_network_id" {
  description = "The ID of the virtual network"
  value       = module.networking.virtual_network_id
}

output "subnet_ids" {
  description = "The IDs of the subnets"
  value       = module.networking.subnet_ids
}

output "database_server_name" {
  description = "The name of the database server"
  value       = module.database.server_name
}

output "database_server_id" {
  description = "The ID of the database server"
  value       = module.database.server_id
}

output "database_connection_string" {
  description = "The connection string for the database"
  value       = module.database.connection_string
  sensitive   = true
}

output "database_replica_id" {
  description = "The ID of the database replica"
  value       = module.database.replica_id
}

output "storage_account_name" {
  description = "The name of the storage account"
  value       = module.storage.account_name
}

output "storage_account_id" {
  description = "The ID of the storage account"
  value       = module.storage.account_id
}

output "storage_primary_connection_string" {
  description = "The primary connection string for the storage account"
  value       = module.storage.primary_connection_string
  sensitive   = true
}

output "app_service_plan_id" {
  description = "The ID of the App Service Plan"
  value       = module.compute.app_service_plan_id
}

output "app_service_id" {
  description = "The ID of the App Service"
  value       = module.compute.app_service_id
}

output "app_service_default_site_hostname" {
  description = "The default hostname of the App Service"
  value       = module.compute.app_service_default_site_hostname
}

output "app_service_slot_ids" {
  description = "The IDs of the App Service deployment slots"
  value       = module.compute.app_service_slot_ids
}

output "app_insights_id" {
  description = "The ID of Application Insights"
  value       = module.monitoring.app_insights_id
}

output "app_insights_instrumentation_key" {
  description = "The instrumentation key for Application Insights"
  value       = module.monitoring.app_insights_instrumentation_key
  sensitive   = true
}

output "log_analytics_workspace_id" {
  description = "The ID of the Log Analytics workspace"
  value       = module.monitoring.log_analytics_workspace_id
}

output "key_vault_id" {
  description = "The ID of the Key Vault"
  value       = module.security.key_vault_id
}

output "key_vault_uri" {
  description = "The URI of the Key Vault"
  value       = module.security.key_vault_uri
}

output "private_endpoints" {
  description = "The IDs of the private endpoints"
  value       = module.security.private_endpoint_ids
}
