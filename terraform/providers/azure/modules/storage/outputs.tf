output "storage_account_name" {
  description = "Name of the storage account"
  value       = azurerm_storage_account.main.name
}

output "storage_account_id" {
  description = "ID of the storage account"
  value       = azurerm_storage_account.main.id
}

output "storage_primary_endpoint" {
  description = "Primary blob endpoint"
  value       = azurerm_storage_account.main.primary_blob_endpoint
}

output "storage_primary_access_key" {
  description = "Primary access key"
  value       = azurerm_storage_account.main.primary_access_key
  sensitive   = true
}

output "storage_connection_string" {
  description = "Storage connection string"
  value       = azurerm_storage_account.main.primary_connection_string
  sensitive   = true
}

output "container_names" {
  description = "Names of storage containers"
  value = {
    assets  = azurerm_storage_container.assets.name
    uploads = azurerm_storage_container.uploads.name
    backups = azurerm_storage_container.backups.name
  }
}

output "static_website_url" {
  description = "Static website URL"
  value       = var.enable_static_website ? azurerm_storage_account.main.primary_web_endpoint : null
}

output "queue_names" {
  description = "Names of storage queues"
  value = {
    notifications = azurerm_storage_queue.notifications.name
    jobs         = azurerm_storage_queue.jobs.name
  }
}

output "table_names" {
  description = "Names of storage tables"
  value = {
    sessions = azurerm_storage_table.sessions.name
    cache    = azurerm_storage_table.cache.name
  }
}
