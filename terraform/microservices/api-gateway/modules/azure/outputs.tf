output "apim_gateway_url" {
  description = "The URL of the Azure API Management"
  value       = "https://${azurerm_api_management.apim.name}.azure-api.net"
}

output "apim_id" {
  description = "The ID of the API Management service"
  value       = azurerm_api_management.apim.id
}

output "apim_developer_portal_url" {
  description = "The Developer Portal URL of the API Management service"
  value       = "https://${azurerm_api_management.apim.name}.developer.azure-api.net"
}

output "apim_management_api_url" {
  description = "The Management API URL of the API Management service"
  value       = "https://${azurerm_api_management.apim.name}.management.azure-api.net"
}

output "apim_scm_url" {
  description = "The SCM URL of the API Management service"
  value       = "https://${azurerm_api_management.apim.name}.scm.azure-api.net"
}

output "apim_gateway_hostname" {
  description = "The hostname of the Gateway component of the API Management service"
  value       = azurerm_api_management.apim.gateway_url
}

output "apim_public_ip_addresses" {
  description = "The Public IP addresses of the Azure API Management service"
  value       = azurerm_api_management.apim.public_ip_addresses
}

output "apim_private_ip_addresses" {
  description = "The Private IP addresses of the Azure API Management service (if integrated with VNET)"
  value       = azurerm_api_management.apim.private_ip_addresses
}

output "app_insights_id" {
  description = "The ID of the Application Insights component (if monitoring is enabled)"
  value       = var.enable_monitoring ? azurerm_application_insights.apim_insights[0].id : null
}

output "app_insights_instrumentation_key" {
  description = "The Instrumentation Key of the Application Insights component (if monitoring is enabled)"
  value       = var.enable_monitoring ? azurerm_application_insights.apim_insights[0].instrumentation_key : null
  sensitive   = true
}

output "key_vault_id" {
  description = "The ID of the Key Vault (if created)"
  value       = var.create_key_vault ? azurerm_key_vault.apim_kv[0].id : null
}
