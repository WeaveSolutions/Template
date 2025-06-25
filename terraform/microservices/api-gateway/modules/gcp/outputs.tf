output "api_gateway_url" {
  description = "The URL of the GCP API Gateway"
  value       = google_api_gateway_gateway.api_gw.default_hostname
}

output "api_gateway_id" {
  description = "The ID of the GCP API Gateway"
  value       = google_api_gateway_gateway.api_gw.id
}

output "api_config_id" {
  description = "The ID of the API config"
  value       = google_api_gateway_api_config.api_config.id
}

output "api_gateway_name" {
  description = "The name of the API Gateway"
  value       = google_api_gateway_gateway.api_gw.name
}

output "api_id" {
  description = "The ID of the API"
  value       = google_api_gateway_api.api.id
}

output "security_policy_id" {
  description = "The ID of the Cloud Armor security policy (if WAF is enabled)"
  value       = var.enable_waf ? google_compute_security_policy.api_security_policy[0].id : null
}

output "vpc_connector_id" {
  description = "The ID of the VPC connector (if private endpoints are enabled)"
  value       = var.enable_private_endpoints ? google_vpc_access_connector.connector[0].id : null
}

output "service_account_email" {
  description = "The email of the service account created for the API Gateway"
  value       = google_service_account.api_gateway_sa.email
}
