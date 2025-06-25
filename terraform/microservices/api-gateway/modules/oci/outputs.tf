output "api_gateway_url" {
  description = "The URL of the OCI API Gateway"
  value       = oci_apigateway_gateway.api_gateway.hostname
  sensitive   = true
}

output "api_gateway_id" {
  description = "The OCID of the API Gateway"
  value       = oci_apigateway_gateway.api_gateway.id
}

output "api_deployment_id" {
  description = "The OCID of the API Deployment"
  value       = oci_apigateway_deployment.api_deployment.id
}

output "waf_policy_id" {
  description = "The OCID of the WAF Policy (if WAF is enabled)"
  value       = var.enable_waf ? oci_waf_web_app_firewall_policy.waf_policy[0].id : null
}

output "network_security_group_id" {
  description = "The OCID of the Network Security Group (if private endpoints are enabled)"
  value       = var.enable_private_endpoints ? oci_core_network_security_group.api_nsg[0].id : null
}

output "subnet_id" {
  description = "The OCID of the subnet where the API Gateway is deployed"
  value       = var.subnet_ocid
}

output "compartment_id" {
  description = "The OCID of the compartment where resources are deployed"
  value       = var.compartment_ocid
}

output "logging_group_id" {
  description = "The OCID of the logging group for API Gateway logs"
  value       = oci_logging_log_group.api_log_group.id
}

output "api_specification_json" {
  description = "The JSON representation of the API specification"
  value       = oci_apigateway_deployment.api_deployment.specification.json
  sensitive   = true
}
