output "api_gateway_id" {
  description = "ID of the IBM API Gateway instance"
  value       = ibm_resource_instance.api_gateway.id
}

output "api_gateway_name" {
  description = "Name of the IBM API Gateway instance"
  value       = ibm_resource_instance.api_gateway.name
}

output "api_gateway_url" {
  description = "URL of the IBM API Gateway endpoint"
  value       = ibm_api_gateway_endpoint.api_gateway_endpoint.url
}

output "api_endpoint_status" {
  description = "Status of the API Gateway endpoint"
  value       = ibm_api_gateway_endpoint.api_gateway_endpoint.endpoint_status
}

output "logging_instance_id" {
  description = "ID of the IBM logging instance"
  value       = ibm_resource_instance.logging.id
}

output "vpc_id" {
  description = "ID of the created VPC"
  value       = ibm_is_vpc.api_gateway_vpc.id
}

output "subnet_ids" {
  description = "IDs of the created subnets"
  value       = ibm_is_subnet.api_gateway_subnet[*].id
}

output "waf_id" {
  description = "ID of the Cloud Internet Services (WAF)"
  value       = var.enable_waf ? ibm_cis.api_waf[0].id : null
}

output "key_protect_id" {
  description = "ID of the Key Protect instance"
  value       = ibm_resource_instance.key_protect.id
}

output "root_key_id" {
  description = "ID of the root key in Key Protect"
  value       = ibm_kms_key.api_key.id
}
