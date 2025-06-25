# General Outputs
output "project_name" {
  description = "The name of the project"
  value       = var.project_name
}

output "environment" {
  description = "The environment name"
  value       = var.environment
}

# AWS Outputs
output "aws_api_gateway_url" {
  description = "The URL of the AWS API Gateway (if enabled)"
  value       = var.enable_aws ? module.aws_api_gateway[0].api_gateway_url : null
}

output "aws_vpc_id" {
  description = "The ID of the VPC (if AWS is enabled)"
  value       = var.enable_aws ? module.aws_api_gateway[0].vpc_id : null
}

output "aws_private_subnets" {
  description = "List of private subnet IDs (if AWS is enabled)"
  value       = var.enable_aws ? module.aws_api_gateway[0].private_subnets : []
}

output "aws_public_subnets" {
  description = "List of public subnet IDs (if AWS is enabled)"
  value       = var.enable_aws ? module.aws_api_gateway[0].public_subnets : []
}

output "aws_app_runner_role_arn" {
  description = "The ARN of the IAM role for App Runner (if AWS is enabled)"
  value       = var.enable_aws ? module.aws_api_gateway[0].app_runner_role_arn : null
}

output "aws_api_gateway_id" {
  description = "The ID of the API Gateway (if AWS is enabled)"
  value       = var.enable_aws ? module.aws_api_gateway[0].api_gateway_id : null
}

output "aws_cloudwatch_log_group_arn" {
  description = "The ARN of the CloudWatch log group (if AWS is enabled)"
  value       = var.enable_aws ? module.aws_api_gateway[0].cloudwatch_log_group_arn : null
}

output "aws_api_gateway_sg_id" {
  description = "The ID of the API Gateway security group (if AWS is enabled)"
  value       = var.enable_aws ? module.aws_api_gateway[0].api_gateway_sg_id : null
}

output "aws_app_runner_sg_id" {
  description = "The ID of the App Runner security group (if AWS is enabled)"
  value       = var.enable_aws ? module.aws_api_gateway[0].app_runner_sg_id : null
}

output "aws_vpc_arn" {
  description = "The ARN of the VPC (if AWS is enabled)"
  value       = var.enable_aws ? module.aws_api_gateway[0].vpc_arn : null
}

output "aws_app_runner_vpc_connector_arn" {
  description = "The ARN of the App Runner VPC connector (if AWS is enabled)"
  value       = var.enable_aws ? module.aws_api_gateway[0].app_runner_vpc_connector_arn : null
}

# GCP Outputs
output "gcp_api_gateway_url" {
  description = "The URL of the GCP API Gateway (if enabled)"
  value       = var.enable_gcp ? module.gcp_api_gateway[0].api_gateway_url : null
}

output "gcp_api_gateway_id" {
  description = "The ID of the GCP API Gateway (if enabled)"
  value       = var.enable_gcp ? module.gcp_api_gateway[0].api_gateway_id : null
}

output "gcp_api_config_id" {
  description = "The ID of the API config (if GCP is enabled)"
  value       = var.enable_gcp ? module.gcp_api_gateway[0].api_config_id : null
}

output "gcp_service_account_email" {
  description = "The email of the service account created for the API Gateway (if GCP is enabled)"
  value       = var.enable_gcp ? module.gcp_api_gateway[0].service_account_email : null
}

output "gcp_security_policy_id" {
  description = "The ID of the Cloud Armor security policy (if GCP is enabled and WAF is enabled)"
  value       = var.enable_gcp ? module.gcp_api_gateway[0].security_policy_id : null
}

# Azure Outputs
output "azure_api_gateway_url" {
  description = "The URL of the Azure API Management (if enabled)"
  value       = var.enable_azure ? module.azure_api_management[0].apim_gateway_url : null
}

output "azure_apim_id" {
  description = "The ID of the API Management service (if Azure is enabled)"
  value       = var.enable_azure ? module.azure_api_management[0].apim_id : null
}

output "azure_apim_developer_portal_url" {
  description = "The Developer Portal URL of the API Management service (if Azure is enabled)"
  value       = var.enable_azure ? module.azure_api_management[0].apim_developer_portal_url : null
}

output "azure_apim_management_api_url" {
  description = "The Management API URL of the API Management service (if Azure is enabled)"
  value       = var.enable_azure ? module.azure_api_management[0].apim_management_api_url : null
}

output "azure_app_insights_id" {
  description = "The ID of the Application Insights component (if Azure is enabled and monitoring is enabled)"
  value       = var.enable_azure ? module.azure_api_management[0].app_insights_id : null
}

# OCI Outputs
output "oci_api_gateway_url" {
  description = "The URL of the OCI API Gateway (if enabled)"
  value       = var.enable_oci ? module.oci_api_gateway[0].api_gateway_url : null
  sensitive   = true
}

output "oci_api_gateway_id" {
  description = "The OCID of the API Gateway (if OCI is enabled)"
  value       = var.enable_oci ? module.oci_api_gateway[0].api_gateway_id : null
}

output "oci_api_deployment_id" {
  description = "The OCID of the API Deployment (if OCI is enabled)"
  value       = var.enable_oci ? module.oci_api_gateway[0].api_deployment_id : null
}

output "oci_waf_policy_id" {
  description = "The OCID of the WAF Policy (if OCI is enabled and WAF is enabled)"
  value       = var.enable_oci && var.oci_enable_waf ? module.oci_api_gateway[0].waf_policy_id : null
}

# IBM Cloud Outputs
output "ibm_api_gateway_url" {
  description = "The URL of the IBM API Gateway (if enabled)"
  value       = var.enable_ibm ? module.ibm_api_gateway[0].api_gateway_url : null
}

output "ibm_api_gateway_id" {
  description = "ID of the IBM API Gateway instance (if enabled)"
  value       = var.enable_ibm ? module.ibm_api_gateway[0].api_gateway_id : null
}

output "ibm_vpc_id" {
  description = "ID of the created VPC in IBM Cloud (if enabled)"
  value       = var.enable_ibm ? module.ibm_api_gateway[0].vpc_id : null
}

output "ibm_waf_id" {
  description = "ID of the IBM Cloud Internet Services (WAF) (if enabled and WAF is enabled)"
  value       = var.enable_ibm ? module.ibm_api_gateway[0].waf_id : null
}

# PostHog Analytics Outputs
output "posthog_project_id" {
  description = "PostHog project ID for API Gateway analytics"
  value       = var.enable_posthog ? module.posthog_analytics.posthog_project_id : null
}

output "posthog_api_key" {
  description = "PostHog API key for application integration"
  value       = var.enable_posthog ? module.posthog_analytics.posthog_project_api_key : null
  sensitive   = true
}

output "posthog_dashboard_url" {
  description = "PostHog analytics dashboard URL"
  value       = var.enable_posthog ? module.posthog_analytics.dashboard_url : null
}

output "posthog_feature_flags" {
  description = "PostHog feature flags for the API Gateway"
  value       = var.enable_posthog ? module.posthog_analytics.feature_flags : {}
}

output "posthog_environment_variables" {
  description = "Environment variables for PostHog integration"
  value       = var.enable_posthog ? module.posthog_analytics.environment_variables : {}
  sensitive   = true
}

# Common Security Outputs
output "allowed_origins" {
  description = "List of allowed CORS origins"
  value       = var.allowed_origins
  sensitive   = true
}

output "auth0_domain" {
  description = "The Auth0 domain used for authentication"
  value       = var.auth0_domain
  sensitive   = true
}

output "auth0_audience" {
  description = "The Auth0 audience used for JWT validation"
  value       = var.auth0_audience
  sensitive   = true
}
