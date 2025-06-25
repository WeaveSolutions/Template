# Common tags to be used for all resources
locals {
  common_tags = {
    Project     = var.project_name
    Environment = var.environment
    ManagedBy   = "Terraform"
    Module      = "api-gateway"
  }

  # Generate a unique name for resources that require it
  resource_name_prefix = "${var.project_name}-${var.environment}"

  # Default CORS headers
  cors_headers = {
    "Access-Control-Allow-Origin"  = join(", ", var.allowed_origins)
    "Access-Control-Allow-Methods" = "GET, POST, PUT, DELETE, OPTIONS"
    "Access-Control-Allow-Headers" = "Content-Type, Authorization, X-Requested-With"
    "Access-Control-Max-Age"       = "3600"
  }

  # Auth0 configuration for API Gateway
  auth0_config = {
    domain   = var.auth0_domain
    audience = var.auth0_audience
  }

  # Monitoring configuration
  monitoring_config = {
    enabled       = var.enable_monitoring
    alert_emails  = var.alert_emails
    slack_webhook = var.slack_webhook_url
  }

  # API Gateway configuration
  api_config = {
    name        = "${local.resource_name_prefix}-api"
    description = "API Gateway for ${var.project_name} (${var.environment})"
    version     = "v1"
  }

  # Cloud provider specific configurations
  aws_config = {
    region = var.aws_region
  }

  gcp_config = {
    project = var.gcp_project_id
    region  = var.gcp_region
  }

  azure_config = {
    location            = var.azurerm_location
    resource_group_name = var.azurerm_resource_group_name
  }

  oci_config = {
    region         = var.oci_region
    compartment_id = var.oci_compartment_ocid
  }
}
