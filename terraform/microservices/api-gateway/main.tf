# AWS API Gateway
module "aws_api_gateway" {
  count  = var.enable_aws ? 1 : 0
  source = "./modules/aws"
  
  project_name = var.project_name
  environment  = var.environment
  region       = var.aws_region
  
  # Security
  allowed_origins = var.allowed_origins
  auth0_domain   = var.auth0_domain
  auth0_audience = var.auth0_audience
  
  # Network
  vpc_id            = var.aws_vpc_id
  subnet_ids        = var.aws_subnet_ids
  security_group_id = var.aws_security_group_id
}

# GCP API Gateway
module "gcp_api_gateway" {
  count  = var.enable_gcp ? 1 : 0
  source = "./modules/gcp"
  
  project_id = var.gcp_project_id
  region     = var.gcp_region
  
  # API Configuration
  api_id          = "${var.project_name}-${var.environment}"
  display_name    = "${title(var.project_name)} ${title(var.environment)} API"
  
  # Security
  allowed_ips = var.allowed_ips
  auth0_domain = var.auth0_domain
  auth0_audience = var.auth0_audience
}

# Azure API Management
module "azure_api_management" {
  count  = var.enable_azure ? 1 : 0
  source = "./modules/azure"
  
  # Resource Naming
  name                = "${replace(var.project_name, "-", "")}${var.environment}"
  resource_group_name = var.azurerm_resource_group_name
  location            = var.azurerm_location
  
  # SKU
  sku_name = var.azurerm_sku_name
}

# IBM Cloud API Gateway
module "ibm_api_gateway" {
  count  = var.enable_ibm ? 1 : 0
  source = "./modules/ibm"
  
  # Project Information
  project_name = var.project_name
  environment  = var.environment
  ibm_region   = var.ibm_region
  
  # Authentication
  ibmcloud_api_key = var.ibmcloud_api_key
  
  # API Configuration
  api_client_id     = var.ibm_api_client_id
  api_client_secret = var.ibm_api_client_secret
  
  # Network
  vpc_cidr          = var.ibm_vpc_cidr
  subnet_cidr_blocks = var.ibm_subnet_cidr_blocks
  enable_private_endpoints = var.ibm_enable_private_endpoints
  
  # Security
  allowed_origins = var.allowed_origins
  enable_waf      = var.ibm_enable_waf
  api_domain      = var.ibm_api_domain
  auth0_domain    = var.auth0_domain
  auth0_audience  = var.auth0_audience
}

# OCI API Gateway
module "oci_api_gateway" {
  source = "./modules/oci"
  
  # Only create if OCI API Gateway is enabled
  count = var.enable_oci ? 1 : 0
  
  # OCI Configuration
  compartment_id = var.oci_compartment_ocid
  region         = var.oci_region
  
  # API Gateway Configuration
  display_name   = "${var.project_name}-${var.environment}"
  endpoint_type  = var.oci_endpoint_type
  
  # Security
  enable_auth0_integration = var.oci_enable_auth0_integration
  enable_logging          = var.oci_enable_logging
  enable_tracing          = var.oci_enable_tracing
  enable_waf              = var.oci_enable_waf
  
  # Auth0 Configuration
  auth0_domain   = var.auth0_domain
  auth0_audience = var.auth0_audience
  
  # CORS Configuration
  allowed_origins = var.allowed_origins
  
  # Network Configuration
  subnet_id = var.oci_subnet_ocid
  
  # Feature Flags
  enable_api_versioning = var.feature_api_versioning
  enable_rate_limiting = var.feature_rate_limiting
  
  # Defined Tags
  defined_tags = merge(
    {
      "${var.project_name}.environment" = var.environment
      "${var.project_name}.project"     = var.project_name
      "${var.project_name}.managed-by"  = "terraform"
      "${var.project_name}.component"   = "api-gateway"
      "${var.project_name}.provider"    = "oci"
    }
  )
}

# PostHog Analytics Integration
module "posthog_analytics" {
  source = "./modules/posthog"
  
  enable_posthog          = var.enable_posthog
  project_name           = var.project_name
  environment            = var.environment
  provider               = var.provider
  posthog_organization_id = var.posthog_organization_id
  posthog_host           = var.posthog_host
  
  # Analytics configuration
  analytics_rollout_percentage = var.posthog_analytics_rollout
  sampling_rate               = var.posthog_sampling_rate
  
  # Privacy settings
  anonymize_ips         = var.posthog_anonymize_ips
  data_retention_days   = var.posthog_data_retention_days
  
  # Performance settings
  batch_size        = var.posthog_batch_size
  flush_interval_ms = var.posthog_flush_interval_ms
  
  # Feature toggles
  enable_session_recording     = var.posthog_enable_session_recording
  enable_console_logs         = var.posthog_enable_console_logs
  enable_detailed_logging     = var.posthog_enable_detailed_logging
  enable_api_analytics        = var.posthog_enable_api_analytics
  enable_error_tracking       = var.posthog_enable_error_tracking
  enable_performance_monitoring = var.posthog_enable_performance_monitoring
  
  # Social login analytics
  track_social_login_providers     = var.posthog_track_social_login_providers
  social_login_conversion_tracking = var.posthog_social_login_conversion_tracking
  
  # Business metrics
  enable_business_metrics    = var.posthog_enable_business_metrics
  track_subscription_events  = var.posthog_track_subscription_events
  enable_revenue_tracking    = var.posthog_enable_revenue_tracking
  
  # Webhook configuration
  webhook_endpoint = var.posthog_webhook_endpoint
  webhook_secret   = var.posthog_webhook_secret
  
  common_tags = local.common_tags
}
