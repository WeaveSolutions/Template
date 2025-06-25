module "auth0" {
  source = "../../"

  auth0_domain        = var.auth0_domain
  auth0_client_id     = var.auth0_client_id
  auth0_client_secret = var.auth0_client_secret
  
  app_name     = "${var.app_name}-development"
  app_url      = var.app_url
  app_scheme   = var.app_scheme
  api_identifier = var.api_identifier
  
  # Cloud Provider Variables
  gcp_project_id = var.gcp_project_id
  aws_region     = var.aws_region
  azure_tenant_id = var.azure_tenant_id
  oci_tenancy_id = var.oci_tenancy_id
}

# Outputs for development
output "web_client_id" {
  value = module.auth0.web_client_id
  description = "Web Application Client ID"
}

output "mobile_client_id" {
  value = module.auth0.mobile_client_id
  description = "Mobile Application Client ID"
}

output "api_identifier" {
  value = module.auth0.api_identifier
  description = "API Identifier"
}
