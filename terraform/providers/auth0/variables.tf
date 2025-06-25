variable "auth0_domain" {
  description = "Your Auth0 domain (e.g., your-tenant.auth0.com)"
  type        = string
}

variable "auth0_client_id" {
  description = "Auth0 Management API Client ID"
  type        = string
  sensitive   = true
}

variable "auth0_client_secret" {
  description = "Auth0 Management API Client Secret"
  type        = string
  sensitive   = true
}

variable "app_name" {
  description = "Name of your application"
  type        = string
  default     = "Nexpo"
}

variable "app_url" {
  description = "Base URL of your application"
  type        = string
  default     = "http://localhost:3000"
}

variable "app_scheme" {
  description = "Custom URL scheme for mobile app"
  type        = string
  default     = "com.your.app"
}

variable "api_identifier" {
  description = "Unique identifier for your API"
  type        = string
  default     = "https://api.yourdomain.com"
}

# Cloud Provider Variables
variable "gcp_project_id" {
  description = "GCP Project ID for GCP integration"
  type        = string
  default     = ""
}

variable "aws_region" {
  description = "AWS Region for AWS integration"
  type        = string
  default     = "us-east-1"
}

variable "azure_tenant_id" {
  description = "Azure AD Tenant ID"
  type        = string
  default     = ""
}

variable "oci_tenancy_id" {
  description = "Oracle Cloud Infrastructure Tenancy ID"
  type        = string
  default     = ""
}
