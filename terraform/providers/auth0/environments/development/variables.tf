variable "auth0_domain" {
  description = "Your Auth0 domain (e.g., your-tenant-dev.auth0.com)"
  type        = string
}

variable "auth0_client_id" {
  description = "Auth0 Management API Client ID for development"
  type        = string
  sensitive   = true
}

variable "auth0_client_secret" {
  description = "Auth0 Management API Client Secret for development"
  type        = string
  sensitive   = true
}

variable "app_name" {
  description = "Name of your application in development"
  type        = string
  default     = "Nexpo-development"
}

variable "app_url" {
  description = "Development URL of your application"
  type        = string
  default     = "http://localhost:3000"
}

variable "app_scheme" {
  description = "Custom URL scheme for development mobile app"
  type        = string
  default     = "com.your.app.development"
}

variable "api_identifier" {
  description = "Development API identifier"
  type        = string
  default     = "http://localhost:3001"
}

# Cloud Provider Variables for Development
variable "gcp_project_id" {
  description = "Development GCP Project ID"
  type        = string
  default     = "your-development-project-id"
}

variable "aws_region" {
  description = "AWS Region for development"
  type        = string
  default     = "us-east-1"
}

variable "azure_tenant_id" {
  description = "Azure AD Development Tenant ID"
  type        = string
  default     = ""
}

variable "oci_tenancy_id" {
  description = "Oracle Cloud Infrastructure Development Tenancy ID"
  type        = string
  default     = ""
}
