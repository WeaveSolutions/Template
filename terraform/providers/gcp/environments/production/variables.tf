variable "project_name" {
  description = "Name of the project"
  type        = string
  default     = "Nexpo"
}

variable "environment" {
  description = "Environment name"
  type        = string
  default     = "production"
}

variable "project_id" {
  description = "GCP Project ID"
  type        = string
}

variable "project_number" {
  description = "GCP Project Number"
  type        = string
}

variable "billing_account_id" {
  description = "GCP Billing Account ID"
  type        = string
}

variable "organization_id" {
  description = "GCP Organization ID"
  type        = string
  default     = ""
}

variable "region" {
  description = "GCP region"
  type        = string
  default     = "us-central1"
}

variable "custom_domain" {
  description = "Custom domain name"
  type        = string
}

variable "supabase_url" {
  description = "Supabase URL"
  type        = string
}

variable "supabase_anon_key" {
  description = "Supabase anonymous key"
  type        = string
}

variable "supabase_service_key" {
  description = "Supabase service key"
  type        = string
  sensitive   = true
}

variable "api_key" {
  description = "API key for external services"
  type        = string
  sensitive   = true
}

variable "alert_email_endpoints" {
  description = "Email addresses for alerts"
  type        = list(string)
}

variable "alert_sms_endpoints" {
  description = "Phone numbers for SMS alerts"
  type        = list(string)
  default     = []
}

variable "access_policy_id" {
  description = "Access Context Manager policy ID"
  type        = string
  default     = ""
}

variable "scan_auth_user" {
  description = "Username for authenticated security scans"
  type        = string
  default     = "security-scanner@example.com"
}

variable "scan_auth_password" {
  description = "Password for authenticated security scans"
  type        = string
  sensitive   = true
}
