variable "project_name" {
  description = "Name of the project"
  type        = string
}

variable "environment" {
  description = "Environment name"
  type        = string
}

variable "project_id" {
  description = "GCP Project ID"
  type        = string
}

variable "project_number" {
  description = "GCP Project Number"
  type        = string
}

variable "region" {
  description = "GCP region"
  type        = string
}

variable "organization_id" {
  description = "GCP Organization ID (optional)"
  type        = string
  default     = ""
}

variable "enable_vpc_service_controls" {
  description = "Enable VPC Service Controls (production only)"
  type        = bool
  default     = false
}

variable "access_policy_id" {
  description = "Access Context Manager policy ID"
  type        = string
  default     = ""
}

variable "enable_binary_authorization" {
  description = "Enable Binary Authorization"
  type        = bool
  default     = false
}

variable "enable_web_security_scanner" {
  description = "Enable Web Security Scanner"
  type        = bool
  default     = false
}

variable "app_url" {
  description = "Application URL for security scanning"
  type        = string
  default     = ""
}

variable "scan_auth_user" {
  description = "Username for authenticated security scans"
  type        = string
  default     = ""
}

variable "scan_auth_password" {
  description = "Password for authenticated security scans"
  type        = string
  sensitive   = true
  default     = ""
}

variable "app_secrets" {
  description = "Application secrets to store in Secret Manager"
  type        = map(string)
  sensitive   = true
  default     = {}
}
