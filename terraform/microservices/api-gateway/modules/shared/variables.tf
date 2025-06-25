variable "project_name" {
  description = "Name of the project"
  type        = string
}

variable "environment" {
  description = "Environment (e.g., dev, staging, prod)"
  type        = string
  default     = "dev"
}

variable "tags" {
  description = "A map of tags to add to all resources"
  type        = map(string)
  default     = {}
}

variable "auth0_domain" {
  description = "Auth0 domain for JWT validation"
  type        = string
  default     = ""
}

variable "auth0_audience" {
  description = "Auth0 API audience"
  type        = string
  default     = ""
}

variable "custom_domains" {
  description = "List of custom domain names for the API"
  type        = list(string)
  default     = []
}

variable "enable_monitoring" {
  description = "Enable monitoring and alerting"
  type        = bool
  default     = true
}

variable "alert_emails" {
  description = "List of email addresses for alerts"
  type        = list(string)
  default     = []
}
