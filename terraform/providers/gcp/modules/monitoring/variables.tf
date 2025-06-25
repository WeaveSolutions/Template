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

variable "app_domain" {
  description = "Application domain for uptime checks"
  type        = string
}

variable "health_check_path" {
  description = "Path for health check"
  type        = string
  default     = "/api/health"
}

variable "alert_email_endpoints" {
  description = "Email addresses for alerts"
  type        = list(string)
  default     = []
}

variable "alert_sms_endpoints" {
  description = "Phone numbers for SMS alerts (production only)"
  type        = list(string)
  default     = []
}

variable "uptime_check_regions" {
  description = "Regions for uptime checks"
  type        = list(string)
  default     = ["USA", "EUROPE", "ASIA_PACIFIC"]
}

variable "db_connection_threshold" {
  description = "Threshold for database connection alerts"
  type        = number
  default     = 80
}

variable "billing_account_id" {
  description = "GCP Billing Account ID"
  type        = string
}

variable "monthly_budget_amount" {
  description = "Monthly budget amount in USD"
  type        = string
  default     = "1000"
}
