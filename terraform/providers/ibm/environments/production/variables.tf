variable "project_name" {
  description = "The name of the project"
  type        = string
}

variable "region" {
  description = "The IBM Cloud region to deploy resources"
  type        = string
  default     = "us-south"
}

variable "db_admin_password" {
  description = "The admin password for the database"
  type        = string
  sensitive   = true
}

variable "enable_auth0" {
  description = "Enable Auth0 integration"
  type        = bool
  default     = true
}

variable "enable_cloudant" {
  description = "Enable Cloudant NoSQL database"
  type        = bool
  default     = false
}

variable "enable_power_vs" {
  description = "Enable Power Virtual Server"
  type        = bool
  default     = false
}

variable "alert_recipients" {
  description = "Email addresses for monitoring alerts"
  type        = list(string)
  default     = []
}
