variable "project_name" {
  description = "The name of the project"
  type        = string
}

variable "location" {
  description = "The Azure region to deploy resources"
  type        = string
  default     = "eastus"
}

variable "replica_location" {
  description = "The Azure region for database replicas"
  type        = string
  default     = "westus"
}

variable "db_admin_username" {
  description = "The admin username for the database"
  type        = string
  default     = "sqladmin"
}

variable "db_admin_password" {
  description = "The admin password for the database"
  type        = string
  sensitive   = true
}

variable "enable_app_insights" {
  description = "Enable Application Insights"
  type        = bool
  default     = true
}

variable "enable_cosmos_db" {
  description = "Enable Cosmos DB instead of SQL Server"
  type        = bool
  default     = false
}

variable "enable_azure_ad" {
  description = "Enable Azure AD integration"
  type        = bool
  default     = true
}

variable "alert_email_addresses" {
  description = "Email addresses to send alerts to"
  type        = list(string)
  default     = []
}

variable "enable_cdn" {
  description = "Enable Azure CDN for static content"
  type        = bool
  default     = true
}
