variable "project_name" {
  description = "The name of the project"
  type        = string
}

variable "location" {
  description = "The Azure region to deploy resources"
  type        = string
  default     = "eastus"
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
  default     = false
}
