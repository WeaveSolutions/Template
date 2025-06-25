variable "project_name" {
  description = "The name of the project"
  type        = string
}

variable "location" {
  description = "Azure region to deploy resources"
  type        = string
  default     = "eastus2"
}

variable "db_admin_username" {
  description = "The admin username for the database"
  type        = string
  sensitive   = true
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

variable "environment" {
  description = "Environment name"
  type        = string
  default     = "staging"
}
