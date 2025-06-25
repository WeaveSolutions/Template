variable "project_name" {
  description = "Name of the project"
  type        = string
}

variable "environment" {
  description = "Environment name"
  type        = string
}

variable "resource_group_name" {
  description = "Name of the resource group"
  type        = string
}

variable "location" {
  description = "Azure region"
  type        = string
}

variable "storage_tier" {
  description = "Storage account tier"
  type        = string
  default     = "Standard"
}

variable "replication_type" {
  description = "Storage replication type"
  type        = string
  default     = "LRS"
}

variable "cors_origins" {
  description = "CORS allowed origins"
  type        = list(string)
  default     = ["*"]
}

variable "soft_delete_retention_days" {
  description = "Soft delete retention in days"
  type        = number
  default     = 7
}

variable "backup_retention_days" {
  description = "Backup retention in days"
  type        = number
  default     = 90
}

variable "enable_versioning" {
  description = "Enable blob versioning"
  type        = bool
  default     = true
}

variable "allowed_ip_rules" {
  description = "Allowed IP addresses"
  type        = list(string)
  default     = []
}

variable "allowed_subnet_ids" {
  description = "Allowed subnet IDs"
  type        = list(string)
  default     = []
}

variable "enable_static_website" {
  description = "Enable static website hosting"
  type        = bool
  default     = false
}

variable "backup_location" {
  description = "Location for backup storage account"
  type        = string
  default     = "westus2"
}
