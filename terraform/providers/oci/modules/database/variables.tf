variable "compartment_ocid" {
  description = "OCI Compartment OCID"
  type        = string
}

variable "project_name" {
  description = "Project name"
  type        = string
}

variable "environment" {
  description = "Environment name"
  type        = string
}

variable "subnet_id" {
  description = "Subnet ID for private endpoint"
  type        = string
  default     = null
}

variable "nsg_id" {
  description = "Network Security Group ID"
  type        = string
  default     = null
}

variable "db_name" {
  description = "Database name (max 14 characters)"
  type        = string
  validation {
    condition     = length(var.db_name) <= 14
    error_message = "Database name must be 14 characters or less."
  }
}

variable "db_display_name" {
  description = "Database display name"
  type        = string
}

variable "db_version" {
  description = "Database version"
  type        = string
  default     = "19c"
}

variable "cpu_core_count" {
  description = "Number of CPU cores"
  type        = number
  default     = 1
}

variable "data_storage_size_gb" {
  description = "Data storage size in GB"
  type        = number
  default     = 20
}

variable "license_model" {
  description = "License model: LICENSE_INCLUDED or BRING_YOUR_OWN_LICENSE"
  type        = string
  default     = "LICENSE_INCLUDED"
}

variable "admin_password" {
  description = "Admin password"
  type        = string
  sensitive   = true
}

variable "app_username" {
  description = "Application username"
  type        = string
  default     = "app_user"
}

variable "wallet_password" {
  description = "Wallet password (auto-generated if not provided)"
  type        = string
  default     = null
  sensitive   = true
}

variable "whitelisted_ips" {
  description = "List of whitelisted IP addresses"
  type        = list(string)
  default     = []
}

variable "enable_auto_scaling" {
  description = "Enable auto-scaling"
  type        = bool
  default     = true
}

variable "enable_regional_wallet" {
  description = "Enable regional wallet management"
  type        = bool
  default     = false
}

variable "create_initial_backup" {
  description = "Create initial backup"
  type        = bool
  default     = true
}

variable "tags" {
  description = "Freeform tags"
  type        = map(string)
  default     = {}
}
