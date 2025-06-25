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

variable "region" {
  description = "GCP region"
  type        = string
}

variable "storage_class" {
  description = "Storage class for buckets"
  type        = string
  default     = "STANDARD"
}

variable "cors_origins" {
  description = "CORS allowed origins"
  type        = list(string)
  default     = ["*"]
}

variable "backup_retention_days" {
  description = "Number of days to retain backups"
  type        = number
  default     = 30
}

variable "create_state_bucket" {
  description = "Whether to create a bucket for Terraform state"
  type        = bool
  default     = false
}
