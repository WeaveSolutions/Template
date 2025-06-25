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

variable "vpc_id" {
  description = "VPC network ID"
  type        = string
}

variable "db_tier" {
  description = "Machine type for database instance"
  type        = string
  default     = "db-f1-micro"
}

variable "db_disk_size" {
  description = "Disk size in GB"
  type        = number
  default     = 10
}

variable "db_username" {
  description = "Database username"
  type        = string
  default     = "app_user"
}

variable "high_availability" {
  description = "Enable high availability"
  type        = bool
  default     = false
}

variable "enable_read_replica" {
  description = "Enable read replica"
  type        = bool
  default     = false
}

variable "replica_region" {
  description = "Region for read replica"
  type        = string
  default     = ""
}

variable "max_connections" {
  description = "Maximum number of connections"
  type        = string
  default     = "100"
}

variable "firestore_location" {
  description = "Location for Firestore database"
  type        = string
  default     = "nam5"  # Multi-region US
}
