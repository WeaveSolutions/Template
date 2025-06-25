variable "gcp_region" {
  description = "GCP region to deploy resources"
  type        = string
  default     = "us-central1"
}

variable "gcp_project_id" {
  description = "GCP project ID"
  type        = string
}

variable "api_config_name" {
  description = "Name for the API config"
  type        = string
  default     = "api-config"
}

variable "api_gateway_name" {
  description = "Name for the API Gateway"
  type        = string
  default     = "api-gateway"
}

variable "enable_cloud_armor" {
  description = "Enable Google Cloud Armor for API protection"
  type        = bool
  default     = true
}

variable "allowed_ips" {
  description = "List of allowed IP addresses in CIDR notation"
  type        = list(string)
  default     = []
}

variable "api_logs_retention_days" {
  description = "Number of days to retain API Gateway logs"
  type        = number
  default     = 30
}
