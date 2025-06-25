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

variable "custom_domain" {
  description = "Custom domain for CDN"
  type        = string
  default     = ""
}

variable "assets_bucket_name" {
  description = "Name of the assets bucket"
  type        = string
}

variable "cloud_run_service_name" {
  description = "Name of the Cloud Run service"
  type        = string
}

variable "health_check_path" {
  description = "Path for health check"
  type        = string
  default     = "/api/health"
}

variable "enable_cloud_armor" {
  description = "Enable Cloud Armor DDoS protection"
  type        = bool
  default     = true
}
