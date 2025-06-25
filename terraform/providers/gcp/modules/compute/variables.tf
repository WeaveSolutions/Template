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

variable "container_image" {
  description = "Container image to deploy"
  type        = string
  default     = ""
}

variable "container_port" {
  description = "Container port"
  type        = number
  default     = 3000
}

variable "cpu_limit" {
  description = "CPU limit"
  type        = string
  default     = "1"
}

variable "memory_limit" {
  description = "Memory limit"
  type        = string
  default     = "512Mi"
}

variable "min_instances" {
  description = "Minimum number of instances"
  type        = number
  default     = 0
}

variable "max_instances" {
  description = "Maximum number of instances"
  type        = number
  default     = 100
}

variable "container_concurrency" {
  description = "Maximum concurrent requests per container"
  type        = number
  default     = 80
}

variable "timeout_seconds" {
  description = "Request timeout in seconds"
  type        = number
  default     = 300
}

variable "environment_variables" {
  description = "Environment variables for the container"
  type        = map(string)
  default     = {}
}

variable "vpc_connector_name" {
  description = "VPC connector name"
  type        = string
}

variable "db_connection_name" {
  description = "Cloud SQL connection name"
  type        = string
}

variable "db_name" {
  description = "Database name"
  type        = string
}

variable "db_username" {
  description = "Database username"
  type        = string
}

variable "custom_domain" {
  description = "Custom domain for the service"
  type        = string
  default     = ""
}

variable "allow_unauthenticated" {
  description = "Allow unauthenticated access"
  type        = bool
  default     = true
}

variable "enable_warming" {
  description = "Enable scheduled warming"
  type        = bool
  default     = false
}

variable "enable_ci_cd" {
  description = "Enable Cloud Build CI/CD trigger"
  type        = bool
  default     = false
}

variable "github_owner" {
  description = "GitHub repository owner"
  type        = string
  default     = ""
}

variable "github_repo" {
  description = "GitHub repository name"
  type        = string
  default     = ""
}
