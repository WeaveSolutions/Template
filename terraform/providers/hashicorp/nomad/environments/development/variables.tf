# HashiCorp Nomad Development Environment Variables

variable "nomad_address" {
  description = "Nomad server address"
  type        = string
  default     = "https://nomad.example.com:4646"
}

variable "nomad_token" {
  description = "Nomad authentication token"
  type        = string
  sensitive   = true
}

variable "nomad_region" {
  description = "Nomad region"
  type        = string
  default     = "global"
}

variable "enable_nextjs_app" {
  description = "Enable Next.js application deployment"
  type        = bool
  default     = true
}

variable "enable_api_service" {
  description = "Enable API service deployment"
  type        = bool
  default     = true
}

variable "enable_background_worker" {
  description = "Enable background worker deployment"
  type        = bool
  default     = false
}

variable "enable_database_backup" {
  description = "Enable database backup job"
  type        = bool
  default     = false
}

variable "create_namespace" {
  description = "Create dedicated namespace for the environment"
  type        = bool
  default     = true
}

variable "enable_quota" {
  description = "Enable resource quota for the namespace"
  type        = bool
  default     = false
}

variable "namespace_quota" {
  description = "Resource quota for the namespace"
  type        = string
  default     = "dev-quota"
}

variable "nextjs_image_tag" {
  description = "Docker image tag for Next.js application"
  type        = string
  default     = "development"
}

variable "api_image_tag" {
  description = "Docker image tag for API service"
  type        = string
  default     = "development"
}

variable "worker_image_tag" {
  description = "Docker image tag for background worker"
  type        = string
  default     = "development"
}

variable "nextjs_port" {
  description = "Port for Next.js application"
  type        = number
  default     = 3000
}

variable "api_port" {
  description = "Port for API service"
  type        = number
  default     = 8000
}

variable "api_url" {
  description = "API URL for Next.js application"
  type        = string
  default     = "http://api.service.consul:8000"
}

variable "database_url" {
  description = "Database connection URL"
  type        = string
  sensitive   = true
  default     = ""
}

variable "redis_url" {
  description = "Redis connection URL"
  type        = string
  sensitive   = true
  default     = ""
}

variable "backup_bucket" {
  description = "S3 bucket for database backups"
  type        = string
  default     = ""
}

variable "backup_schedule" {
  description = "Cron schedule for database backups"
  type        = string
  default     = "0 2 * * *"  # Daily at 2 AM
}
