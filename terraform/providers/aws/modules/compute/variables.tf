# Variables for Compute Module

variable "project_name" {
  description = "Project name for resource naming"
  type        = string
}

variable "environment" {
  description = "Environment name"
  type        = string
}

variable "app_port" {
  description = "Port the application runs on"
  type        = number
  default     = 3000
}

variable "cpu" {
  description = "CPU units for App Runner (0.25, 0.5, 1, 2, 4)"
  type        = string
  default     = "1 vCPU"
}

variable "memory" {
  description = "Memory for App Runner (0.5, 1, 2, 3, 4, 6, 8, 10, 12)"
  type        = string
  default     = "2 GB"
}

variable "min_size" {
  description = "Minimum number of instances"
  type        = number
  default     = 1
}

variable "max_size" {
  description = "Maximum number of instances"
  type        = number
  default     = 10
}

variable "max_concurrency" {
  description = "Maximum concurrent requests per instance"
  type        = number
  default     = 100
}

variable "subnet_ids" {
  description = "Subnet IDs for VPC connector"
  type        = list(string)
}

variable "security_group_ids" {
  description = "Security group IDs for VPC connector"
  type        = list(string)
}

variable "environment_variables" {
  description = "Environment variables for the application"
  type        = map(string)
  default     = {}
}

variable "environment_secrets" {
  description = "Secrets from AWS Secrets Manager"
  type        = map(string)
  default     = {}
}

variable "custom_domain" {
  description = "Custom domain for the application"
  type        = string
  default     = null
}

variable "enable_www_subdomain" {
  description = "Enable www subdomain"
  type        = bool
  default     = true
}

variable "database_url" {
  description = "Database connection URL"
  type        = string
  sensitive   = true
}

variable "api_endpoint" {
  description = "API endpoint URL"
  type        = string
}

variable "auth0_domain" {
  description = "Auth0 domain"
  type        = string
}

variable "auth0_client_id" {
  description = "Auth0 client ID"
  type        = string
}

variable "assets_bucket_arn" {
  description = "ARN of the assets S3 bucket"
  type        = string
}

variable "secrets_arns" {
  description = "ARNs of secrets the app needs access to"
  type        = list(string)
  default     = []
}

variable "common_tags" {
  description = "Common tags to apply to resources"
  type        = map(string)
}

variable "allowed_origins" {
  description = "Allowed origins for CORS"
  type        = list(string)
  default     = ["*"]
}

variable "api_rate_limit" {
  description = "API Gateway rate limit (requests per second)"
  type        = number
  default     = 100
}

variable "api_burst_limit" {
  description = "API Gateway burst limit"
  type        = number
  default     = 200
}

variable "log_retention_days" {
  description = "CloudWatch log retention in days"
  type        = number
  default     = 30
}

variable "kms_key_id" {
  description = "KMS key ID for encryption"
  type        = string
  default     = null
}
