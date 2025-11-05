# Scala API Service Variables

# Service Configuration
variable "enable_scala_api" {
  description = "Enable Scala API service deployment"
  type        = bool
  default     = true
}

variable "project_name" {
  description = "Name of the project"
  type        = string
  default     = "nexpo"
}

variable "environment" {
  description = "Environment name (dev, staging, prod)"
  type        = string
  default     = "dev"
}

variable "aws_region" {
  description = "AWS region"
  type        = string
  default     = "us-east-1"
}

variable "provider" {
  description = "Cloud provider (aws, gcp, azure)"
  type        = string
  default     = "aws"
}

# Container Configuration
variable "container_image" {
  description = "Docker image for Scala API"
  type        = string
  default     = "nexpo/api-scala:latest"
}

variable "desired_count" {
  description = "Number of instances to run"
  type        = number
  default     = 1
}

variable "cpu" {
  description = "CPU units for the container (1024 = 1 vCPU)"
  type        = number
  default     = 512
}

variable "memory" {
  description = "Memory in MB for the container"
  type        = number
  default     = 1024
}

# Networking
variable "vpc_id" {
  description = "VPC ID"
  type        = string
}

variable "vpc_cidr" {
  description = "VPC CIDR block"
  type        = string
}

variable "private_subnet_ids" {
  description = "List of private subnet IDs"
  type        = list(string)
}

variable "alb_listener_arn" {
  description = "ARN of the ALB listener"
  type        = string
}

variable "listener_priority" {
  description = "Priority for the listener rule"
  type        = number
  default     = 1060
}

# ECS Configuration
variable "ecs_cluster_id" {
  description = "ECS cluster ID"
  type        = string
}

variable "ecs_cluster_name" {
  description = "ECS cluster name"
  type        = string
}

# Auth0 Configuration
variable "auth0_domain" {
  description = "Auth0 domain"
  type        = string
}

variable "auth0_audience" {
  description = "Auth0 API audience"
  type        = string
}

variable "auth0_client_id" {
  description = "Auth0 client ID"
  type        = string
}

variable "auth0_client_secret" {
  description = "Auth0 client secret"
  type        = string
  sensitive   = true
}

# MindsDB Configuration
variable "mindsdb_host" {
  description = "MindsDB host"
  type        = string
  default     = "localhost"
}

variable "mindsdb_port" {
  description = "MindsDB port"
  type        = number
  default     = 47334
}

# Kong Configuration
variable "kong_proxy_url" {
  description = "Kong proxy URL"
  type        = string
  default     = "http://localhost:8000"
}

variable "kong_admin_url" {
  description = "Kong admin URL"
  type        = string
  default     = "http://localhost:8001"
}

variable "enable_kong_integration" {
  description = "Enable Kong integration"
  type        = bool
  default     = true
}

# PostHog Configuration
variable "posthog_api_key" {
  description = "PostHog API key"
  type        = string
  default     = ""
  sensitive   = true
}

variable "posthog_host" {
  description = "PostHog host"
  type        = string
  default     = "https://app.posthog.com"
}

variable "posthog_project_id" {
  description = "PostHog project ID"
  type        = string
  default     = ""
}

variable "posthog_organization_id" {
  description = "PostHog organization ID"
  type        = string
  default     = ""
}

# Scala-specific Configuration
variable "scala_opts" {
  description = "Scala JVM options"
  type        = string
  default     = "-Xmx1g -Xms256m"
}

variable "java_opts" {
  description = "Java JVM options"
  type        = string
  default     = "-Xmx1g -Xms256m -XX:+UseG1GC"
}

variable "play_secret_key" {
  description = "Play Framework secret key"
  type        = string
  sensitive   = true
}

variable "scala_version" {
  description = "Scala version"
  type        = string
  default     = "2.13"
}

variable "play_version" {
  description = "Play Framework version"
  type        = string
  default     = "2.8"
}

# Logging Configuration
variable "log_level" {
  description = "Log level (DEBUG, INFO, WARN, ERROR)"
  type        = string
  default     = "INFO"
}

variable "log_retention_days" {
  description = "Log retention in days"
  type        = number
  default     = 7
}

# Auto Scaling Configuration
variable "enable_auto_scaling" {
  description = "Enable auto scaling"
  type        = bool
  default     = true
}

variable "min_capacity" {
  description = "Minimum number of instances"
  type        = number
  default     = 1
}

variable "max_capacity" {
  description = "Maximum number of instances"
  type        = number
  default     = 10
}

variable "cpu_target_value" {
  description = "Target CPU utilization percentage"
  type        = number
  default     = 70
}

variable "memory_target_value" {
  description = "Target memory utilization percentage"
  type        = number
  default     = 80
}

# Service Discovery Configuration
variable "enable_service_discovery" {
  description = "Enable service discovery"
  type        = bool
  default     = true
}

variable "service_discovery_namespace_id" {
  description = "Service discovery namespace ID"
  type        = string
  default     = ""
}

# Database Configuration
variable "database_host" {
  description = "Database host"
  type        = string
  default     = "localhost"
}

variable "database_port" {
  description = "Database port"
  type        = number
  default     = 5432
}

variable "database_name" {
  description = "Database name"
  type        = string
  default     = "nexpo"
}

variable "database_username" {
  description = "Database username"
  type        = string
  default     = "nexpo_user"
}

variable "database_password" {
  description = "Database password"
  type        = string
  default     = ""
  sensitive   = true
}

# Redis Configuration
variable "redis_host" {
  description = "Redis host"
  type        = string
  default     = "localhost"
}

variable "redis_port" {
  description = "Redis port"
  type        = number
  default     = 6379
}

variable "redis_password" {
  description = "Redis password"
  type        = string
  default     = ""
  sensitive   = true
}

# Health Check Configuration
variable "health_check_enabled" {
  description = "Enable health checks"
  type        = bool
  default     = true
}

variable "health_check_path" {
  description = "Health check path"
  type        = string
  default     = "/health"
}

variable "health_check_interval" {
  description = "Health check interval in seconds"
  type        = number
  default     = 30
}

variable "health_check_timeout" {
  description = "Health check timeout in seconds"
  type        = number
  default     = 5
}

variable "health_check_retries" {
  description = "Health check retries"
  type        = number
  default     = 3
}

# Security Configuration
variable "enable_https" {
  description = "Enable HTTPS"
  type        = bool
  default     = true
}

variable "ssl_certificate_arn" {
  description = "SSL certificate ARN"
  type        = string
  default     = ""
}

# Monitoring Configuration
variable "enable_monitoring" {
  description = "Enable monitoring"
  type        = bool
  default     = true
}

variable "enable_xray" {
  description = "Enable X-Ray tracing"
  type        = bool
  default     = false
}

# Environment Variables
variable "environment_variables" {
  description = "Additional environment variables"
  type        = map(string)
  default     = {}
}

# Tags
variable "tags" {
  description = "Additional tags for resources"
  type        = map(string)
  default     = {}
}
