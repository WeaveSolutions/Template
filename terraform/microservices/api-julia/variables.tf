# Julia API Service Variables

# Service Configuration
variable "enable_julia_api" {
  description = "Enable Julia API service deployment"
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
  description = "Docker image for Julia API"
  type        = string
  default     = "nexpo/api-julia:latest"
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
  default     = 2048
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
  default     = 1090
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

# Julia-specific Configuration
variable "julia_version" {
  description = "Julia version"
  type        = string
  default     = "1.9.2"
}

variable "julia_num_threads" {
  description = "Number of Julia threads"
  type        = number
  default     = 4
}

variable "julia_depot_path" {
  description = "Julia depot path"
  type        = string
  default     = "/home/julia/.julia"
}

variable "julia_load_path" {
  description = "Julia load path"
  type        = string
  default     = "@:@v#.#:@stdlib"
}

variable "julia_project" {
  description = "Julia project path"
  type        = string
  default     = "/app"
}

variable "genie_env" {
  description = "Genie environment"
  type        = string
  default     = "prod"
}

variable "genie_host" {
  description = "Genie host"
  type        = string
  default     = "0.0.0.0"
}

variable "genie_port" {
  description = "Genie port"
  type        = number
  default     = 8090
}

variable "julia_optimization_level" {
  description = "Julia optimization level (0-3)"
  type        = number
  default     = 2
}

variable "julia_gc_threads" {
  description = "Number of Julia GC threads"
  type        = number
  default     = 2
}

variable "julia_heap_size_hint" {
  description = "Julia heap size hint (e.g., '1G', '2G')"
  type        = string
  default     = "1G"
}

# Package Configuration
variable "julia_packages" {
  description = "List of Julia packages to install"
  type        = list(string)
  default     = [
    "Genie",
    "JSON3",
    "HTTP",
    "DataFrames",
    "CSV",
    "Dates",
    "Logging",
    "UUIDs",
    "Base64",
    "Pkg"
  ]
}

variable "julia_registry_url" {
  description = "Julia registry URL"
  type        = string
  default     = "https://github.com/JuliaRegistries/General.git"
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

# Performance Configuration
variable "julia_compile_cache" {
  description = "Enable Julia compile cache"
  type        = bool
  default     = true
}

variable "julia_precompile_packages" {
  description = "Precompile Julia packages"
  type        = bool
  default     = true
}

variable "julia_startup_file" {
  description = "Julia startup file path"
  type        = string
  default     = ""
}

variable "julia_sysimage" {
  description = "Julia system image path"
  type        = string
  default     = ""
}

variable "julia_inline" {
  description = "Julia inline optimization"
  type        = bool
  default     = true
}

variable "julia_check_bounds" {
  description = "Julia bounds checking"
  type        = string
  default     = "auto"
  validation {
    condition = contains(["yes", "no", "auto"], var.julia_check_bounds)
    error_message = "Julia check bounds must be 'yes', 'no', or 'auto'."
  }
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
