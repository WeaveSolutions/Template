# R API Service Variables

# Service Configuration
variable "enable_r_api" {
  description = "Enable R API service deployment"
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
  description = "Docker image for R API"
  type        = string
  default     = "nexpo/api-r:latest"
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
  default     = 1080
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

# R-specific Configuration
variable "r_version" {
  description = "R version"
  type        = string
  default     = "4.3.0"
}

variable "r_libs_user" {
  description = "R user library path"
  type        = string
  default     = "/home/rstudio/R/library"
}

variable "r_keep_pkg_source" {
  description = "Keep package source"
  type        = string
  default     = "yes"
}

variable "r_compile_and_install_packages" {
  description = "Compile and install packages"
  type        = string
  default     = "yes"
}

variable "plumber_host" {
  description = "Plumber host"
  type        = string
  default     = "0.0.0.0"
}

variable "plumber_port" {
  description = "Plumber port"
  type        = number
  default     = 8080
}

variable "r_max_connections" {
  description = "Maximum number of R connections"
  type        = number
  default     = 100
}

variable "r_timeout" {
  description = "R timeout in seconds"
  type        = number
  default     = 30
}

variable "r_memory_limit" {
  description = "R memory limit in MB"
  type        = number
  default     = 1024
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

# R Package Configuration
variable "r_packages" {
  description = "List of R packages to install"
  type        = list(string)
  default     = [
    "plumber",
    "jsonlite",
    "httr",
    "dplyr",
    "ggplot2",
    "lubridate",
    "stringr",
    "tidyr",
    "readr",
    "purrr"
  ]
}

variable "r_cran_mirror" {
  description = "CRAN mirror URL"
  type        = string
  default     = "https://cran.rstudio.com/"
}

variable "r_bioconductor_mirror" {
  description = "Bioconductor mirror URL"
  type        = string
  default     = "https://bioconductor.org/packages/release/bioc"
}

# Performance Configuration
variable "r_gc_mem_grow" {
  description = "R garbage collection memory growth"
  type        = number
  default     = 3
}

variable "r_gc_mem_grow_mb" {
  description = "R garbage collection memory growth in MB"
  type        = number
  default     = 256
}

variable "r_max_vsize" {
  description = "R maximum virtual size"
  type        = string
  default     = "2G"
}

variable "r_max_nsize" {
  description = "R maximum number of cons cells"
  type        = string
  default     = "50M"
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
