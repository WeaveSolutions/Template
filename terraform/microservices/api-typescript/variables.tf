# TypeScript API Service Variables

# Service Configuration
variable "enable_typescript_api" {
  description = "Enable TypeScript API service deployment"
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
  description = "Docker image for TypeScript API"
  type        = string
  default     = "nexpo/api-typescript:latest"
}

variable "desired_count" {
  description = "Number of instances to run"
  type        = number
  default     = 1
}

variable "cpu" {
  description = "CPU units for the container (1024 = 1 vCPU)"
  type        = number
  default     = 256
}

variable "memory" {
  description = "Memory in MB for the container"
  type        = number
  default     = 512
}

# Network Configuration
variable "vpc_id" {
  description = "VPC ID where resources will be created"
  type        = string
}

variable "subnet_ids" {
  description = "List of subnet IDs for the service"
  type        = list(string)
}

variable "security_group_ids" {
  description = "List of security group IDs"
  type        = list(string)
}

variable "cluster_id" {
  description = "ECS cluster ID"
  type        = string
}

variable "cluster_name" {
  description = "ECS cluster name"
  type        = string
}

variable "listener_arn" {
  description = "Load balancer listener ARN"
  type        = string
}

variable "service_discovery_namespace_id" {
  description = "Service discovery namespace ID"
  type        = string
}

# Authentication Configuration
variable "auth0_domain" {
  description = "Auth0 domain"
  type        = string
}

variable "auth0_audience" {
  description = "Auth0 audience"
  type        = string
}

variable "auth0_client_secret_arn" {
  description = "ARN of Auth0 client secret in AWS Secrets Manager"
  type        = string
}

variable "jwt_secret_arn" {
  description = "ARN of JWT secret in AWS Secrets Manager"
  type        = string
}

# Database Configuration
variable "database_url" {
  description = "Database connection URL"
  type        = string
}

variable "database_password_arn" {
  description = "ARN of database password in AWS Secrets Manager"
  type        = string
}

variable "redis_url" {
  description = "Redis connection URL"
  type        = string
  default     = ""
}

# Kong API Gateway Configuration
variable "kong_admin_url" {
  description = "Kong Admin API URL"
  type        = string
  default     = ""
}

variable "kong_proxy_cache_ttl" {
  description = "Kong proxy cache TTL in seconds"
  type        = number
  default     = 300
}

variable "kong_rate_limit_minute" {
  description = "Kong rate limit per minute"
  type        = number
  default     = 100
}

variable "kong_rate_limit_hour" {
  description = "Kong rate limit per hour"
  type        = number
  default     = 1000
}

variable "kong_jwt_validation_enabled" {
  description = "Enable Kong JWT validation plugin"
  type        = bool
  default     = true
}

variable "kong_cors_enabled" {
  description = "Enable Kong CORS plugin"
  type        = bool
  default     = true
}

variable "kong_response_transformer_enabled" {
  description = "Enable Kong response transformer plugin"
  type        = bool
  default     = false
}

variable "kong_circuit_breaker_enabled" {
  description = "Enable Kong circuit breaker plugin"
  type        = bool
  default     = true
}

# Logging Configuration
variable "log_retention_days" {
  description = "CloudWatch log retention period in days"
  type        = number
  default     = 14
}

# Auto Scaling Configuration
variable "enable_auto_scaling" {
  description = "Enable auto scaling for the service"
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
  description = "Target CPU utilization percentage for auto scaling"
  type        = number
  default     = 70
}

variable "memory_target_value" {
  description = "Target memory utilization percentage for auto scaling"
  type        = number
  default     = 80
}

# PostHog Analytics Configuration
variable "enable_posthog" {
  description = "Enable PostHog analytics"
  type        = bool
  default     = true
}

variable "posthog_organization_id" {
  description = "PostHog organization ID"
  type        = string
  default     = ""
}

variable "parent_organization_id" {
  description = "Parent organization ID for PostHog"
  type        = string
  default     = ""
}

variable "posthog_host" {
  description = "PostHog host URL"
  type        = string
  default     = "https://app.posthog.com"
}

variable "parent_project_id" {
  description = "Parent project ID for PostHog"
  type        = string
  default     = ""
}

variable "analytics_rollout_percentage" {
  description = "Percentage of users to enable analytics for"
  type        = number
  default     = 100
}

variable "sampling_rate" {
  description = "Analytics sampling rate (0.0 to 1.0)"
  type        = number
  default     = 1.0
}

variable "data_retention_days" {
  description = "Data retention period in days"
  type        = number
  default     = 30
}

variable "batch_size" {
  description = "Batch size for analytics events"
  type        = number
  default     = 100
}

variable "flush_interval_ms" {
  description = "Flush interval for analytics events in milliseconds"
  type        = number
  default     = 5000
}

variable "enable_console_logs" {
  description = "Enable console logging for analytics"
  type        = bool
  default     = false
}

variable "enable_debug_logging" {
  description = "Enable debug logging for analytics"
  type        = bool
  default     = false
}

variable "enable_performance_monitoring" {
  description = "Enable performance monitoring"
  type        = bool
  default     = true
}

variable "enable_error_tracking" {
  description = "Enable error tracking"
  type        = bool
  default     = true
}

variable "webhook_endpoint" {
  description = "Webhook endpoint for analytics events"
  type        = string
  default     = ""
}

# Additional Environment Variables
variable "additional_environment_variables" {
  description = "Additional environment variables for the container"
  type        = map(string)
  default     = {}
}

variable "additional_secrets" {
  description = "Additional secrets for the container"
  type        = map(string)
  default     = {}
}

# Health Check Configuration
variable "health_check_path" {
  description = "Health check endpoint path"
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
  description = "Number of health check retries"
  type        = number
  default     = 3
}

# Service Discovery Configuration
variable "service_discovery_ttl" {
  description = "TTL for service discovery records"
  type        = number
  default     = 10
}

# Load Balancer Configuration
variable "deregistration_delay" {
  description = "Deregistration delay for target group"
  type        = number
  default     = 300
}

variable "sticky_sessions" {
  description = "Enable sticky sessions"
  type        = bool
  default     = false
}

variable "cookie_duration" {
  description = "Cookie duration for sticky sessions"
  type        = number
  default     = 86400
}

# Monitoring Configuration
variable "enable_detailed_monitoring" {
  description = "Enable detailed CloudWatch monitoring"
  type        = bool
  default     = true
}

variable "alarm_evaluation_periods" {
  description = "Number of periods for alarm evaluation"
  type        = number
  default     = 2
}

variable "alarm_threshold_cpu" {
  description = "CPU alarm threshold percentage"
  type        = number
  default     = 80
}

variable "alarm_threshold_memory" {
  description = "Memory alarm threshold percentage"
  type        = number
  default     = 85
}

# Security Configuration
variable "enable_security_groups" {
  description = "Enable security groups"
  type        = bool
  default     = true
}

variable "allowed_cidr_blocks" {
  description = "CIDR blocks allowed to access the service"
  type        = list(string)
  default     = ["0.0.0.0/0"]
}

variable "enable_waf" {
  description = "Enable WAF protection"
  type        = bool
  default     = false
}

# Backup Configuration
variable "enable_backup" {
  description = "Enable backup for the service"
  type        = bool
  default     = false
}

variable "backup_retention_days" {
  description = "Backup retention period in days"
  type        = number
  default     = 7
}

# Cost Optimization
variable "enable_spot_instances" {
  description = "Enable spot instances for cost optimization"
  type        = bool
  default     = false
}

variable "spot_instance_percentage" {
  description = "Percentage of spot instances to use"
  type        = number
  default     = 50
}

# Tags
variable "tags" {
  description = "Additional tags for resources"
  type        = map(string)
  default     = {}
}
