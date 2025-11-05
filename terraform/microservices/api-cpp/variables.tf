# Variables for C++ Drogon API Service

# Service Configuration
variable "enable_cpp_api" {
  description = "Enable C++ Drogon API service"
  type        = bool
  default     = false
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

# Container Configuration
variable "container_image" {
  description = "Docker image for the C++ API service"
  type        = string
  default     = "nexpo/api-cpp:latest"
}

variable "cpu" {
  description = "CPU units for the container (1024 = 1 vCPU)"
  type        = number
  default     = 512
}

variable "memory" {
  description = "Memory in MiB for the container"
  type        = number
  default     = 1024
}

variable "desired_count" {
  description = "Number of instances of the task definition to place and keep running"
  type        = number
  default     = 1
}

# Network Configuration
variable "vpc_id" {
  description = "ID of the VPC"
  type        = string
}

variable "subnet_ids" {
  description = "List of subnet IDs"
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

variable "load_balancer_arn" {
  description = "ARN of the load balancer"
  type        = string
}

variable "listener_arn" {
  description = "ARN of the load balancer listener"
  type        = string
}

variable "service_discovery_namespace_id" {
  description = "ID of the service discovery namespace"
  type        = string
}

# Authentication and Security
variable "auth0_domain" {
  description = "Auth0 domain"
  type        = string
}

variable "auth0_audience" {
  description = "Auth0 audience"
  type        = string
}

variable "auth0_client_secret_arn" {
  description = "ARN of the Auth0 client secret in AWS Secrets Manager"
  type        = string
}

variable "jwt_secret_arn" {
  description = "ARN of the JWT secret in AWS Secrets Manager"
  type        = string
}

# Database Configuration
variable "database_url" {
  description = "Database connection URL"
  type        = string
}

variable "database_password_arn" {
  description = "ARN of the database password in AWS Secrets Manager"
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

# Drogon Framework Configuration
variable "drogon_thread_num" {
  description = "Number of threads for Drogon framework"
  type        = number
  default     = 4
}

variable "drogon_listener_count" {
  description = "Number of listeners for Drogon framework"
  type        = number
  default     = 1
}

variable "drogon_enable_compression" {
  description = "Enable compression in Drogon framework"
  type        = bool
  default     = true
}

variable "drogon_session_timeout" {
  description = "Session timeout in seconds for Drogon framework"
  type        = number
  default     = 3600
}

variable "drogon_max_connections" {
  description = "Maximum number of connections for Drogon framework"
  type        = number
  default     = 1000
}

variable "drogon_db_connection_number" {
  description = "Number of database connections for Drogon framework"
  type        = number
  default     = 10
}

# Logging Configuration
variable "log_retention_days" {
  description = "CloudWatch log retention period in days"
  type        = number
  default     = 30
}

# Auto Scaling Configuration
variable "enable_auto_scaling" {
  description = "Enable auto scaling for the service"
  type        = bool
  default     = true
}

variable "auto_scaling_min_capacity" {
  description = "Minimum number of tasks"
  type        = number
  default     = 1
}

variable "auto_scaling_max_capacity" {
  description = "Maximum number of tasks"
  type        = number
  default     = 10
}

variable "auto_scaling_target_cpu" {
  description = "Target CPU utilization percentage for auto scaling"
  type        = number
  default     = 70
}

variable "auto_scaling_target_memory" {
  description = "Target memory utilization percentage for auto scaling"
  type        = number
  default     = 80
}

# Health Check Configuration
variable "health_check_path" {
  description = "Health check endpoint path"
  type        = string
  default     = "/health"
}

variable "health_check_timeout" {
  description = "Health check timeout in seconds"
  type        = number
  default     = 5
}

variable "health_check_interval" {
  description = "Health check interval in seconds"
  type        = number
  default     = 30
}

variable "health_check_retries" {
  description = "Number of health check retries"
  type        = number
  default     = 3
}

# PostHog Analytics Configuration
variable "enable_posthog" {
  description = "Enable PostHog analytics"
  type        = bool
  default     = false
}

variable "posthog_api_key" {
  description = "PostHog API key"
  type        = string
  default     = ""
  sensitive   = true
}

variable "posthog_host" {
  description = "PostHog host URL"
  type        = string
  default     = "https://app.posthog.com"
}

variable "posthog_project_id" {
  description = "PostHog project ID"
  type        = string
  default     = ""
}

variable "posthog_capture_pageviews" {
  description = "Enable PostHog pageview capture"
  type        = bool
  default     = true
}

variable "posthog_capture_pageleaves" {
  description = "Enable PostHog pageleave capture"
  type        = bool
  default     = true
}

variable "posthog_disable_session_recording" {
  description = "Disable PostHog session recording"
  type        = bool
  default     = false
}

variable "posthog_opt_out_capturing" {
  description = "Opt out of PostHog capturing"
  type        = bool
  default     = false
}

variable "posthog_anonymize_ips" {
  description = "Anonymize IP addresses in PostHog"
  type        = bool
  default     = true
}

variable "posthog_batch_size" {
  description = "PostHog batch size for events"
  type        = number
  default     = 100
}

variable "posthog_request_timeout" {
  description = "PostHog request timeout in seconds"
  type        = number
  default     = 10
}

variable "posthog_custom_events" {
  description = "List of custom events for PostHog"
  type        = list(string)
  default     = []
}

# Webhook Configuration
variable "webhook_endpoint" {
  description = "Webhook endpoint URL"
  type        = string
  default     = ""
}

# Performance Configuration
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

# Security Configuration
variable "enable_waf" {
  description = "Enable Web Application Firewall"
  type        = bool
  default     = false
}

variable "enable_vpc_endpoints" {
  description = "Enable VPC endpoints for AWS services"
  type        = bool
  default     = false
}

variable "enable_encryption_at_rest" {
  description = "Enable encryption at rest for logs"
  type        = bool
  default     = true
}

variable "enable_encryption_in_transit" {
  description = "Enable encryption in transit"
  type        = bool
  default     = true
}

# Monitoring Configuration
variable "enable_detailed_monitoring" {
  description = "Enable detailed monitoring"
  type        = bool
  default     = true
}

variable "enable_xray_tracing" {
  description = "Enable AWS X-Ray tracing"
  type        = bool
  default     = false
}

variable "enable_cloudwatch_insights" {
  description = "Enable CloudWatch Insights"
  type        = bool
  default     = true
}
