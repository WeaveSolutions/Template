# Variables for Go Beego API Service Infrastructure

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

variable "enable_go_api" {
  description = "Whether to enable the Go API service"
  type        = bool
  default     = false
}

# Container Configuration
variable "container_image" {
  description = "Docker image for the Go API service"
  type        = string
  default     = "nexpo/api-go:latest"
}

variable "cpu" {
  description = "CPU units for the container (256, 512, 1024, 2048, 4096)"
  type        = number
  default     = 512
}

variable "memory" {
  description = "Memory for the container in MB"
  type        = number
  default     = 1024
}

variable "desired_count" {
  description = "Desired number of instances"
  type        = number
  default     = 1
}

# Auto Scaling Configuration
variable "enable_autoscaling" {
  description = "Whether to enable auto scaling"
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

# Network Configuration
variable "vpc_id" {
  description = "VPC ID where the service will be deployed"
  type        = string
}

variable "private_subnet_ids" {
  description = "List of private subnet IDs"
  type        = list(string)
}

variable "alb_security_group_id" {
  description = "Application Load Balancer security group ID"
  type        = string
}

variable "alb_listener_arn" {
  description = "Application Load Balancer listener ARN"
  type        = string
}

variable "listener_rule_priority" {
  description = "Priority for the load balancer listener rule"
  type        = number
  default     = 300
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

variable "enable_execute_command" {
  description = "Whether to enable ECS execute command"
  type        = bool
  default     = false
}

# Service Discovery Configuration
variable "service_discovery_namespace_id" {
  description = "Service discovery namespace ID"
  type        = string
}

variable "service_discovery_namespace_name" {
  description = "Service discovery namespace name"
  type        = string
}

# Database Configuration
variable "database_host" {
  description = "Database host"
  type        = string
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

variable "database_password_secret_arn" {
  description = "ARN of the secret containing database password"
  type        = string
}

# Redis Configuration
variable "redis_host" {
  description = "Redis host"
  type        = string
}

variable "redis_password_secret_arn" {
  description = "ARN of the secret containing Redis password"
  type        = string
}

# Auth0 Configuration
variable "auth0_domain" {
  description = "Auth0 domain"
  type        = string
}

variable "auth0_client_id" {
  description = "Auth0 client ID"
  type        = string
}

variable "auth0_client_secret_arn" {
  description = "ARN of the secret containing Auth0 client secret"
  type        = string
}

variable "auth0_audience" {
  description = "Auth0 audience"
  type        = string
}

# JWT Configuration
variable "jwt_secret_arn" {
  description = "ARN of the secret containing JWT secret"
  type        = string
}

# MindsDB Configuration
variable "mindsdb_host" {
  description = "MindsDB host"
  type        = string
  default     = "localhost"
}

# Kong Gateway Configuration
variable "kong_gateway_url" {
  description = "Kong Gateway URL"
  type        = string
  default     = "http://localhost:8000"
}

variable "enable_kong_integration" {
  description = "Whether to enable Kong Gateway integration"
  type        = bool
  default     = true
}

# PostHog Configuration
variable "posthog_api_key" {
  description = "PostHog API key"
  type        = string
  sensitive   = true
}

variable "posthog_host" {
  description = "PostHog host"
  type        = string
  default     = "https://app.posthog.com"
}

variable "enable_posthog" {
  description = "Whether to enable PostHog integration"
  type        = bool
  default     = true
}

# CORS Configuration
variable "cors_allowed_origins" {
  description = "Comma-separated list of allowed CORS origins"
  type        = string
  default     = "*"
}

# Logging Configuration
variable "log_retention_days" {
  description = "CloudWatch log retention in days"
  type        = number
  default     = 7
}

# AWS Configuration
variable "aws_region" {
  description = "AWS region"
  type        = string
  default     = "us-east-1"
}

# Tags
variable "additional_tags" {
  description = "Additional tags to apply to all resources"
  type        = map(string)
  default     = {}
}
