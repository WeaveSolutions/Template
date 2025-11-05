# Python FastAPI Service Variables

# Service Configuration
variable "enable_python_api" {
  description = "Enable Python FastAPI service deployment"
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
  description = "Docker image for Python FastAPI service"
  type        = string
  default     = "nexpo/api-python:latest"
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
  default     = 2
}

# Network Configuration
variable "vpc_id" {
  description = "VPC ID where resources will be created"
  type        = string
}

variable "vpc_cidr" {
  description = "CIDR block for VPC"
  type        = string
  default     = "10.0.0.0/16"
}

variable "private_subnet_ids" {
  description = "List of private subnet IDs"
  type        = list(string)
}

variable "public_subnet_ids" {
  description = "List of public subnet IDs"
  type        = list(string)
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

# Load Balancer Configuration
variable "alb_listener_arn" {
  description = "ARN of the ALB listener"
  type        = string
}

variable "listener_rule_priority" {
  description = "Priority for the listener rule"
  type        = number
  default     = 130
}

# Auto Scaling Configuration
variable "enable_auto_scaling" {
  description = "Enable auto scaling for the service"
  type        = bool
  default     = true
}

variable "min_capacity" {
  description = "Minimum number of tasks"
  type        = number
  default     = 2
}

variable "max_capacity" {
  description = "Maximum number of tasks"
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

# Logging Configuration
variable "log_retention_days" {
  description = "CloudWatch log retention in days"
  type        = number
  default     = 7
}

# External Service Configuration
variable "auth0_domain" {
  description = "Auth0 domain"
  type        = string
  default     = ""
}

variable "auth0_audience" {
  description = "Auth0 audience"
  type        = string
  default     = ""
}

variable "mindsdb_url" {
  description = "MindsDB URL"
  type        = string
  default     = "http://localhost:47334"
}

variable "kong_admin_url" {
  description = "Kong Admin URL"
  type        = string
  default     = "http://localhost:8001"
}

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

# Database Configuration
variable "database_url" {
  description = "Database connection URL"
  type        = string
  default     = ""
  sensitive   = true
}

variable "redis_url" {
  description = "Redis connection URL"
  type        = string
  default     = ""
  sensitive   = true
}

# Service Discovery
variable "service_discovery_namespace_id" {
  description = "Service discovery namespace ID"
  type        = string
  default     = ""
}

# Monitoring Configuration
variable "enable_monitoring" {
  description = "Enable monitoring and alerting"
  type        = bool
  default     = true
}

variable "alarm_cpu_threshold" {
  description = "CPU utilization threshold for alarms"
  type        = number
  default     = 80
}

variable "alarm_memory_threshold" {
  description = "Memory utilization threshold for alarms"
  type        = number
  default     = 85
}

# Security Configuration
variable "enable_container_insights" {
  description = "Enable Container Insights"
  type        = bool
  default     = true
}

variable "enable_execution_command" {
  description = "Enable ECS Exec for debugging"
  type        = bool
  default     = false
}
