# Variables for Haskell Servant API Service

variable "enable_haskell_api" {
  description = "Enable deployment of the Haskell Servant API service"
  type        = bool
  default     = false
}

variable "project_name" {
  description = "Name of the project"
  type        = string
}

variable "environment" {
  description = "Environment name (development, staging, production)"
  type        = string
}

variable "aws_region" {
  description = "AWS region"
  type        = string
}

variable "vpc_id" {
  description = "VPC ID where resources will be created"
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

variable "ecs_cluster_id" {
  description = "ECS cluster ID"
  type        = string
}

variable "alb_listener_arn" {
  description = "Application Load Balancer listener ARN"
  type        = string
}

variable "listener_rule_priority" {
  description = "Priority for the ALB listener rule"
  type        = number
  default     = 120
}

variable "service_discovery_namespace_id" {
  description = "Service Discovery namespace ID"
  type        = string
}

# Container Configuration
variable "container_image" {
  description = "Docker image for the Haskell API service"
  type        = string
  default     = "nexpo/haskell-api:latest"
}

variable "cpu" {
  description = "CPU units for the Fargate task"
  type        = number
  default     = 512
}

variable "memory" {
  description = "Memory for the Fargate task"
  type        = number
  default     = 1024
}

variable "desired_count" {
  description = "Desired number of instances of the task definition"
  type        = number
  default     = 1
}

# Auth0 Configuration
variable "auth0_domain_secret_arn" {
  description = "ARN of the Auth0 domain secret in AWS Secrets Manager"
  type        = string
}

variable "auth0_audience_secret_arn" {
  description = "ARN of the Auth0 audience secret in AWS Secrets Manager"
  type        = string
}

variable "auth0_client_id_secret_arn" {
  description = "ARN of the Auth0 client ID secret in AWS Secrets Manager"
  type        = string
}

variable "auth0_client_secret_secret_arn" {
  description = "ARN of the Auth0 client secret in AWS Secrets Manager"
  type        = string
}

# MindsDB Configuration
variable "mindsdb_host_secret_arn" {
  description = "ARN of the MindsDB host secret in AWS Secrets Manager"
  type        = string
}

variable "mindsdb_port_secret_arn" {
  description = "ARN of the MindsDB port secret in AWS Secrets Manager"
  type        = string
}

variable "mindsdb_user_secret_arn" {
  description = "ARN of the MindsDB user secret in AWS Secrets Manager"
  type        = string
}

variable "mindsdb_password_secret_arn" {
  description = "ARN of the MindsDB password secret in AWS Secrets Manager"
  type        = string
}

# Logging Configuration
variable "log_retention_days" {
  description = "CloudWatch logs retention period in days"
  type        = number
  default     = 30
}

# Scaling Configuration
variable "min_capacity" {
  description = "Minimum number of tasks for auto scaling"
  type        = number
  default     = 1
}

variable "max_capacity" {
  description = "Maximum number of tasks for auto scaling"
  type        = number
  default     = 10
}

variable "target_cpu_utilization" {
  description = "Target CPU utilization for auto scaling"
  type        = number
  default     = 70
}

variable "target_memory_utilization" {
  description = "Target memory utilization for auto scaling"
  type        = number
  default     = 80
}
