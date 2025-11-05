# R API Service Outputs

# Service Information
output "service_name" {
  description = "Name of the R API service"
  value       = var.enable_r_api ? "r-api" : null
}

output "service_port" {
  description = "Port the R API service runs on"
  value       = var.enable_r_api ? 8080 : null
}

output "service_enabled" {
  description = "Whether the R API service is enabled"
  value       = var.enable_r_api
}

# ECS Service Information
output "ecs_service_name" {
  description = "Name of the ECS service"
  value       = var.enable_r_api ? aws_ecs_service.r_api[0].name : null
}

output "ecs_service_arn" {
  description = "ARN of the ECS service"
  value       = var.enable_r_api ? aws_ecs_service.r_api[0].id : null
}

output "ecs_task_definition_arn" {
  description = "ARN of the ECS task definition"
  value       = var.enable_r_api ? aws_ecs_task_definition.r_api[0].arn : null
}

output "ecs_task_definition_revision" {
  description = "Revision of the ECS task definition"
  value       = var.enable_r_api ? aws_ecs_task_definition.r_api[0].revision : null
}

# Load Balancer Information
output "target_group_arn" {
  description = "ARN of the target group"
  value       = var.enable_r_api ? aws_lb_target_group.r_api[0].arn : null
}

output "target_group_name" {
  description = "Name of the target group"
  value       = var.enable_r_api ? aws_lb_target_group.r_api[0].name : null
}

output "listener_rule_arn" {
  description = "ARN of the listener rule"
  value       = var.enable_r_api ? aws_lb_listener_rule.r_api[0].arn : null
}

output "listener_rule_priority" {
  description = "Priority of the listener rule"
  value       = var.enable_r_api ? aws_lb_listener_rule.r_api[0].priority : null
}

# Security Group Information
output "security_group_id" {
  description = "ID of the security group"
  value       = var.enable_r_api ? aws_security_group.r_api[0].id : null
}

output "security_group_name" {
  description = "Name of the security group"
  value       = var.enable_r_api ? aws_security_group.r_api[0].name : null
}

# IAM Role Information
output "ecs_execution_role_arn" {
  description = "ARN of the ECS execution role"
  value       = var.enable_r_api ? aws_iam_role.ecs_execution_role[0].arn : null
}

output "ecs_task_role_arn" {
  description = "ARN of the ECS task role"
  value       = var.enable_r_api ? aws_iam_role.ecs_task_role[0].arn : null
}

# CloudWatch Log Group Information
output "log_group_name" {
  description = "Name of the CloudWatch log group"
  value       = var.enable_r_api ? aws_cloudwatch_log_group.r_api[0].name : null
}

output "log_group_arn" {
  description = "ARN of the CloudWatch log group"
  value       = var.enable_r_api ? aws_cloudwatch_log_group.r_api[0].arn : null
}

# Service Discovery Information
output "service_discovery_service_name" {
  description = "Name of the service discovery service"
  value       = var.enable_r_api && var.enable_service_discovery ? aws_service_discovery_service.r_api[0].name : null
}

output "service_discovery_service_arn" {
  description = "ARN of the service discovery service"
  value       = var.enable_r_api && var.enable_service_discovery ? aws_service_discovery_service.r_api[0].arn : null
}

# Auto Scaling Information
output "autoscaling_target_resource_id" {
  description = "Resource ID of the auto scaling target"
  value       = var.enable_r_api && var.enable_auto_scaling ? aws_appautoscaling_target.r_api[0].resource_id : null
}

output "autoscaling_cpu_policy_arn" {
  description = "ARN of the CPU auto scaling policy"
  value       = var.enable_r_api && var.enable_auto_scaling ? aws_appautoscaling_policy.r_api_cpu[0].arn : null
}

output "autoscaling_memory_policy_arn" {
  description = "ARN of the memory auto scaling policy"
  value       = var.enable_r_api && var.enable_auto_scaling ? aws_appautoscaling_policy.r_api_memory[0].arn : null
}

# Container Configuration
output "container_image" {
  description = "Docker image used for the container"
  value       = var.enable_r_api ? var.container_image : null
}

output "container_port" {
  description = "Port the container listens on"
  value       = var.enable_r_api ? 8080 : null
}

output "desired_count" {
  description = "Desired number of running instances"
  value       = var.enable_r_api ? var.desired_count : null
}

output "cpu_allocation" {
  description = "CPU units allocated to the container"
  value       = var.enable_r_api ? var.cpu : null
}

output "memory_allocation" {
  description = "Memory allocated to the container in MB"
  value       = var.enable_r_api ? var.memory : null
}

# Environment Configuration
output "environment" {
  description = "Environment name"
  value       = var.enable_r_api ? var.environment : null
}

output "project_name" {
  description = "Project name"
  value       = var.enable_r_api ? var.project_name : null
}

output "aws_region" {
  description = "AWS region"
  value       = var.enable_r_api ? var.aws_region : null
}

# R-specific Configuration
output "r_version" {
  description = "R version"
  value       = var.enable_r_api ? var.r_version : null
}

output "plumber_host" {
  description = "Plumber host"
  value       = var.enable_r_api ? var.plumber_host : null
}

output "plumber_port" {
  description = "Plumber port"
  value       = var.enable_r_api ? var.plumber_port : null
}

output "r_libs_user" {
  description = "R user library path"
  value       = var.enable_r_api ? var.r_libs_user : null
}

output "r_packages" {
  description = "List of R packages to install"
  value       = var.enable_r_api ? var.r_packages : null
}

output "r_cran_mirror" {
  description = "CRAN mirror URL"
  value       = var.enable_r_api ? var.r_cran_mirror : null
}

output "r_memory_limit" {
  description = "R memory limit in MB"
  value       = var.enable_r_api ? var.r_memory_limit : null
}

output "r_max_connections" {
  description = "Maximum number of R connections"
  value       = var.enable_r_api ? var.r_max_connections : null
}

output "r_timeout" {
  description = "R timeout in seconds"
  value       = var.enable_r_api ? var.r_timeout : null
}

# API Endpoints
output "api_base_url" {
  description = "Base URL for the R API"
  value       = var.enable_r_api ? "/api/v1/r" : null
}

output "health_check_endpoint" {
  description = "Health check endpoint"
  value       = var.enable_r_api ? "/health" : null
}

output "swagger_endpoint" {
  description = "Swagger documentation endpoint"
  value       = var.enable_r_api ? "/swagger" : null
}

output "plumber_docs_endpoint" {
  description = "Plumber documentation endpoint"
  value       = var.enable_r_api ? "/__docs__/" : null
}

# Integration Information
output "auth0_domain" {
  description = "Auth0 domain"
  value       = var.enable_r_api ? var.auth0_domain : null
}

output "auth0_audience" {
  description = "Auth0 API audience"
  value       = var.enable_r_api ? var.auth0_audience : null
}

output "mindsdb_host" {
  description = "MindsDB host"
  value       = var.enable_r_api ? var.mindsdb_host : null
}

output "mindsdb_port" {
  description = "MindsDB port"
  value       = var.enable_r_api ? var.mindsdb_port : null
}

output "kong_proxy_url" {
  description = "Kong proxy URL"
  value       = var.enable_r_api ? var.kong_proxy_url : null
}

output "kong_admin_url" {
  description = "Kong admin URL"
  value       = var.enable_r_api ? var.kong_admin_url : null
}

output "posthog_host" {
  description = "PostHog host"
  value       = var.enable_r_api ? var.posthog_host : null
}

# Performance Configuration
output "r_gc_mem_grow" {
  description = "R garbage collection memory growth"
  value       = var.enable_r_api ? var.r_gc_mem_grow : null
}

output "r_gc_mem_grow_mb" {
  description = "R garbage collection memory growth in MB"
  value       = var.enable_r_api ? var.r_gc_mem_grow_mb : null
}

output "r_max_vsize" {
  description = "R maximum virtual size"
  value       = var.enable_r_api ? var.r_max_vsize : null
}

output "r_max_nsize" {
  description = "R maximum number of cons cells"
  value       = var.enable_r_api ? var.r_max_nsize : null
}

# Status Information
output "deployment_status" {
  description = "Deployment status"
  value = var.enable_r_api ? {
    service_running = aws_ecs_service.r_api[0].desired_count > 0
    task_definition_active = aws_ecs_task_definition.r_api[0].status == "ACTIVE"
    target_group_healthy = aws_lb_target_group.r_api[0].health_check[0].enabled
    auto_scaling_enabled = var.enable_auto_scaling
    service_discovery_enabled = var.enable_service_discovery
    kong_integration_enabled = var.enable_kong_integration
  } : null
}

# Resource Tags
output "common_tags" {
  description = "Common tags applied to all resources"
  value = var.enable_r_api ? {
    Service     = "r-api"
    Backend     = "r"
    Port        = 8080
    Environment = var.environment
    Project     = var.project_name
  } : null
}
