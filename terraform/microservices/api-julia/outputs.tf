# Julia API Service Outputs

# Service Information
output "service_name" {
  description = "Name of the Julia API service"
  value       = var.enable_julia_api ? "julia-api" : null
}

output "service_port" {
  description = "Port the Julia API service runs on"
  value       = var.enable_julia_api ? 8090 : null
}

output "service_enabled" {
  description = "Whether the Julia API service is enabled"
  value       = var.enable_julia_api
}

# ECS Service Information
output "ecs_service_name" {
  description = "Name of the ECS service"
  value       = var.enable_julia_api ? aws_ecs_service.julia_api[0].name : null
}

output "ecs_service_arn" {
  description = "ARN of the ECS service"
  value       = var.enable_julia_api ? aws_ecs_service.julia_api[0].id : null
}

output "ecs_task_definition_arn" {
  description = "ARN of the ECS task definition"
  value       = var.enable_julia_api ? aws_ecs_task_definition.julia_api[0].arn : null
}

output "ecs_task_definition_revision" {
  description = "Revision of the ECS task definition"
  value       = var.enable_julia_api ? aws_ecs_task_definition.julia_api[0].revision : null
}

# Load Balancer Information
output "target_group_arn" {
  description = "ARN of the target group"
  value       = var.enable_julia_api ? aws_lb_target_group.julia_api[0].arn : null
}

output "target_group_name" {
  description = "Name of the target group"
  value       = var.enable_julia_api ? aws_lb_target_group.julia_api[0].name : null
}

output "listener_rule_arn" {
  description = "ARN of the listener rule"
  value       = var.enable_julia_api ? aws_lb_listener_rule.julia_api[0].arn : null
}

output "listener_rule_priority" {
  description = "Priority of the listener rule"
  value       = var.enable_julia_api ? aws_lb_listener_rule.julia_api[0].priority : null
}

# Security Group Information
output "security_group_id" {
  description = "ID of the security group"
  value       = var.enable_julia_api ? aws_security_group.julia_api[0].id : null
}

output "security_group_name" {
  description = "Name of the security group"
  value       = var.enable_julia_api ? aws_security_group.julia_api[0].name : null
}

# IAM Role Information
output "ecs_execution_role_arn" {
  description = "ARN of the ECS execution role"
  value       = var.enable_julia_api ? aws_iam_role.ecs_execution_role[0].arn : null
}

output "ecs_task_role_arn" {
  description = "ARN of the ECS task role"
  value       = var.enable_julia_api ? aws_iam_role.ecs_task_role[0].arn : null
}

# CloudWatch Log Group Information
output "log_group_name" {
  description = "Name of the CloudWatch log group"
  value       = var.enable_julia_api ? aws_cloudwatch_log_group.julia_api[0].name : null
}

output "log_group_arn" {
  description = "ARN of the CloudWatch log group"
  value       = var.enable_julia_api ? aws_cloudwatch_log_group.julia_api[0].arn : null
}

# Service Discovery Information
output "service_discovery_service_name" {
  description = "Name of the service discovery service"
  value       = var.enable_julia_api && var.enable_service_discovery ? aws_service_discovery_service.julia_api[0].name : null
}

output "service_discovery_service_arn" {
  description = "ARN of the service discovery service"
  value       = var.enable_julia_api && var.enable_service_discovery ? aws_service_discovery_service.julia_api[0].arn : null
}

# Auto Scaling Information
output "autoscaling_target_resource_id" {
  description = "Resource ID of the auto scaling target"
  value       = var.enable_julia_api && var.enable_auto_scaling ? aws_appautoscaling_target.julia_api[0].resource_id : null
}

output "autoscaling_cpu_policy_arn" {
  description = "ARN of the CPU auto scaling policy"
  value       = var.enable_julia_api && var.enable_auto_scaling ? aws_appautoscaling_policy.julia_api_cpu[0].arn : null
}

output "autoscaling_memory_policy_arn" {
  description = "ARN of the memory auto scaling policy"
  value       = var.enable_julia_api && var.enable_auto_scaling ? aws_appautoscaling_policy.julia_api_memory[0].arn : null
}

# Container Configuration
output "container_image" {
  description = "Docker image used for the container"
  value       = var.enable_julia_api ? var.container_image : null
}

output "container_port" {
  description = "Port the container listens on"
  value       = var.enable_julia_api ? 8090 : null
}

output "desired_count" {
  description = "Desired number of running instances"
  value       = var.enable_julia_api ? var.desired_count : null
}

output "cpu_allocation" {
  description = "CPU units allocated to the container"
  value       = var.enable_julia_api ? var.cpu : null
}

output "memory_allocation" {
  description = "Memory allocated to the container in MB"
  value       = var.enable_julia_api ? var.memory : null
}

# Environment Configuration
output "environment" {
  description = "Environment name"
  value       = var.enable_julia_api ? var.environment : null
}

output "project_name" {
  description = "Project name"
  value       = var.enable_julia_api ? var.project_name : null
}

output "aws_region" {
  description = "AWS region"
  value       = var.enable_julia_api ? var.aws_region : null
}

# Julia-specific Configuration
output "julia_version" {
  description = "Julia version"
  value       = var.enable_julia_api ? var.julia_version : null
}

output "julia_num_threads" {
  description = "Number of Julia threads"
  value       = var.enable_julia_api ? var.julia_num_threads : null
}

output "julia_depot_path" {
  description = "Julia depot path"
  value       = var.enable_julia_api ? var.julia_depot_path : null
}

output "julia_load_path" {
  description = "Julia load path"
  value       = var.enable_julia_api ? var.julia_load_path : null
}

output "julia_project" {
  description = "Julia project path"
  value       = var.enable_julia_api ? var.julia_project : null
}

output "genie_env" {
  description = "Genie environment"
  value       = var.enable_julia_api ? var.genie_env : null
}

output "genie_host" {
  description = "Genie host"
  value       = var.enable_julia_api ? var.genie_host : null
}

output "genie_port" {
  description = "Genie port"
  value       = var.enable_julia_api ? var.genie_port : null
}

output "julia_packages" {
  description = "List of Julia packages to install"
  value       = var.enable_julia_api ? var.julia_packages : null
}

output "julia_registry_url" {
  description = "Julia registry URL"
  value       = var.enable_julia_api ? var.julia_registry_url : null
}

output "julia_optimization_level" {
  description = "Julia optimization level"
  value       = var.enable_julia_api ? var.julia_optimization_level : null
}

output "julia_gc_threads" {
  description = "Number of Julia GC threads"
  value       = var.enable_julia_api ? var.julia_gc_threads : null
}

output "julia_heap_size_hint" {
  description = "Julia heap size hint"
  value       = var.enable_julia_api ? var.julia_heap_size_hint : null
}

# API Endpoints
output "api_base_url" {
  description = "Base URL for the Julia API"
  value       = var.enable_julia_api ? "/api/v1/julia" : null
}

output "health_check_endpoint" {
  description = "Health check endpoint"
  value       = var.enable_julia_api ? "/health" : null
}

output "swagger_endpoint" {
  description = "Swagger documentation endpoint"
  value       = var.enable_julia_api ? "/swagger" : null
}

output "genie_docs_endpoint" {
  description = "Genie documentation endpoint"
  value       = var.enable_julia_api ? "/docs" : null
}

# Integration Information
output "auth0_domain" {
  description = "Auth0 domain"
  value       = var.enable_julia_api ? var.auth0_domain : null
}

output "auth0_audience" {
  description = "Auth0 API audience"
  value       = var.enable_julia_api ? var.auth0_audience : null
}

output "mindsdb_host" {
  description = "MindsDB host"
  value       = var.enable_julia_api ? var.mindsdb_host : null
}

output "mindsdb_port" {
  description = "MindsDB port"
  value       = var.enable_julia_api ? var.mindsdb_port : null
}

output "kong_proxy_url" {
  description = "Kong proxy URL"
  value       = var.enable_julia_api ? var.kong_proxy_url : null
}

output "kong_admin_url" {
  description = "Kong admin URL"
  value       = var.enable_julia_api ? var.kong_admin_url : null
}

output "posthog_host" {
  description = "PostHog host"
  value       = var.enable_julia_api ? var.posthog_host : null
}

# Performance Configuration
output "julia_compile_cache" {
  description = "Julia compile cache enabled"
  value       = var.enable_julia_api ? var.julia_compile_cache : null
}

output "julia_precompile_packages" {
  description = "Julia precompile packages enabled"
  value       = var.enable_julia_api ? var.julia_precompile_packages : null
}

output "julia_startup_file" {
  description = "Julia startup file path"
  value       = var.enable_julia_api ? var.julia_startup_file : null
}

output "julia_sysimage" {
  description = "Julia system image path"
  value       = var.enable_julia_api ? var.julia_sysimage : null
}

output "julia_inline" {
  description = "Julia inline optimization enabled"
  value       = var.enable_julia_api ? var.julia_inline : null
}

output "julia_check_bounds" {
  description = "Julia bounds checking setting"
  value       = var.enable_julia_api ? var.julia_check_bounds : null
}

# Status Information
output "deployment_status" {
  description = "Deployment status"
  value = var.enable_julia_api ? {
    service_running = aws_ecs_service.julia_api[0].desired_count > 0
    task_definition_active = aws_ecs_task_definition.julia_api[0].status == "ACTIVE"
    target_group_healthy = aws_lb_target_group.julia_api[0].health_check[0].enabled
    auto_scaling_enabled = var.enable_auto_scaling
    service_discovery_enabled = var.enable_service_discovery
    kong_integration_enabled = var.enable_kong_integration
  } : null
}

# Resource Tags
output "common_tags" {
  description = "Common tags applied to all resources"
  value = var.enable_julia_api ? {
    Service     = "julia-api"
    Backend     = "julia"
    Port        = 8090
    Environment = var.environment
    Project     = var.project_name
  } : null
}
