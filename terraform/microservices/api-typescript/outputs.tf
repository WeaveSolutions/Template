# TypeScript API Service Outputs

# Service Information
output "service_name" {
  description = "Name of the TypeScript API service"
  value       = var.enable_typescript_api ? "typescript-api" : null
}

output "service_arn" {
  description = "ARN of the ECS service"
  value       = var.enable_typescript_api ? aws_ecs_service.typescript_api[0].id : null
}

output "task_definition_arn" {
  description = "ARN of the ECS task definition"
  value       = var.enable_typescript_api ? aws_ecs_task_definition.typescript_api[0].arn : null
}

# Load Balancer Information
output "target_group_arn" {
  description = "ARN of the target group"
  value       = var.enable_typescript_api ? aws_lb_target_group.typescript_api[0].arn : null
}

output "target_group_name" {
  description = "Name of the target group"
  value       = var.enable_typescript_api ? aws_lb_target_group.typescript_api[0].name : null
}

output "listener_rule_arn" {
  description = "ARN of the listener rule"
  value       = var.enable_typescript_api ? aws_lb_listener_rule.typescript_api[0].arn : null
}

# Service Discovery
output "service_discovery_service_arn" {
  description = "ARN of the service discovery service"
  value       = var.enable_typescript_api ? aws_service_discovery_service.typescript_api[0].arn : null
}

output "service_discovery_service_name" {
  description = "Name of the service discovery service"
  value       = var.enable_typescript_api ? aws_service_discovery_service.typescript_api[0].name : null
}

# CloudWatch Logs
output "log_group_name" {
  description = "Name of the CloudWatch log group"
  value       = var.enable_typescript_api ? aws_cloudwatch_log_group.typescript_api[0].name : null
}

output "log_group_arn" {
  description = "ARN of the CloudWatch log group"
  value       = var.enable_typescript_api ? aws_cloudwatch_log_group.typescript_api[0].arn : null
}

# IAM Roles
output "execution_role_arn" {
  description = "ARN of the ECS execution role"
  value       = var.enable_typescript_api ? aws_iam_role.ecs_execution_role[0].arn : null
}

output "task_role_arn" {
  description = "ARN of the ECS task role"
  value       = var.enable_typescript_api ? aws_iam_role.ecs_task_role[0].arn : null
}

# Container Information
output "container_image" {
  description = "Docker image used for the container"
  value       = var.container_image
}

output "container_port" {
  description = "Port the container is listening on"
  value       = 8020
}

# Auto Scaling
output "autoscaling_target_arn" {
  description = "ARN of the auto scaling target"
  value       = var.enable_typescript_api && var.enable_auto_scaling ? aws_appautoscaling_target.typescript_api[0].arn : null
}

output "autoscaling_policy_cpu_arn" {
  description = "ARN of the CPU auto scaling policy"
  value       = var.enable_typescript_api && var.enable_auto_scaling ? aws_appautoscaling_policy.typescript_api_cpu[0].arn : null
}

output "autoscaling_policy_memory_arn" {
  description = "ARN of the memory auto scaling policy"
  value       = var.enable_typescript_api && var.enable_auto_scaling ? aws_appautoscaling_policy.typescript_api_memory[0].arn : null
}

# Service Configuration
output "desired_count" {
  description = "Desired number of running tasks"
  value       = var.desired_count
}

output "cpu" {
  description = "CPU units allocated to the container"
  value       = var.cpu
}

output "memory" {
  description = "Memory allocated to the container"
  value       = var.memory
}

# Health Check Information
output "health_check_path" {
  description = "Health check endpoint path"
  value       = var.health_check_path
}

output "health_check_url" {
  description = "Full health check URL"
  value       = var.enable_typescript_api ? "http://localhost:8020${var.health_check_path}" : null
}

# Environment Information
output "environment" {
  description = "Environment name"
  value       = var.environment
}

output "project_name" {
  description = "Project name"
  value       = var.project_name
}

# Network Configuration
output "vpc_id" {
  description = "VPC ID where the service is deployed"
  value       = var.vpc_id
}

output "subnet_ids" {
  description = "Subnet IDs where the service is deployed"
  value       = var.subnet_ids
}

output "security_group_ids" {
  description = "Security group IDs associated with the service"
  value       = var.security_group_ids
}

# PostHog Analytics
output "posthog_enabled" {
  description = "Whether PostHog analytics is enabled"
  value       = var.enable_posthog && var.enable_typescript_api
}

output "posthog_organization_id" {
  description = "PostHog organization ID"
  value       = var.posthog_organization_id
  sensitive   = true
}

output "posthog_host" {
  description = "PostHog host URL"
  value       = var.posthog_host
}

# Service Status
output "service_status" {
  description = "Current service deployment status"
  value = var.enable_typescript_api ? {
    enabled           = true
    service_name      = "typescript-api"
    container_port    = 8020
    desired_count     = var.desired_count
    cpu               = var.cpu
    memory            = var.memory
    auto_scaling      = var.enable_auto_scaling
    posthog_enabled   = var.enable_posthog
    environment       = var.environment
  } : {
    enabled = false
  }
}

# API Endpoints
output "api_endpoints" {
  description = "Available API endpoints"
  value = var.enable_typescript_api ? {
    health_check = "/health"
    api_base     = "/api/v1/typescript"
    metrics      = "/metrics"
    docs         = "/docs"
  } : {}
}

# Resource Tags
output "common_tags" {
  description = "Common tags applied to all resources"
  value = {
    Service     = "typescript-api"
    Backend     = "typescript"
    Port        = "8020"
    Environment = var.environment
    Project     = var.project_name
  }
}

# Monitoring Information
output "monitoring_configuration" {
  description = "Monitoring configuration details"
  value = var.enable_typescript_api ? {
    log_group_name                = aws_cloudwatch_log_group.typescript_api[0].name
    log_retention_days            = var.log_retention_days
    detailed_monitoring_enabled   = var.enable_detailed_monitoring
    performance_monitoring_enabled = var.enable_performance_monitoring
    error_tracking_enabled        = var.enable_error_tracking
    cpu_alarm_threshold           = var.alarm_threshold_cpu
    memory_alarm_threshold        = var.alarm_threshold_memory
  } : {}
}

# Security Configuration
output "security_configuration" {
  description = "Security configuration details"
  value = var.enable_typescript_api ? {
    auth0_domain                = var.auth0_domain
    auth0_audience              = var.auth0_audience
    jwt_validation_enabled      = true
    waf_enabled                 = var.enable_waf
    security_groups_enabled     = var.enable_security_groups
    allowed_cidr_blocks         = var.allowed_cidr_blocks
  } : {}
}

# Performance Configuration
output "performance_configuration" {
  description = "Performance configuration details"
  value = var.enable_typescript_api ? {
    cpu_target_value        = var.cpu_target_value
    memory_target_value     = var.memory_target_value
    min_capacity            = var.min_capacity
    max_capacity            = var.max_capacity
    health_check_interval   = var.health_check_interval
    health_check_timeout    = var.health_check_timeout
    health_check_retries    = var.health_check_retries
    deregistration_delay    = var.deregistration_delay
  } : {}
}

# Database Configuration
output "database_configuration" {
  description = "Database configuration details"
  value = var.enable_typescript_api ? {
    database_url_configured     = var.database_url != ""
    redis_url_configured        = var.redis_url != ""
    backup_enabled              = var.enable_backup
    backup_retention_days       = var.backup_retention_days
  } : {}
  sensitive = true
}

# Kong Gateway Configuration
output "kong_configuration" {
  description = "Kong API Gateway configuration"
  value = var.enable_typescript_api ? {
    admin_url_configured        = var.kong_admin_url != ""
    proxy_cache_ttl            = var.kong_proxy_cache_ttl
    rate_limit_minute          = var.kong_rate_limit_minute
    rate_limit_hour            = var.kong_rate_limit_hour
    jwt_validation_enabled     = var.kong_jwt_validation_enabled
    cors_enabled               = var.kong_cors_enabled
    response_transformer_enabled = var.kong_response_transformer_enabled
    circuit_breaker_enabled    = var.kong_circuit_breaker_enabled
  } : {}
}

# Integration Configuration
output "integration_configuration" {
  description = "External integration configuration"
  value = var.enable_typescript_api ? {
    kong_admin_url_configured   = var.kong_admin_url != ""
    webhook_endpoint_configured = var.webhook_endpoint != ""
    spot_instances_enabled      = var.enable_spot_instances
    spot_instance_percentage    = var.spot_instance_percentage
  } : {}
}

# Cost Optimization
output "cost_optimization" {
  description = "Cost optimization features"
  value = var.enable_typescript_api ? {
    spot_instances_enabled      = var.enable_spot_instances
    spot_instance_percentage    = var.spot_instance_percentage
    auto_scaling_enabled        = var.enable_auto_scaling
    min_capacity                = var.min_capacity
    max_capacity                = var.max_capacity
  } : {}
}
