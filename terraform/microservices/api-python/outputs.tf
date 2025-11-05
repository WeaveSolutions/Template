# Python FastAPI Service Outputs

# Service Information
output "service_name" {
  description = "Name of the Python FastAPI service"
  value       = var.enable_python_api ? aws_ecs_service.python_api[0].name : null
}

output "service_arn" {
  description = "ARN of the Python FastAPI service"
  value       = var.enable_python_api ? aws_ecs_service.python_api[0].id : null
}

output "task_definition_arn" {
  description = "ARN of the task definition"
  value       = var.enable_python_api ? aws_ecs_task_definition.python_api[0].arn : null
}

output "service_url" {
  description = "Service URL for Python FastAPI"
  value       = var.enable_python_api ? "https://${var.project_name}.example.com/api/v1/python" : null
}

# Network Information
output "security_group_id" {
  description = "Security group ID for Python FastAPI service"
  value       = var.enable_python_api ? aws_security_group.python_api[0].id : null
}

output "target_group_arn" {
  description = "Target group ARN for Python FastAPI service"
  value       = var.enable_python_api ? aws_lb_target_group.python_api[0].arn : null
}

output "listener_rule_arn" {
  description = "Listener rule ARN for Python FastAPI service"
  value       = var.enable_python_api ? aws_lb_listener_rule.python_api[0].arn : null
}

# Container Information
output "container_port" {
  description = "Container port for Python FastAPI service"
  value       = 8030
}

output "container_image" {
  description = "Container image used for Python FastAPI service"
  value       = var.container_image
}

# IAM Role Information
output "execution_role_arn" {
  description = "ECS execution role ARN"
  value       = var.enable_python_api ? aws_iam_role.ecs_execution_role[0].arn : null
}

output "task_role_arn" {
  description = "ECS task role ARN"
  value       = var.enable_python_api ? aws_iam_role.ecs_task_role[0].arn : null
}

# Logging Information
output "log_group_name" {
  description = "CloudWatch log group name"
  value       = var.enable_python_api ? aws_cloudwatch_log_group.python_api[0].name : null
}

output "log_group_arn" {
  description = "CloudWatch log group ARN"
  value       = var.enable_python_api ? aws_cloudwatch_log_group.python_api[0].arn : null
}

# Auto Scaling Information
output "autoscaling_target_resource_id" {
  description = "Auto scaling target resource ID"
  value       = var.enable_python_api && var.enable_auto_scaling ? aws_appautoscaling_target.python_api[0].resource_id : null
}

output "autoscaling_cpu_policy_arn" {
  description = "Auto scaling CPU policy ARN"
  value       = var.enable_python_api && var.enable_auto_scaling ? aws_appautoscaling_policy.python_api_cpu[0].arn : null
}

output "autoscaling_memory_policy_arn" {
  description = "Auto scaling memory policy ARN"
  value       = var.enable_python_api && var.enable_auto_scaling ? aws_appautoscaling_policy.python_api_memory[0].arn : null
}

# Service Discovery Information
output "service_discovery_service_arn" {
  description = "Service discovery service ARN"
  value       = var.enable_python_api && var.service_discovery_namespace_id != "" ? aws_service_discovery_service.python_api[0].arn : null
}

# Configuration Information
output "service_configuration" {
  description = "Service configuration details"
  value = var.enable_python_api ? {
    service_name    = aws_ecs_service.python_api[0].name
    cluster_name    = var.ecs_cluster_name
    desired_count   = var.desired_count
    cpu             = var.cpu
    memory          = var.memory
    container_port  = 8030
    health_check_path = "/health"
    auto_scaling_enabled = var.enable_auto_scaling
    min_capacity    = var.min_capacity
    max_capacity    = var.max_capacity
  } : null
}

# Health Check Information
output "health_check_url" {
  description = "Health check URL for Python FastAPI service"
  value       = var.enable_python_api ? "https://${var.project_name}.example.com/api/v1/python/health" : null
}

# Monitoring Information
output "service_metrics" {
  description = "Service metrics information"
  value = var.enable_python_api ? {
    cpu_alarm_threshold    = var.alarm_cpu_threshold
    memory_alarm_threshold = var.alarm_memory_threshold
    log_retention_days     = var.log_retention_days
    monitoring_enabled     = var.enable_monitoring
  } : null
}

# Development Information
output "development_info" {
  description = "Development and debugging information"
  value = var.enable_python_api ? {
    local_port           = 8030
    framework           = "FastAPI"
    language            = "Python"
    package_manager     = "pip/poetry"
    docker_image        = var.container_image
    execution_command   = var.enable_execution_command
    container_insights  = var.enable_container_insights
  } : null
}

# Environment Variables
output "environment_variables" {
  description = "Environment variables used by the service"
  value = var.enable_python_api ? {
    NODE_ENV         = var.environment
    PORT             = "8030"
    AUTH0_DOMAIN     = var.auth0_domain
    AUTH0_AUDIENCE   = var.auth0_audience
    MINDSDB_URL      = var.mindsdb_url
    KONG_ADMIN_URL   = var.kong_admin_url
    POSTHOG_HOST     = var.posthog_host
    DATABASE_URL     = var.database_url != "" ? "[CONFIGURED]" : "[NOT_SET]"
    REDIS_URL        = var.redis_url != "" ? "[CONFIGURED]" : "[NOT_SET]"
  } : null
  sensitive = true
}

# Terraform State Information
output "terraform_state_info" {
  description = "Terraform state information for this service"
  value = {
    service_enabled = var.enable_python_api
    provider        = var.provider
    aws_region      = var.aws_region
    project_name    = var.project_name
    environment     = var.environment
  }
}
