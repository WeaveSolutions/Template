# Outputs for PHP Laravel API Service Infrastructure

output "service_name" {
  description = "Name of the PHP API service"
  value       = var.enable_php_api ? "php-api" : null
}

output "service_id" {
  description = "ECS service ID"
  value       = var.enable_php_api ? aws_ecs_service.php_api[0].id : null
}

output "service_arn" {
  description = "ECS service ARN"
  value       = var.enable_php_api ? aws_ecs_service.php_api[0].id : null
}

output "task_definition_arn" {
  description = "ECS task definition ARN"
  value       = var.enable_php_api ? aws_ecs_task_definition.php_api[0].arn : null
}

output "task_definition_revision" {
  description = "ECS task definition revision"
  value       = var.enable_php_api ? aws_ecs_task_definition.php_api[0].revision : null
}

output "container_port" {
  description = "Container port"
  value       = 8100
}

output "service_url" {
  description = "Service URL for internal communication"
  value       = var.enable_php_api ? "http://php-api.${var.service_discovery_namespace_name}:8100" : null
}

output "api_base_path" {
  description = "API base path for load balancer routing"
  value       = "/api/v1/php"
}

output "health_check_path" {
  description = "Health check endpoint path"
  value       = "/health"
}

output "target_group_arn" {
  description = "Load balancer target group ARN"
  value       = var.enable_php_api ? aws_lb_target_group.php_api[0].arn : null
}

output "security_group_id" {
  description = "Security group ID"
  value       = var.enable_php_api ? aws_security_group.php_api[0].id : null
}

output "cloudwatch_log_group_name" {
  description = "CloudWatch log group name"
  value       = var.enable_php_api ? aws_cloudwatch_log_group.php_api_logs[0].name : null
}

output "cloudwatch_log_group_arn" {
  description = "CloudWatch log group ARN"
  value       = var.enable_php_api ? aws_cloudwatch_log_group.php_api_logs[0].arn : null
}

output "service_discovery_service_arn" {
  description = "Service discovery service ARN"
  value       = var.enable_php_api ? aws_service_discovery_service.php_api[0].arn : null
}

output "ecs_execution_role_arn" {
  description = "ECS execution role ARN"
  value       = var.enable_php_api ? aws_iam_role.ecs_execution_role[0].arn : null
}

output "ecs_task_role_arn" {
  description = "ECS task role ARN"
  value       = var.enable_php_api ? aws_iam_role.ecs_task_role[0].arn : null
}

output "autoscaling_target_resource_id" {
  description = "Auto scaling target resource ID"
  value       = var.enable_php_api && var.enable_autoscaling ? aws_appautoscaling_target.php_api[0].resource_id : null
}

output "cpu_scaling_policy_arn" {
  description = "CPU-based auto scaling policy ARN"
  value       = var.enable_php_api && var.enable_autoscaling ? aws_appautoscaling_policy.php_api_cpu[0].arn : null
}

output "memory_scaling_policy_arn" {
  description = "Memory-based auto scaling policy ARN"
  value       = var.enable_php_api && var.enable_autoscaling ? aws_appautoscaling_policy.php_api_memory[0].arn : null
}

output "kong_service_config_parameter" {
  description = "Kong service configuration parameter name"
  value       = var.enable_php_api && var.enable_kong_integration ? aws_ssm_parameter.kong_service_config[0].name : null
}

output "posthog_integration_enabled" {
  description = "Whether PostHog integration is enabled"
  value       = var.enable_php_api && var.enable_posthog
}

output "deployment_status" {
  description = "Deployment status information"
  value = var.enable_php_api ? {
    enabled           = true
    service_name      = "php-api"
    container_port    = 8100
    api_path          = "/api/v1/php/*"
    health_check      = "/health"
    autoscaling       = var.enable_autoscaling
    kong_integration  = var.enable_kong_integration
    posthog_analytics = var.enable_posthog
    min_capacity      = var.enable_autoscaling ? var.min_capacity : var.desired_count
    max_capacity      = var.enable_autoscaling ? var.max_capacity : var.desired_count
    cpu_target        = var.enable_autoscaling ? var.cpu_target_value : null
    memory_target     = var.enable_autoscaling ? var.memory_target_value : null
  } : {
    enabled = false
  }
}

output "environment_variables" {
  description = "Environment variables configuration"
  value = var.enable_php_api ? {
    APP_ENV         = var.environment
    APP_DEBUG       = var.environment == "development" ? "true" : "false"
    DB_CONNECTION   = "pgsql"
    DB_HOST         = var.database_host
    DB_DATABASE     = var.database_name
    DB_USERNAME     = var.database_username
    CACHE_DRIVER    = "redis"
    SESSION_DRIVER  = "redis"
    QUEUE_CONNECTION = "redis"
    REDIS_HOST      = var.redis_host
    AUTH0_DOMAIN    = var.auth0_domain
    AUTH0_CLIENT_ID = var.auth0_client_id
    AUTH0_AUDIENCE  = var.auth0_audience
    MINDSDB_HOST    = var.mindsdb_host
    KONG_GATEWAY_URL = var.kong_gateway_url
    POSTHOG_HOST    = var.posthog_host
  } : null
}

output "secrets_configuration" {
  description = "Secrets configuration"
  value = var.enable_php_api ? {
    database_password_secret = var.database_password_secret_arn
    auth0_client_secret      = var.auth0_client_secret_arn
    redis_password_secret    = var.redis_password_secret_arn
    laravel_app_key         = "*** SENSITIVE ***"
    posthog_api_key         = "*** SENSITIVE ***"
  } : null
}

output "integration_endpoints" {
  description = "Integration endpoints and URLs"
  value = var.enable_php_api ? {
    service_internal_url = "http://php-api.${var.service_discovery_namespace_name}:8100"
    api_external_path    = "/api/v1/php/*"
    health_check_url     = "http://php-api.${var.service_discovery_namespace_name}:8100/health"
    kong_upstream_url    = "http://php-api.${var.service_discovery_namespace_name}:8100"
    swagger_docs_url     = "http://php-api.${var.service_discovery_namespace_name}:8100/api/documentation"
  } : null
}
