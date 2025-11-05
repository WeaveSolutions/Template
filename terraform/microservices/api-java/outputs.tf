# Java Spring Boot API Service Outputs

# Service Information
output "service_name" {
  description = "Name of the Java Spring Boot API service"
  value       = var.enable_java_api ? aws_ecs_service.java_api[0].name : null
}

output "service_arn" {
  description = "ARN of the Java Spring Boot API service"
  value       = var.enable_java_api ? aws_ecs_service.java_api[0].id : null
}

output "task_definition_arn" {
  description = "ARN of the task definition"
  value       = var.enable_java_api ? aws_ecs_task_definition.java_api[0].arn : null
}

output "service_url" {
  description = "Service URL for Java Spring Boot API"
  value       = var.enable_java_api ? "https://${var.project_name}.example.com/api/v1/java" : null
}

# Network Information
output "security_group_id" {
  description = "Security group ID for Java Spring Boot API service"
  value       = var.enable_java_api ? aws_security_group.java_api[0].id : null
}

output "target_group_arn" {
  description = "Target group ARN for Java Spring Boot API service"
  value       = var.enable_java_api ? aws_lb_target_group.java_api[0].arn : null
}

output "listener_rule_arn" {
  description = "Listener rule ARN for Java Spring Boot API service"
  value       = var.enable_java_api ? aws_lb_listener_rule.java_api[0].arn : null
}

# Container Information
output "container_port" {
  description = "Container port for Java Spring Boot API service"
  value       = 8040
}

output "container_image" {
  description = "Container image used for Java Spring Boot API service"
  value       = var.container_image
}

# IAM Role Information
output "execution_role_arn" {
  description = "ECS execution role ARN"
  value       = var.enable_java_api ? aws_iam_role.ecs_execution_role[0].arn : null
}

output "task_role_arn" {
  description = "ECS task role ARN"
  value       = var.enable_java_api ? aws_iam_role.ecs_task_role[0].arn : null
}

# Logging Information
output "log_group_name" {
  description = "CloudWatch log group name"
  value       = var.enable_java_api ? aws_cloudwatch_log_group.java_api[0].name : null
}

output "log_group_arn" {
  description = "CloudWatch log group ARN"
  value       = var.enable_java_api ? aws_cloudwatch_log_group.java_api[0].arn : null
}

# Auto Scaling Information
output "autoscaling_target_resource_id" {
  description = "Auto scaling target resource ID"
  value       = var.enable_java_api && var.enable_auto_scaling ? aws_appautoscaling_target.java_api[0].resource_id : null
}

output "autoscaling_cpu_policy_arn" {
  description = "Auto scaling CPU policy ARN"
  value       = var.enable_java_api && var.enable_auto_scaling ? aws_appautoscaling_policy.java_api_cpu[0].arn : null
}

output "autoscaling_memory_policy_arn" {
  description = "Auto scaling memory policy ARN"
  value       = var.enable_java_api && var.enable_auto_scaling ? aws_appautoscaling_policy.java_api_memory[0].arn : null
}

# Service Discovery Information
output "service_discovery_service_arn" {
  description = "Service discovery service ARN"
  value       = var.enable_java_api && var.service_discovery_namespace_id != "" ? aws_service_discovery_service.java_api[0].arn : null
}

# Configuration Information
output "service_configuration" {
  description = "Service configuration details"
  value = var.enable_java_api ? {
    service_name    = aws_ecs_service.java_api[0].name
    cluster_name    = var.ecs_cluster_name
    desired_count   = var.desired_count
    cpu             = var.cpu
    memory          = var.memory
    container_port  = 8040
    health_check_path = "/actuator/health"
    auto_scaling_enabled = var.enable_auto_scaling
    min_capacity    = var.min_capacity
    max_capacity    = var.max_capacity
  } : null
}

# Health Check Information
output "health_check_url" {
  description = "Health check URL for Java Spring Boot API service"
  value       = var.enable_java_api ? "https://${var.project_name}.example.com/api/v1/java/actuator/health" : null
}

# Monitoring Information
output "service_metrics" {
  description = "Service metrics information"
  value = var.enable_java_api ? {
    cpu_alarm_threshold    = var.alarm_cpu_threshold
    memory_alarm_threshold = var.alarm_memory_threshold
    log_retention_days     = var.log_retention_days
    monitoring_enabled     = var.enable_monitoring
  } : null
}

# Development Information
output "development_info" {
  description = "Development and debugging information"
  value = var.enable_java_api ? {
    local_port           = 8040
    framework           = "Spring Boot"
    language            = "Java"
    build_tool          = "Maven/Gradle"
    docker_image        = var.container_image
    execution_command   = var.enable_execution_command
    container_insights  = var.enable_container_insights
    jvm_options         = var.java_opts
    spring_profiles     = var.spring_profiles
  } : null
}

# Environment Variables
output "environment_variables" {
  description = "Environment variables used by the service"
  value = var.enable_java_api ? {
    SPRING_PROFILES_ACTIVE = var.environment
    SERVER_PORT           = "8040"
    AUTH0_DOMAIN          = var.auth0_domain
    AUTH0_AUDIENCE        = var.auth0_audience
    MINDSDB_URL           = var.mindsdb_url
    KONG_ADMIN_URL        = var.kong_admin_url
    POSTHOG_HOST          = var.posthog_host
    DATABASE_URL          = var.database_url != "" ? "[CONFIGURED]" : "[NOT_SET]"
    REDIS_URL             = var.redis_url != "" ? "[CONFIGURED]" : "[NOT_SET]"
    JAVA_OPTS             = var.java_opts
  } : null
  sensitive = true
}

# Actuator Information
output "actuator_endpoints" {
  description = "Spring Boot Actuator endpoints"
  value = var.enable_java_api ? {
    health_endpoint     = "https://${var.project_name}.example.com/api/v1/java/actuator/health"
    info_endpoint       = "https://${var.project_name}.example.com/api/v1/java/actuator/info"
    metrics_endpoint    = "https://${var.project_name}.example.com/api/v1/java/actuator/metrics"
    prometheus_endpoint = "https://${var.project_name}.example.com/api/v1/java/actuator/prometheus"
    endpoints_enabled   = var.management_endpoints_enabled
    security_enabled    = var.management_security_enabled
  } : null
}

# Terraform State Information
output "terraform_state_info" {
  description = "Terraform state information for this service"
  value = {
    service_enabled = var.enable_java_api
    provider        = var.provider
    aws_region      = var.aws_region
    project_name    = var.project_name
    environment     = var.environment
  }
}
