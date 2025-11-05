# Outputs for C++ Drogon API Service

# Service Information
output "service_name" {
  description = "Name of the C++ API service"
  value       = var.enable_cpp_api ? "cpp-api" : null
}

output "service_arn" {
  description = "ARN of the ECS service"
  value       = var.enable_cpp_api ? aws_ecs_service.cpp_api[0].id : null
}

output "task_definition_arn" {
  description = "ARN of the ECS task definition"
  value       = var.enable_cpp_api ? aws_ecs_task_definition.cpp_api[0].arn : null
}

# Load Balancer Information
output "target_group_arn" {
  description = "ARN of the target group"
  value       = var.enable_cpp_api ? aws_lb_target_group.cpp_api[0].arn : null
}

output "target_group_name" {
  description = "Name of the target group"
  value       = var.enable_cpp_api ? aws_lb_target_group.cpp_api[0].name : null
}

output "listener_rule_arn" {
  description = "ARN of the listener rule"
  value       = var.enable_cpp_api ? aws_lb_listener_rule.cpp_api[0].arn : null
}

# Service Discovery
output "service_discovery_service_arn" {
  description = "ARN of the service discovery service"
  value       = var.enable_cpp_api ? aws_service_discovery_service.cpp_api[0].arn : null
}

output "service_discovery_service_name" {
  description = "Name of the service discovery service"
  value       = var.enable_cpp_api ? aws_service_discovery_service.cpp_api[0].name : null
}

# CloudWatch Logs
output "log_group_name" {
  description = "Name of the CloudWatch log group"
  value       = var.enable_cpp_api ? aws_cloudwatch_log_group.cpp_api[0].name : null
}

output "log_group_arn" {
  description = "ARN of the CloudWatch log group"
  value       = var.enable_cpp_api ? aws_cloudwatch_log_group.cpp_api[0].arn : null
}

# IAM Roles
output "execution_role_arn" {
  description = "ARN of the ECS execution role"
  value       = var.enable_cpp_api ? aws_iam_role.ecs_execution_role[0].arn : null
}

output "task_role_arn" {
  description = "ARN of the ECS task role"
  value       = var.enable_cpp_api ? aws_iam_role.ecs_task_role[0].arn : null
}

# Auto Scaling
output "autoscaling_target_arn" {
  description = "ARN of the auto scaling target"
  value       = var.enable_cpp_api && var.enable_auto_scaling ? aws_appautoscaling_target.cpp_api[0].arn : null
}

output "autoscaling_policy_cpu_arn" {
  description = "ARN of the CPU auto scaling policy"
  value       = var.enable_cpp_api && var.enable_auto_scaling ? aws_appautoscaling_policy.cpp_api_cpu[0].arn : null
}

output "autoscaling_policy_memory_arn" {
  description = "ARN of the memory auto scaling policy"
  value       = var.enable_cpp_api && var.enable_auto_scaling ? aws_appautoscaling_policy.cpp_api_memory[0].arn : null
}

# Configuration Status
output "configuration_status" {
  description = "Configuration status of the C++ API service"
  value = var.enable_cpp_api ? {
    service_enabled          = true
    container_image         = var.container_image
    cpu                     = var.cpu
    memory                  = var.memory
    desired_count           = var.desired_count
    auto_scaling_enabled    = var.enable_auto_scaling
    health_check_path       = var.health_check_path
    log_retention_days      = var.log_retention_days
    environment             = var.environment
    aws_region              = var.aws_region
  } : {}
}

# Environment Variables
output "environment_variables" {
  description = "Environment variables for the C++ API service"
  value = var.enable_cpp_api ? {
    PORT                     = "8110"
    SERVICE_NAME            = "cpp-api"
    NODE_ENV                = var.environment
    DATABASE_URL            = var.database_url
    REDIS_URL               = var.redis_url
    AUTH0_DOMAIN            = var.auth0_domain
    AUTH0_AUDIENCE          = var.auth0_audience
    KONG_ADMIN_URL          = var.kong_admin_url
  } : {}
}

# API Endpoints
output "api_endpoints" {
  description = "API endpoints for the C++ service"
  value = var.enable_cpp_api ? {
    base_path    = "/api/v1/cpp"
    health_check = "http://localhost:8110${var.health_check_path}"
    swagger_ui   = "http://localhost:8110/docs"
  } : {}
}

# Performance Metrics
output "performance_configuration" {
  description = "Performance configuration for the C++ API service"
  value = var.enable_cpp_api ? {
    drogon_thread_num           = var.drogon_thread_num
    drogon_listener_count       = var.drogon_listener_count
    drogon_enable_compression   = var.drogon_enable_compression
    drogon_session_timeout      = var.drogon_session_timeout
    drogon_max_connections      = var.drogon_max_connections
    drogon_db_connection_number = var.drogon_db_connection_number
    auto_scaling_target_cpu     = var.auto_scaling_target_cpu
    auto_scaling_target_memory  = var.auto_scaling_target_memory
  } : {}
}

# Network Configuration
output "network_configuration" {
  description = "Network configuration for the C++ API service"
  value = var.enable_cpp_api ? {
    vpc_id              = var.vpc_id
    subnet_ids          = var.subnet_ids
    security_group_ids  = var.security_group_ids
    container_port      = 8110
    protocol            = "HTTP"
    path_pattern        = "/api/v1/cpp/*"
  } : {}
}

# Kong Gateway Configuration
output "kong_configuration" {
  description = "Kong API Gateway configuration"
  value = var.enable_cpp_api ? {
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

# PostHog Analytics
output "posthog_configuration" {
  description = "PostHog analytics configuration"
  value = var.enable_cpp_api ? {
    enabled                        = var.enable_posthog
    api_key_configured            = var.posthog_api_key != ""
    host                          = var.posthog_host
    project_id                    = var.posthog_project_id
    capture_pageviews             = var.posthog_capture_pageviews
    capture_pageleaves            = var.posthog_capture_pageleaves
    disable_session_recording     = var.posthog_disable_session_recording
    opt_out_capturing            = var.posthog_opt_out_capturing
    anonymize_ips                = var.posthog_anonymize_ips
    batch_size                   = var.posthog_batch_size
    request_timeout              = var.posthog_request_timeout
    custom_events                = var.posthog_custom_events
  } : {}
}

# PostHog Module Output
output "posthog_module_enabled" {
  description = "Whether PostHog module is enabled"
  value       = var.enable_posthog && var.enable_cpp_api
}

# Security Configuration
output "security_configuration" {
  description = "Security configuration for the C++ API service"
  value = var.enable_cpp_api ? {
    auth0_domain_configured      = var.auth0_domain != ""
    auth0_audience_configured    = var.auth0_audience != ""
    jwt_secret_configured        = var.jwt_secret_arn != ""
    database_password_configured = var.database_password_arn != ""
    waf_enabled                  = var.enable_waf
    vpc_endpoints_enabled        = var.enable_vpc_endpoints
    encryption_at_rest_enabled   = var.enable_encryption_at_rest
    encryption_in_transit_enabled = var.enable_encryption_in_transit
  } : {}
}

# Monitoring Configuration
output "monitoring_configuration" {
  description = "Monitoring configuration for the C++ API service"
  value = var.enable_cpp_api ? {
    detailed_monitoring_enabled = var.enable_detailed_monitoring
    xray_tracing_enabled       = var.enable_xray_tracing
    cloudwatch_insights_enabled = var.enable_cloudwatch_insights
    log_group_name             = var.enable_cpp_api ? aws_cloudwatch_log_group.cpp_api[0].name : null
    log_retention_days         = var.log_retention_days
  } : {}
}

# Integration Configuration
output "integration_configuration" {
  description = "External integration configuration"
  value = var.enable_cpp_api ? {
    kong_admin_url_configured   = var.kong_admin_url != ""
    webhook_endpoint_configured = var.webhook_endpoint != ""
    spot_instances_enabled      = var.enable_spot_instances
    spot_instance_percentage    = var.spot_instance_percentage
  } : {}
}

# Drogon Framework Configuration
output "drogon_framework_configuration" {
  description = "Drogon framework specific configuration"
  value = var.enable_cpp_api ? {
    framework_name            = "drogon"
    language                  = "cpp"
    thread_num                = var.drogon_thread_num
    listener_count            = var.drogon_listener_count
    enable_compression        = var.drogon_enable_compression
    session_timeout           = var.drogon_session_timeout
    max_connections           = var.drogon_max_connections
    db_connection_number      = var.drogon_db_connection_number
    container_port            = 8110
    health_check_path         = var.health_check_path
  } : {}
  sensitive = true
}
