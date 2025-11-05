# Outputs for Haskell Servant API Service

output "service_name" {
  description = "Name of the Haskell API service"
  value       = var.enable_haskell_api ? "haskell-api" : null
}

output "service_url" {
  description = "Internal service URL for the Haskell API"
  value       = var.enable_haskell_api ? "http://haskell-api.${var.service_discovery_namespace_id}:8110" : null
}

output "ecs_service_arn" {
  description = "ARN of the ECS service"
  value       = var.enable_haskell_api ? aws_ecs_service.haskell_api[0].id : null
}

output "ecs_task_definition_arn" {
  description = "ARN of the ECS task definition"
  value       = var.enable_haskell_api ? aws_ecs_task_definition.haskell_api[0].arn : null
}

output "target_group_arn" {
  description = "ARN of the load balancer target group"
  value       = var.enable_haskell_api ? aws_lb_target_group.haskell_api[0].arn : null
}

output "security_group_id" {
  description = "ID of the security group"
  value       = var.enable_haskell_api ? aws_security_group.haskell_api[0].id : null
}

output "service_discovery_service_arn" {
  description = "ARN of the service discovery service"
  value       = var.enable_haskell_api ? aws_service_discovery_service.haskell_api[0].arn : null
}

output "cloudwatch_log_group_name" {
  description = "Name of the CloudWatch log group"
  value       = var.enable_haskell_api ? aws_cloudwatch_log_group.haskell_api[0].name : null
}

output "container_port" {
  description = "Container port for the Haskell API service"
  value       = var.enable_haskell_api ? 8110 : null
}

output "ecs_execution_role_arn" {
  description = "ARN of the ECS execution role"
  value       = var.enable_haskell_api ? aws_iam_role.ecs_execution_role[0].arn : null
}

output "ecs_task_role_arn" {
  description = "ARN of the ECS task role"
  value       = var.enable_haskell_api ? aws_iam_role.ecs_task_role[0].arn : null
}
