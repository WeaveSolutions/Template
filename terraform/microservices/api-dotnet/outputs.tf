# Outputs for .NET ASP.NET Core API Service

output "service_name" {
  description = "Name of the .NET API service"
  value       = var.enable_dotnet_api ? "dotnet-api" : null
}

output "service_url" {
  description = "Internal service URL for the .NET API"
  value       = var.enable_dotnet_api ? "http://dotnet-api.${var.service_discovery_namespace_id}:8100" : null
}

output "ecs_service_arn" {
  description = "ARN of the ECS service"
  value       = var.enable_dotnet_api ? aws_ecs_service.dotnet_api[0].id : null
}

output "ecs_task_definition_arn" {
  description = "ARN of the ECS task definition"
  value       = var.enable_dotnet_api ? aws_ecs_task_definition.dotnet_api[0].arn : null
}

output "target_group_arn" {
  description = "ARN of the load balancer target group"
  value       = var.enable_dotnet_api ? aws_lb_target_group.dotnet_api[0].arn : null
}

output "security_group_id" {
  description = "ID of the security group"
  value       = var.enable_dotnet_api ? aws_security_group.dotnet_api[0].id : null
}

output "service_discovery_service_arn" {
  description = "ARN of the service discovery service"
  value       = var.enable_dotnet_api ? aws_service_discovery_service.dotnet_api[0].arn : null
}

output "cloudwatch_log_group_name" {
  description = "Name of the CloudWatch log group"
  value       = var.enable_dotnet_api ? aws_cloudwatch_log_group.dotnet_api[0].name : null
}

output "container_port" {
  description = "Container port for the .NET API service"
  value       = var.enable_dotnet_api ? 8100 : null
}

output "ecs_execution_role_arn" {
  description = "ARN of the ECS execution role"
  value       = var.enable_dotnet_api ? aws_iam_role.ecs_execution_role[0].arn : null
}

output "ecs_task_role_arn" {
  description = "ARN of the ECS task role"
  value       = var.enable_dotnet_api ? aws_iam_role.ecs_task_role[0].arn : null
}
