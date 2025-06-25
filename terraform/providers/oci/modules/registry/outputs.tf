output "registry_endpoint" {
  description = "Container registry endpoint"
  value       = local.registry_endpoint
}

output "registry_namespace" {
  description = "Container registry namespace"
  value       = local.registry_namespace
}

output "next_app_repository" {
  description = "Next.js app repository URL"
  value       = "${local.registry_endpoint}/${local.registry_namespace}/${var.project_name}/next"
}

output "api_gateway_repository" {
  description = "API Gateway repository URL"
  value       = "${local.registry_endpoint}/${local.registry_namespace}/${var.project_name}/api-gateway"
}

output "auth_service_repository" {
  description = "Auth Service repository URL"
  value       = "${local.registry_endpoint}/${local.registry_namespace}/${var.project_name}/auth"
}

output "user_service_repository" {
  description = "User Service repository URL"
  value       = "${local.registry_endpoint}/${local.registry_namespace}/${var.project_name}/user"
}

output "notification_service_repository" {
  description = "Notification Service repository URL"
  value       = "${local.registry_endpoint}/${local.registry_namespace}/${var.project_name}/notifications"
}

output "function_repositories" {
  description = "Function repository URLs"
  value = {
    for k, v in oci_artifacts_container_repository.functions : 
    k => "${local.registry_endpoint}/${local.registry_namespace}/${v.display_name}"
  }
}

output "developers_group_id" {
  description = "Developers group OCID"
  value       = oci_identity_group.developers.id
}

output "cicd_group_id" {
  description = "CI/CD group OCID"
  value       = oci_identity_group.cicd.id
}

output "nodes_dynamic_group_id" {
  description = "Nodes dynamic group OCID"
  value       = oci_identity_dynamic_group.nodes.id
}
