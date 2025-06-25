output "vcn_id" {
  description = "VCN OCID"
  value       = module.networking.vcn_id
}

output "cluster_id" {
  description = "OKE Cluster OCID"
  value       = module.compute.cluster_id
}

output "cluster_endpoint" {
  description = "OKE Cluster endpoint"
  value       = module.compute.cluster_endpoint
  sensitive   = true
}

output "kubeconfig_path" {
  description = "Path to kubeconfig file"
  value       = module.compute.kubeconfig_path
}

output "load_balancer_ip" {
  description = "Load balancer public IP"
  value       = module.load_balancer.load_balancer_ip
}

output "registry_endpoint" {
  description = "Container registry endpoint"
  value       = module.registry.registry_endpoint
}

output "registry_namespace" {
  description = "Container registry namespace"
  value       = module.registry.registry_namespace
}

output "database_connection_url" {
  description = "Database connection URL"
  value       = module.database.connection_url
  sensitive   = true
}

output "object_storage_namespace" {
  description = "Object storage namespace"
  value       = module.storage.namespace
}

output "bucket_names" {
  description = "Created bucket names"
  value       = module.storage.bucket_names
}

output "vault_id" {
  description = "Vault OCID"
  value       = module.security.vault_id
}

output "auth0_config_file" {
  description = "Path to Auth0 configuration file"
  value       = local_file.auth0_config.filename
}

output "k8s_deployment_file" {
  description = "Path to Kubernetes deployment file"
  value       = local_file.k8s_deployments.filename
}

output "api_gateway_url" {
  description = "API Gateway URL"
  value       = "http://${module.compute.api_gateway_private_ip}:3000"
}

output "auth_service_url" {
  description = "Auth Service URL"
  value       = "http://${module.compute.auth_service_private_ip}:8000"
}

output "app_url" {
  description = "Application URL"
  value       = "https://${module.load_balancer.load_balancer_ip}"
}
