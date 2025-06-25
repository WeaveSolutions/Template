output "vcn_id" {
  description = "The OCID of the VCN"
  value       = module.networking.vcn_id
}

output "subnet_ids" {
  description = "The OCIDs of the subnets"
  value       = module.networking.subnet_ids
}

output "database_id" {
  description = "The OCID of the Autonomous Database"
  value       = module.database.database_id
}

output "database_connection_string" {
  description = "The connection string for the database"
  value       = module.database.connection_string
  sensitive   = true
}

output "database_wallet_password" {
  description = "The password for the database wallet"
  value       = module.database.wallet_password
  sensitive   = true
}

output "database_standby_id" {
  description = "The OCID of the standby Autonomous Database"
  value       = module.database.standby_database_id
}

output "storage_bucket_name" {
  description = "The name of the storage bucket"
  value       = module.storage.bucket_name
}

output "storage_bucket_namespace" {
  description = "The namespace of the storage bucket"
  value       = module.storage.bucket_namespace
}

output "oke_cluster_id" {
  description = "The OCID of the OKE cluster"
  value       = module.compute.cluster_id
}

output "oke_cluster_endpoint" {
  description = "The endpoint of the OKE cluster"
  value       = module.compute.cluster_endpoint
}

output "oke_kubeconfig" {
  description = "The kubeconfig for the OKE cluster"
  value       = module.compute.kubeconfig
  sensitive   = true
}

output "registry_repository_urls" {
  description = "The URLs of the container registry repositories"
  value       = module.registry.repository_urls
}

output "load_balancer_id" {
  description = "The OCID of the load balancer"
  value       = module.load_balancer.load_balancer_id
}

output "load_balancer_ip" {
  description = "The IP address of the load balancer"
  value       = module.load_balancer.load_balancer_ip
}

output "security_policies" {
  description = "The OCIDs of the security policies"
  value       = module.security.security_policy_ids
}

output "waf_policy_id" {
  description = "The OCID of the WAF policy"
  value       = var.enable_waf ? module.security.waf_policy_id : null
}
