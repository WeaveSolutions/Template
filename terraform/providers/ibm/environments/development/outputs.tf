output "resource_group_id" {
  description = "The ID of the resource group"
  value       = ibm_resource_group.group.id
}

output "vpc_id" {
  description = "The ID of the VPC"
  value       = module.networking.vpc_id
}

output "subnet_ids" {
  description = "The IDs of the subnets"
  value       = module.networking.subnet_ids
}

output "database_id" {
  description = "The ID of the database instance"
  value       = module.database.database_id
}

output "database_connection_string" {
  description = "The connection string for the database"
  value       = module.database.connection_string
  sensitive   = true
}

output "storage_bucket_name" {
  description = "The name of the storage bucket"
  value       = module.storage.bucket_name
}

output "storage_bucket_crn" {
  description = "The CRN of the storage bucket"
  value       = module.storage.bucket_crn
}

output "kubernetes_cluster_id" {
  description = "The ID of the Kubernetes cluster"
  value       = module.kubernetes.cluster_id
}

output "kubernetes_config" {
  description = "The kubeconfig for the Kubernetes cluster"
  value       = module.kubernetes.cluster_config
  sensitive   = true
}
