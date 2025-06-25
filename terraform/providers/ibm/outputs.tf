output "resource_group" {
  description = "The resource group created for this environment"
  value       = ibm_resource_group.resource_group
}

# VPC and Networking Outputs
output "vpc" {
  description = "The VPC configuration"
  value       = module.networking.vpc
}

output "subnets" {
  description = "The subnets created in the VPC"
  value       = module.networking.subnets
}

output "security_groups" {
  description = "The security groups created in the VPC"
  value       = module.networking.security_groups
}

# Compute Outputs
output "instances" {
  description = "The virtual server instances created"
  value       = module.compute.instances
}

output "instance_ips" {
  description = "The IP addresses of the virtual server instances"
  value       = module.compute.instance_ips
}

output "ssh_keys" {
  description = "The SSH keys created in IBM Cloud"
  value       = module.compute.ssh_keys
}

# Kubernetes Outputs
output "kubernetes_cluster" {
  description = "The Kubernetes cluster configuration"
  value       = var.enable_kubernetes ? module.kubernetes[0].cluster : null
}

output "kubernetes_config" {
  description = "The Kubernetes cluster configuration file path"
  value       = var.enable_kubernetes ? module.kubernetes[0].cluster_config_file_path : null
  sensitive   = true
}

# Storage Outputs
output "cos_instances" {
  description = "The Cloud Object Storage instances created"
  value       = module.storage.cos_instances
}

output "cos_buckets" {
  description = "The Cloud Object Storage buckets created"
  value       = module.storage.cos_buckets
}

output "block_volumes" {
  description = "The block storage volumes created"
  value       = module.storage.block_volumes
}

output "file_shares" {
  description = "The file storage shares created"
  value       = module.storage.file_shares
}

# Database Outputs
output "database_instances" {
  description = "The database instances created"
  value       = var.enable_databases ? module.database[0].db_instances : null
}

output "database_connection_strings" {
  description = "The connection strings for the database instances"
  value       = var.enable_databases ? module.database[0].connection_strings : null
  sensitive   = true
}

# Cloudant Outputs
output "cloudant_instances" {
  description = "The Cloudant NoSQL database instances created"
  value       = var.enable_cloudant ? module.cloudant[0].instances : null
}

output "cloudant_credentials" {
  description = "The Cloudant instance credentials"
  value       = var.enable_cloudant ? module.cloudant[0].credentials : null
  sensitive   = true
}

# Power VS Outputs
output "power_vs_instances" {
  description = "The Power VS instances created"
  value       = var.enable_power_vs ? module.power_vs[0].instances : null
}

output "power_vs_networks" {
  description = "The Power VS networks created"
  value       = var.enable_power_vs ? module.power_vs[0].networks : null
}

output "power_vs_volumes" {
  description = "The Power VS volumes created"
  value       = var.enable_power_vs ? module.power_vs[0].volumes : null
}

# Security Outputs
output "kms_instances" {
  description = "The Key Management Service instances created"
  value       = module.security.kms_instances
}

output "kms_keys" {
  description = "The encryption keys created in Key Management Service"
  value       = module.security.kms_keys
}

output "certificates" {
  description = "The SSL certificates managed"
  value       = module.security.certificates
}

# Schematics Outputs
output "schematics_workspace" {
  description = "The Schematics workspace created"
  value       = var.enable_schematics ? module.schematics[0].workspace : null
}

output "schematics_template" {
  description = "The Schematics template details"
  value       = var.enable_schematics ? module.schematics[0].template : null
}

# Summary Output
output "ibm_cloud_summary" {
  description = "Summary of all IBM Cloud resources created"
  value = {
    resource_group_name = ibm_resource_group.resource_group.name
    vpc_name            = module.networking.vpc.name
    subnet_count        = length(module.networking.subnets)
    instance_count      = length(module.compute.instances)
    kubernetes_enabled  = var.enable_kubernetes
    kubernetes_cluster  = var.enable_kubernetes ? module.kubernetes[0].cluster.name : null
    cos_instances       = length(module.storage.cos_instances)
    block_volumes       = length(module.storage.block_volumes)
    databases_enabled   = var.enable_databases
    cloudant_enabled    = var.enable_cloudant
    power_vs_enabled    = var.enable_power_vs
    region              = var.region
  }
}
