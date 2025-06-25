/**
 * # IBM Cloud Provider Configuration
 *
 * This is the main entry point for the IBM Cloud provider in the multi-cloud infrastructure.
 * It configures various IBM Cloud services including VPC, Kubernetes, Power VS, and more.
 */

terraform {
  required_providers {
    ibm = {
      source  = "IBM-Cloud/ibm"
      version = "~> 1.45.0"
    }
  }
}

provider "ibm" {
  ibmcloud_api_key = var.ibmcloud_api_key
  region           = var.region
  zone             = var.zone
  generation       = 2
}

# Resource Group
resource "ibm_resource_group" "resource_group" {
  name = "${var.project_name}-${var.environment}"
  tags = var.tags
}

# Networking Module
module "networking" {
  source = "./modules/networking"
  
  # Pass through common variables
  resource_group_id = ibm_resource_group.resource_group.id
  region            = var.region
  tags              = var.tags
  
  # VPC Configuration
  vpc_name          = var.vpc_name
  vpc_subnets       = var.vpc_subnets
  vpc_acls          = var.vpc_acls
  vpc_security_groups = var.vpc_security_groups
}

# Compute Module (Virtual Servers)
module "compute" {
  source = "./modules/compute"
  
  # Pass through common variables
  resource_group_id = ibm_resource_group.resource_group.id
  region            = var.region
  tags              = var.tags
  
  # Instance Configuration
  vpc_id            = module.networking.vpc_id
  subnet_ids        = module.networking.subnet_ids
  security_group_ids = module.networking.security_group_ids
  
  instances         = var.instances
  ssh_keys          = var.ssh_keys
}

# Kubernetes Service
module "kubernetes" {
  source = "./modules/kubernetes"
  count  = var.enable_kubernetes ? 1 : 0
  
  # Pass through common variables
  resource_group_id = ibm_resource_group.resource_group.id
  region            = var.region
  tags              = var.tags
  
  # Cluster Configuration
  vpc_id            = module.networking.vpc_id
  subnet_ids        = module.networking.subnet_ids
  cluster_name      = var.kubernetes_cluster_name
  worker_count      = var.kubernetes_worker_count
  machine_type      = var.kubernetes_machine_type
  kube_version      = var.kubernetes_version
  flavor            = var.kubernetes_flavor
}

# Storage Module
module "storage" {
  source = "./modules/storage"
  
  # Pass through common variables
  resource_group_id = ibm_resource_group.resource_group.id
  region            = var.region
  tags              = var.tags
  
  # Storage Configuration
  cos_instances     = var.cos_instances
  block_volumes     = var.block_volumes
  file_shares       = var.file_shares
}

# Database Module
module "database" {
  source = "./modules/database"
  count  = var.enable_databases ? 1 : 0
  
  # Pass through common variables
  resource_group_id = ibm_resource_group.resource_group.id
  region            = var.region
  tags              = var.tags
  
  # Database Configuration
  db_instances      = var.db_instances
}

# Cloudant NoSQL Database
module "cloudant" {
  source = "./modules/cloudant"
  count  = var.enable_cloudant ? 1 : 0
  
  # Pass through common variables
  resource_group_id = ibm_resource_group.resource_group.id
  region            = var.region
  tags              = var.tags
  
  # Cloudant Configuration
  cloudant_instances = var.cloudant_instances
}

# Power VS (Power Virtual Servers)
module "power_vs" {
  source = "./modules/power_vs"
  count  = var.enable_power_vs ? 1 : 0
  
  # Pass through common variables
  resource_group_id = ibm_resource_group.resource_group.id
  zone              = var.power_vs_zone
  tags              = var.tags
  
  # Power VS Configuration
  power_vs_instances = var.power_vs_instances
  power_vs_networks  = var.power_vs_networks
  power_vs_volumes   = var.power_vs_volumes
}

# Security Module
module "security" {
  source = "./modules/security"
  
  # Pass through common variables
  resource_group_id = ibm_resource_group.resource_group.id
  region            = var.region
  tags              = var.tags
  
  # Security Configuration
  kms_instances     = var.kms_instances
  certificates      = var.certificates
  security_groups   = var.security_groups
}

# Schematics (IBM Cloud's Terraform as a Service)
module "schematics" {
  source = "./modules/schematics"
  count  = var.enable_schematics ? 1 : 0
  
  # Pass through common variables
  resource_group_id = ibm_resource_group.resource_group.id
  region            = var.region
  tags              = var.tags
  
  # Schematics Configuration
  workspace_name    = var.schematics_workspace_name
  template_repo_url = var.schematics_template_repo_url
  template_git_branch = var.schematics_template_git_branch
  template_git_token = var.schematics_template_git_token
}
