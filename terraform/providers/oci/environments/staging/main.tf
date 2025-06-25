/**
 * Oracle Cloud Infrastructure (OCI) Staging Environment Configuration
 */

terraform {
  required_version = ">= 1.0.0"
  
  backend "s3" {
    bucket                      = "tf-state-staging"
    key                         = "terraform/state"
    region                      = "us-phoenix-1"
    endpoint                    = "https://staging.compat.objectstorage.us-phoenix-1.oraclecloud.com"
    skip_region_validation      = true
    skip_credentials_validation = true
    skip_metadata_api_check     = true
    force_path_style            = true
  }
}

provider "oci" {
  region = var.region
}

locals {
  environment = "staging"
  freeform_tags = {
    "environment" = local.environment
    "project"     = var.project_name
    "managed-by"  = "terraform"
  }
}

# Networking Module
module "networking" {
  source = "../../modules/networking"
  
  compartment_id  = var.compartment_id
  environment     = local.environment
  vcn_display_name = "${var.project_name}-${local.environment}-vcn"
  vcn_dns_label   = "${var.project_name}${local.environment}"
  vcn_cidr_block  = "10.1.0.0/16"
  
  subnets = [
    {
      display_name = "app-subnet"
      cidr_block   = "10.1.1.0/24"
      dns_label    = "app"
      is_public    = true
    },
    {
      display_name = "db-subnet"
      cidr_block   = "10.1.2.0/24"
      dns_label    = "db"
      is_public    = false
    }
  ]
  
  freeform_tags = local.freeform_tags
}

# Database Module
module "database" {
  source = "../../modules/database"
  
  compartment_id  = var.compartment_id
  environment     = local.environment
  db_name         = "${var.project_name}${local.environment}db"
  
  subnet_id       = module.networking.subnet_ids["db-subnet"]
  cpu_core_count  = 2
  storage_size_in_tbs = 1
  admin_password  = var.db_admin_password
  
  freeform_tags   = local.freeform_tags
}

# Storage Module
module "storage" {
  source = "../../modules/storage"
  
  compartment_id  = var.compartment_id
  environment     = local.environment
  bucket_name     = "${var.project_name}-${local.environment}-storage"
  
  freeform_tags   = local.freeform_tags
}

# Compute Module (OKE - Oracle Kubernetes Engine)
module "compute" {
  source = "../../modules/compute"
  
  compartment_id  = var.compartment_id
  environment     = local.environment
  cluster_name    = "${var.project_name}-${local.environment}-cluster"
  
  vcn_id          = module.networking.vcn_id
  subnet_ids      = [module.networking.subnet_ids["app-subnet"]]
  
  node_pool_size  = 3
  node_shape      = "VM.Standard.E3.Flex"
  node_ocpus      = 2
  node_memory_in_gbs = 16
  
  kubernetes_version = var.kubernetes_version
  
  freeform_tags   = local.freeform_tags
}

# Registry Module
module "registry" {
  source = "../../modules/registry"
  
  compartment_id  = var.compartment_id
  environment     = local.environment
  repository_prefix = "${var.project_name}-${local.environment}"
  
  freeform_tags   = local.freeform_tags
}

# Load Balancer Module
module "load_balancer" {
  source = "../../modules/load_balancer"
  
  compartment_id  = var.compartment_id
  environment     = local.environment
  lb_display_name = "${var.project_name}-${local.environment}-lb"
  
  subnet_ids      = [module.networking.subnet_ids["app-subnet"]]
  backend_set_name = "${var.project_name}-${local.environment}-backend-set"
  
  freeform_tags   = local.freeform_tags
}
