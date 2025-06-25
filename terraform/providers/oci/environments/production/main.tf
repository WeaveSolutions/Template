/**
 * Oracle Cloud Infrastructure (OCI) Production Environment Configuration
 */

terraform {
  required_version = ">= 1.0.0"
  
  backend "s3" {
    bucket                      = "tf-state-prod"
    key                         = "terraform/state"
    region                      = "us-phoenix-1"
    endpoint                    = "https://prod.compat.objectstorage.us-phoenix-1.oraclecloud.com"
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
  environment = "prod"
  freeform_tags = {
    "environment" = local.environment
    "project"     = var.project_name
    "managed-by"  = "terraform"
  }
  defined_tags = {
    "Operations.CostCenter" = "Production"
    "Security.Compliance"   = "HIPAA"
  }
}

# Networking Module
module "networking" {
  source = "../../modules/networking"
  
  compartment_id   = var.compartment_id
  environment      = local.environment
  vcn_display_name = "${var.project_name}-${local.environment}-vcn"
  vcn_dns_label    = "${var.project_name}${local.environment}"
  vcn_cidr_block   = "10.2.0.0/16"
  
  subnets = [
    {
      display_name = "app-subnet-1"
      cidr_block   = "10.2.1.0/24"
      dns_label    = "app1"
      is_public    = true
    },
    {
      display_name = "app-subnet-2"
      cidr_block   = "10.2.2.0/24"
      dns_label    = "app2"
      is_public    = true
    },
    {
      display_name = "db-subnet-1"
      cidr_block   = "10.2.3.0/24"
      dns_label    = "db1"
      is_public    = false
    },
    {
      display_name = "db-subnet-2"
      cidr_block   = "10.2.4.0/24"
      dns_label    = "db2"
      is_public    = false
    }
  ]
  
  freeform_tags = local.freeform_tags
  defined_tags  = local.defined_tags
}

# Database Module
module "database" {
  source = "../../modules/database"
  
  compartment_id  = var.compartment_id
  environment     = local.environment
  db_name         = "${var.project_name}${local.environment}db"
  
  subnet_id       = module.networking.subnet_ids["db-subnet-1"]
  cpu_core_count  = 4
  storage_size_in_tbs = 2
  admin_password  = var.db_admin_password
  is_auto_scaling_enabled = true
  is_data_guard_enabled = true
  standby_subnet_id = module.networking.subnet_ids["db-subnet-2"]
  
  freeform_tags   = local.freeform_tags
  defined_tags    = local.defined_tags
}

# Storage Module
module "storage" {
  source = "../../modules/storage"
  
  compartment_id  = var.compartment_id
  environment     = local.environment
  bucket_name     = "${var.project_name}-${local.environment}-storage"
  versioning      = true
  auto_tiering    = true
  retention_rules = true
  
  freeform_tags   = local.freeform_tags
  defined_tags    = local.defined_tags
}

# Compute Module (OKE - Oracle Kubernetes Engine)
module "compute" {
  source = "../../modules/compute"
  
  compartment_id  = var.compartment_id
  environment     = local.environment
  cluster_name    = "${var.project_name}-${local.environment}-cluster"
  
  vcn_id          = module.networking.vcn_id
  subnet_ids      = [module.networking.subnet_ids["app-subnet-1"], module.networking.subnet_ids["app-subnet-2"]]
  
  node_pool_size  = 5
  node_shape      = "VM.Standard.E4.Flex"
  node_ocpus      = 4
  node_memory_in_gbs = 32
  
  kubernetes_version = var.kubernetes_version
  is_auto_scaling_enabled = true
  
  freeform_tags   = local.freeform_tags
  defined_tags    = local.defined_tags
}

# Registry Module
module "registry" {
  source = "../../modules/registry"
  
  compartment_id  = var.compartment_id
  environment     = local.environment
  repository_prefix = "${var.project_name}-${local.environment}"
  
  freeform_tags   = local.freeform_tags
  defined_tags    = local.defined_tags
}

# Load Balancer Module
module "load_balancer" {
  source = "../../modules/load_balancer"
  
  compartment_id  = var.compartment_id
  environment     = local.environment
  lb_display_name = "${var.project_name}-${local.environment}-lb"
  
  subnet_ids      = [module.networking.subnet_ids["app-subnet-1"], module.networking.subnet_ids["app-subnet-2"]]
  backend_set_name = "${var.project_name}-${local.environment}-backend-set"
  is_private      = false
  shape           = "flexible"
  shape_min_mbps  = 100
  shape_max_mbps  = 1000
  
  freeform_tags   = local.freeform_tags
  defined_tags    = local.defined_tags
}

# Security Module
module "security" {
  source = "../../modules/security"
  
  compartment_id  = var.compartment_id
  environment     = local.environment
  
  vcn_id          = module.networking.vcn_id
  subnet_ids      = [
    module.networking.subnet_ids["app-subnet-1"],
    module.networking.subnet_ids["app-subnet-2"],
    module.networking.subnet_ids["db-subnet-1"],
    module.networking.subnet_ids["db-subnet-2"]
  ]
  
  enable_waf      = var.enable_waf
  
  freeform_tags   = local.freeform_tags
  defined_tags    = local.defined_tags
}
