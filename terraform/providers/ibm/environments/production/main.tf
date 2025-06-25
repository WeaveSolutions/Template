/**
 * IBM Cloud Production Environment Configuration
 */

terraform {
  required_version = ">= 1.0.0"
  
  backend "cos" {
    bucket                  = "tf-state-prod"
    endpoint_url            = "s3.us-east.cloud-object-storage.appdomain.cloud"
    key                     = "terraform/state"
    resource_instance_id    = "crn:v1:bluemix:public:cloud-object-storage:global:a/example:example::"
  }
}

provider "ibm" {
  region = var.region
}

locals {
  environment = "prod"
  tags = [
    "environment:${local.environment}",
    "project:${var.project_name}",
    "managed-by:terraform"
  ]
}

# Resource Group
resource "ibm_resource_group" "group" {
  name = "${var.project_name}-${local.environment}"
  tags = local.tags
}

# VPC Module
module "networking" {
  source = "../../modules/networking"
  
  environment       = local.environment
  resource_group_id = ibm_resource_group.group.id
  region            = var.region
  vpc_name          = "${var.project_name}-${local.environment}-vpc"
  
  subnets = [
    {
      name           = "app-subnet-1"
      zone           = "${var.region}-1"
      cidr           = "10.30.10.0/24"
      public_gateway = true
    },
    {
      name           = "app-subnet-2"
      zone           = "${var.region}-2"
      cidr           = "10.30.20.0/24"
      public_gateway = true
    },
    {
      name           = "db-subnet-1"
      zone           = "${var.region}-1"
      cidr           = "10.30.30.0/24"
      public_gateway = false
    },
    {
      name           = "db-subnet-2"
      zone           = "${var.region}-2"
      cidr           = "10.30.40.0/24"
      public_gateway = false
    }
  ]
  
  tags = local.tags
}

# Database Module
module "database" {
  source = "../../modules/database"
  
  environment       = local.environment
  resource_group_id = ibm_resource_group.group.id
  region            = var.region
  
  instance_name     = "${var.project_name}-${local.environment}-db"
  plan              = "enterprise"
  service           = "databases-for-postgresql"
  
  admin_password    = var.db_admin_password
  
  tags = local.tags
}

# Storage Module
module "storage" {
  source = "../../modules/storage"
  
  environment       = local.environment
  resource_group_id = ibm_resource_group.group.id
  region            = var.region
  
  bucket_name       = "${var.project_name}-${local.environment}-storage"
  
  tags = local.tags
}

# Kubernetes Module (for containerized applications)
module "kubernetes" {
  source = "../../modules/kubernetes"
  
  environment       = local.environment
  resource_group_id = ibm_resource_group.group.id
  region            = var.region
  
  cluster_name      = "${var.project_name}-${local.environment}-cluster"
  vpc_id            = module.networking.vpc_id
  subnet_ids        = [module.networking.subnet_ids[0], module.networking.subnet_ids[1]]
  worker_count      = 5
  flavor            = "bx2.8x32"
  
  tags = local.tags
}

# Monitoring Module
module "monitoring" {
  source = "../../modules/monitoring"
  
  environment       = local.environment
  resource_group_id = ibm_resource_group.group.id
  region            = var.region
  
  alert_recipients  = var.alert_recipients
  
  tags = local.tags
}

# Security Module
module "security" {
  source = "../../modules/security"
  
  environment       = local.environment
  resource_group_id = ibm_resource_group.group.id
  region            = var.region
  
  vpc_id            = module.networking.vpc_id
  
  tags = local.tags
}
