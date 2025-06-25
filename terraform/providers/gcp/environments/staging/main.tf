/**
 * Google Cloud Platform Staging Environment Configuration
 */

terraform {
  required_version = ">= 1.0.0"
  
  backend "gcs" {
    bucket = "tf-state-staging"
    prefix = "terraform/state"
  }
}

provider "google" {
  project = var.project_id
  region  = var.region
  zone    = var.zone
}

locals {
  environment = "staging"
  common_labels = {
    environment = local.environment
    project     = var.project_name
    managed_by  = "terraform"
  }
}

# Networking Module
module "networking" {
  source = "../../modules/networking"
  
  project_id  = var.project_id
  environment = local.environment
  region      = var.region
  network_name = "${var.project_name}-${local.environment}-vpc"
  subnets = [
    {
      name          = "app-subnet"
      ip_cidr_range = "10.1.0.0/24"
      region        = var.region
    },
    {
      name          = "db-subnet"
      ip_cidr_range = "10.1.1.0/24"
      region        = var.region
    }
  ]
  
  labels = local.common_labels
}

# Database Module
module "database" {
  source = "../../modules/database"
  
  project_id  = var.project_id
  environment = local.environment
  region      = var.region
  network     = module.networking.network_name
  
  instance_name = "${var.project_name}-${local.environment}-db"
  database_version = "POSTGRES_13"
  tier           = "db-g1-small"
  
  db_name        = "${var.project_name}_${local.environment}_db"
  db_user        = var.db_username
  db_password    = var.db_password
  
  labels = local.common_labels
}

# Storage Module
module "storage" {
  source = "../../modules/storage"
  
  project_id  = var.project_id
  environment = local.environment
  region      = var.region
  
  bucket_name = "${var.project_id}-${local.environment}-storage"
  
  labels = local.common_labels
}

# Compute Module
module "compute" {
  source = "../../modules/compute"
  
  project_id  = var.project_id
  environment = local.environment
  region      = var.region
  network     = module.networking.network_name
  subnet      = module.networking.subnet_names[0]
  
  service_name = "${var.project_name}-${local.environment}-app"
  
  labels = local.common_labels
}
