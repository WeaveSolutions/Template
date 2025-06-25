/**
 * Azure Staging Environment Configuration
 */

terraform {
  required_version = ">= 1.0.0"
  
  backend "azurerm" {
    resource_group_name  = "terraform-state-rg"
    storage_account_name = "tfstateaccount"
    container_name       = "tfstate"
    key                  = "staging.terraform.tfstate"
  }
}

provider "azurerm" {
  features {}
}

locals {
  environment = "staging"
  common_tags = {
    Environment = local.environment
    Project     = var.project_name
    ManagedBy   = "terraform"
  }
}

# Networking Module
module "networking" {
  source = "../../modules/networking"
  
  environment        = local.environment
  resource_group_name = "${var.project_name}-${local.environment}-rg"
  location           = var.location
  address_space      = ["10.1.0.0/16"]
  subnet_prefixes    = ["10.1.1.0/24", "10.1.2.0/24"]
  subnet_names       = ["app-subnet", "db-subnet"]
  
  tags = local.common_tags
}

# Database Module
module "database" {
  source = "../../modules/database"
  
  environment        = local.environment
  resource_group_name = "${var.project_name}-${local.environment}-rg"
  location           = var.location
  subnet_id          = module.networking.subnet_ids["db-subnet"]
  
  server_name        = "${var.project_name}${local.environment}sqlserver"
  admin_username     = var.db_admin_username
  admin_password     = var.db_admin_password
  
  tags = local.common_tags
}

# Storage Module
module "storage" {
  source = "../../modules/storage"
  
  environment        = local.environment
  resource_group_name = "${var.project_name}-${local.environment}-rg"
  location           = var.location
  account_name       = "${var.project_name}${local.environment}storage"
  
  tags = local.common_tags
}

# Compute Module
module "compute" {
  source = "../../modules/compute"
  
  environment        = local.environment
  resource_group_name = "${var.project_name}-${local.environment}-rg"
  location           = var.location
  subnet_id          = module.networking.subnet_ids["app-subnet"]
  
  app_service_plan_name = "${var.project_name}-${local.environment}-plan"
  app_service_name      = "${var.project_name}-${local.environment}-app"
  
  tags = local.common_tags
}
