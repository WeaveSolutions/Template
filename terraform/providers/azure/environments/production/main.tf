/**
 * Azure Production Environment Configuration
 */

terraform {
  required_version = ">= 1.0.0"
  
  backend "azurerm" {
    resource_group_name  = "terraform-state-rg"
    storage_account_name = "tfstateaccount"
    container_name       = "tfstate"
    key                  = "prod.terraform.tfstate"
  }
}

provider "azurerm" {
  features {}
}

locals {
  environment = "prod"
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
  address_space      = ["10.2.0.0/16"]
  subnet_prefixes    = ["10.2.1.0/24", "10.2.2.0/24", "10.2.3.0/24"]
  subnet_names       = ["app-subnet", "db-subnet", "private-endpoints-subnet"]
  
  # Production requires more advanced networking setup
  create_network_watcher = true
  enable_ddos_protection = true
  
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
  
  # Production tier configuration
  sku_name           = "GP_Gen5_8"
  storage_mb         = 102400
  
  # High availability settings
  geo_redundant_backup_enabled = true
  auto_grow_enabled           = true
  public_network_access_enabled = false
  
  # Replicas for read scalability
  create_replica     = true
  replica_location   = var.replica_location
  
  tags = local.common_tags
}

# Storage Module
module "storage" {
  source = "../../modules/storage"
  
  environment        = local.environment
  resource_group_name = "${var.project_name}-${local.environment}-rg"
  location           = var.location
  account_name       = "${var.project_name}${local.environment}storage"
  
  # Production tier configuration
  account_tier       = "Standard"
  account_replication_type = "GRS"  # Geo-redundant storage
  
  # Security settings
  enable_https_traffic_only = true
  min_tls_version          = "TLS1_2"
  
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
  
  # Production tier configuration
  app_service_plan_tier = "PremiumV2"
  app_service_plan_size = "P2v2"
  
  # High availability settings
  auto_scale_default   = 2
  auto_scale_minimum   = 2
  auto_scale_maximum   = 10
  
  # Deployment slots for blue-green deployments
  deployment_slots     = ["staging", "qa"]
  
  tags = local.common_tags
}

# Application Insights
module "monitoring" {
  source = "../../modules/monitoring"
  
  environment        = local.environment
  resource_group_name = "${var.project_name}-${local.environment}-rg"
  location           = var.location
  
  app_insights_name  = "${var.project_name}-${local.environment}-insights"
  log_analytics_workspace_name = "${var.project_name}-${local.environment}-workspace"
  
  app_service_id     = module.compute.app_service_id
  
  tags = local.common_tags
}

# Security Module
module "security" {
  source = "../../modules/security"
  
  environment        = local.environment
  resource_group_name = "${var.project_name}-${local.environment}-rg"
  location           = var.location
  
  key_vault_name     = "${var.project_name}-${local.environment}-vault"
  
  # Set up private endpoints
  subnet_id          = module.networking.subnet_ids["private-endpoints-subnet"]
  private_endpoints  = {
    db = {
      name = "${var.project_name}-${local.environment}-db-pe"
      resource_id = module.database.server_id
      subresource_names = ["sqlServer"]
    }
    storage = {
      name = "${var.project_name}-${local.environment}-storage-pe"
      resource_id = module.storage.account_id
      subresource_names = ["blob"]
    }
  }
  
  tags = local.common_tags
}
