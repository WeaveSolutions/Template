terraform {
  required_providers {
    supabase = {
      source  = "supabase/supabase"
      version = "~> 1.0"
    }
  }
}

provider "supabase" {
  access_token = var.supabase_access_token
}

# Organization management
resource "supabase_organization" "main" {
  count = var.manage_organization ? 1 : 0
  name  = var.organization_name
}

# Projects for different environments
resource "supabase_project" "development" {
  count             = var.create_development_project ? 1 : 0
  organization_id   = local.organization_id
  name              = "${var.project_name}-dev"
  database_password = var.dev_database_password
  region            = var.supabase_region
  
  lifecycle {
    ignore_changes = [database_password]
  }
}

resource "supabase_project" "staging" {
  count             = var.create_staging_project ? 1 : 0
  organization_id   = local.organization_id
  name              = "${var.project_name}-staging"
  database_password = var.staging_database_password
  region            = var.supabase_region
  
  lifecycle {
    ignore_changes = [database_password]
  }
}

resource "supabase_project" "production" {
  count             = var.create_production_project ? 1 : 0
  organization_id   = local.organization_id
  name              = "${var.project_name}-prod"
  database_password = var.prod_database_password
  region            = var.supabase_region
  
  lifecycle {
    ignore_changes = [database_password]
  }
}

locals {
  organization_id = var.manage_organization ? supabase_organization.main[0].id : var.existing_organization_id
  
  # Project references
  dev_project_ref     = var.create_development_project ? supabase_project.development[0].id : var.existing_dev_project_ref
  staging_project_ref = var.create_staging_project ? supabase_project.staging[0].id : var.existing_staging_project_ref
  prod_project_ref    = var.create_production_project ? supabase_project.production[0].id : var.existing_prod_project_ref
}

# ==============================================================================
# SUPABASE MODULES - DATA SERVICES ONLY
# ==============================================================================
# Note: Authentication handled by Auth0, not Supabase Auth
# Supabase provides database, storage, and edge functions as data services

# Database module - PostgreSQL database configuration
module "database" {
  source = "./modules/database"
  
  dev_project_ref     = local.dev_project_ref
  staging_project_ref = local.staging_project_ref
  prod_project_ref    = local.prod_project_ref
  
  # Pass database configuration
  enable_db_config = var.enable_db_config
  db_settings      = var.db_settings
}

# API Settings module - REST API configuration
module "api_settings" {
  source = "./modules/api"
  
  dev_project_ref     = local.dev_project_ref
  staging_project_ref = local.staging_project_ref
  prod_project_ref    = local.prod_project_ref
  
  # Pass API configuration
  api_settings = var.api_settings
}

# Secrets module - Environment variables and secrets
module "secrets" {
  source = "./modules/secrets"
  
  dev_project_ref     = local.dev_project_ref
  staging_project_ref = local.staging_project_ref
  prod_project_ref    = local.prod_project_ref
  
  # Pass secrets configuration
  project_secrets = var.project_secrets
}

# Storage module - File storage buckets and policies
module "storage" {
  source = "./modules/storage"
  
  dev_project_ref     = local.dev_project_ref
  staging_project_ref = local.staging_project_ref
  prod_project_ref    = local.prod_project_ref
  
  # Pass storage configuration
  storage_buckets = var.storage_buckets
}

# Edge Functions module - Serverless functions
module "edge_functions" {
  source = "./modules/edge_functions"
  
  dev_project_ref     = local.dev_project_ref
  staging_project_ref = local.staging_project_ref
  prod_project_ref    = local.prod_project_ref
  
  # Pass edge functions configuration
  edge_functions = var.edge_functions
}
