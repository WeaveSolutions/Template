# GCP Production Environment

terraform {
  required_version = ">= 1.5.0"
  
  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "~> 5.0"
    }
    random = {
      source  = "hashicorp/random"
      version = "~> 3.5"
    }
  }
  
  backend "gcs" {
    bucket = "Nexpo-terraform-state"
    prefix = "gcp/prod"
  }
}

provider "google" {
  project = var.project_id
  region  = var.region
}

# Networking
module "networking" {
  source = "../../modules/networking"

  project_name        = var.project_name
  environment        = var.environment
  project_id         = var.project_id
  region             = var.region
  public_subnet_cidr = "10.1.1.0/24"
  private_subnet_cidr = "10.1.2.0/24"
}

# Storage
module "storage" {
  source = "../../modules/storage"

  project_name          = var.project_name
  environment          = var.environment
  project_id           = var.project_id
  region               = var.region
  storage_class        = "STANDARD"
  cors_origins         = ["https://${var.custom_domain}"]
  backup_retention_days = 30
}

# Database
module "database" {
  source = "../../modules/database"

  project_name        = var.project_name
  environment        = var.environment
  project_id         = var.project_id
  region             = var.region
  vpc_id             = module.networking.vpc_id
  db_tier            = "db-n1-standard-2"
  db_disk_size       = 100
  high_availability  = true
  enable_read_replica = true
  backup_configuration = {
    enabled                        = true
    start_time                     = "02:00"
    location                       = var.region
    point_in_time_recovery_enabled = true
    retention_days                 = 30
  }
}

# Compute
module "compute" {
  source = "../../modules/compute"

  project_name          = var.project_name
  environment          = var.environment
  project_id           = var.project_id
  region               = var.region
  cpu_limit            = "2"
  memory_limit         = "2Gi"
  min_instances        = 2
  max_instances        = 100
  vpc_connector_name   = module.networking.vpc_connector_name
  db_connection_name   = module.database.db_instance_connection_name
  db_name              = module.database.db_name
  db_username          = module.database.db_username
  environment_variables = {
    NODE_ENV                     = "production"
    NEXT_PUBLIC_SUPABASE_URL     = var.supabase_url
    NEXT_PUBLIC_SUPABASE_ANON_KEY = var.supabase_anon_key
  }
}

# CDN
module "cdn" {
  source = "../../modules/cdn"

  project_name           = var.project_name
  environment           = var.environment
  project_id            = var.project_id
  region                = var.region
  custom_domain         = var.custom_domain
  assets_bucket_name    = module.storage.assets_bucket_name
  cloud_run_service_name = module.compute.service_name
  enable_cloud_armor    = true
}

# Monitoring
module "monitoring" {
  source = "../../modules/monitoring"

  project_name          = var.project_name
  environment          = var.environment
  project_id           = var.project_id
  app_domain           = var.custom_domain
  alert_email_endpoints = var.alert_email_endpoints
  alert_sms_endpoints  = var.alert_sms_endpoints
  billing_account_id   = var.billing_account_id
  monthly_budget_amount = "2000"
  db_connection_threshold = 90
}

# Security
module "security" {
  source = "../../modules/security"

  project_name               = var.project_name
  environment               = var.environment
  project_id                = var.project_id
  project_number            = var.project_number
  region                    = var.region
  organization_id           = var.organization_id
  app_url                   = "https://${var.custom_domain}"
  enable_vpc_service_controls = true
  access_policy_id          = var.access_policy_id
  enable_binary_authorization = true
  enable_web_security_scanner = true
  scan_auth_user            = var.scan_auth_user
  scan_auth_password        = var.scan_auth_password
  app_secrets               = {
    supabase_service_key = var.supabase_service_key
    api_key             = var.api_key
  }
}
