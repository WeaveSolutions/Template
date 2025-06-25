# GCP Development Environment

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
  public_subnet_cidr = "10.0.1.0/24"
  private_subnet_cidr = "10.0.2.0/24"
}

# Storage
module "storage" {
  source = "../../modules/storage"

  project_name          = var.project_name
  environment          = var.environment
  project_id           = var.project_id
  region               = var.region
  cors_origins         = ["*"]
  backup_retention_days = 7
}

# Database
module "database" {
  source = "../../modules/database"

  project_name        = var.project_name
  environment        = var.environment
  project_id         = var.project_id
  region             = var.region
  vpc_id             = module.networking.vpc_id
  db_tier            = "db-f1-micro"
  db_disk_size       = 10
  high_availability  = false
  enable_read_replica = false
}

# Compute
module "compute" {
  source = "../../modules/compute"

  project_name          = var.project_name
  environment          = var.environment
  project_id           = var.project_id
  region               = var.region
  cpu_limit            = "1"
  memory_limit         = "512Mi"
  min_instances        = 0
  max_instances        = 3
  vpc_connector_name   = module.networking.vpc_connector_name
  db_connection_name   = module.database.db_instance_connection_name
  db_name              = module.database.db_name
  db_username          = module.database.db_username
  environment_variables = {
    NODE_ENV                     = "development"
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
  assets_bucket_name    = module.storage.assets_bucket_name
  cloud_run_service_name = module.compute.service_name
}

# Monitoring
module "monitoring" {
  source = "../../modules/monitoring"

  project_name          = var.project_name
  environment          = var.environment
  project_id           = var.project_id
  app_domain           = module.cdn.cdn_url
  alert_email_endpoints = var.alert_email_endpoints
  billing_account_id   = var.billing_account_id
  monthly_budget_amount = "200"
}

# Security
module "security" {
  source = "../../modules/security"

  project_name    = var.project_name
  environment    = var.environment
  project_id     = var.project_id
  project_number = var.project_number
  region         = var.region
  app_url        = module.cdn.cdn_url
  app_secrets    = {
    supabase_service_key = var.supabase_service_key
  }
}

# Firebase services with Auth0 integration
module "firebase" {
  source = "../../modules/firebase"

  project_name         = var.project_name
  environment          = var.environment
  project_id           = var.project_id
  region               = var.region
  firestore_location   = var.firestore_location
  
  # Auth0 configuration
  auth0_domain         = var.auth0_domain
  auth0_api_identifier = var.auth0_api_identifier
  
  # Enable features
  enable_analytics       = var.enable_firebase_analytics
  enable_crashlytics     = var.enable_firebase_crashlytics
  enable_performance     = var.enable_firebase_performance
  enable_cloud_messaging = var.enable_firebase_messaging
  enable_remote_config   = var.enable_firebase_remote_config
  
  labels = local.labels

  depends_on = [module.networking]
}
