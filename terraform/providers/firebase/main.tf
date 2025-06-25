# ==============================================================================
# FIREBASE TERRAFORM PROVIDER - MAIN CONFIGURATION
# ==============================================================================
# This configuration manages Firebase data services only.
# Authentication is handled by Auth0 with custom token exchange via CRA service.
# ==============================================================================

terraform {
  required_version = ">= 1.0"
  
  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "~> 6.0"
    }
    google-beta = {
      source  = "hashicorp/google-beta"  
      version = "~> 6.0"
    }
  }
}

# ==============================================================================
# PROVIDER CONFIGURATION
# ==============================================================================

provider "google" {
  project = var.gcp_project_id
  region  = var.gcp_region
}

provider "google-beta" {
  project = var.gcp_project_id
  region  = var.gcp_region
}

# ==============================================================================
# GOOGLE CLOUD PROJECT
# ==============================================================================

# Create or manage Google Cloud Project
resource "google_project" "nexpo_project" {
  count           = var.create_new_project ? 1 : 0
  name            = var.project_name
  project_id      = var.gcp_project_id
  billing_account = var.billing_account_id
  
  labels = {
    environment = "multi"
    managed_by  = "terraform"
    project     = "nexpo"
  }
}

# Enable required APIs
resource "google_project_service" "required_apis" {
  for_each = toset([
    "firebase.googleapis.com",
    "firestore.googleapis.com",
    "storage-component.googleapis.com",
    "cloudfunctions.googleapis.com",
    "appengine.googleapis.com",
    "identitytoolkit.googleapis.com",
    "firebaserules.googleapis.com",
    "cloudresourcemanager.googleapis.com",
    "serviceusage.googleapis.com",
    "iam.googleapis.com"
  ])
  
  project                    = var.create_new_project ? google_project.nexpo_project[0].project_id : var.gcp_project_id
  service                    = each.value
  disable_dependent_services = false
}

# ==============================================================================
# ENABLE FIREBASE SERVICES
# ==============================================================================

resource "google_firebase_project" "nexpo_firebase" {
  provider   = google-beta
  project    = var.create_new_project ? google_project.nexpo_project[0].project_id : var.gcp_project_id
  depends_on = [google_project_service.required_apis]
}

# ==============================================================================
# FIREBASE APPLICATIONS
# ==============================================================================

# Web Application
resource "google_firebase_web_app" "nexpo_web" {
  count        = var.enable_web_app ? 1 : 0
  provider     = google-beta
  project      = google_firebase_project.nexpo_firebase.project
  display_name = "${var.project_name} Web App"
  
  depends_on = [google_firebase_project.nexpo_firebase]
}

# Android Application
resource "google_firebase_android_app" "nexpo_android" {
  count         = var.enable_android_app ? 1 : 0
  provider      = google-beta
  project       = google_firebase_project.nexpo_firebase.project
  display_name  = "${var.project_name} Android App"
  package_name  = var.android_package_name
  
  depends_on = [google_firebase_project.nexpo_firebase]
}

# iOS Application
resource "google_firebase_apple_app" "nexpo_ios" {
  count         = var.enable_ios_app ? 1 : 0
  provider      = google-beta
  project       = google_firebase_project.nexpo_firebase.project
  display_name  = "${var.project_name} iOS App"
  bundle_id     = var.ios_bundle_id
  app_store_id  = var.ios_app_store_id
  
  depends_on = [google_firebase_project.nexpo_firebase]
}

# ==============================================================================
# FIREBASE MODULES - DATA SERVICES ONLY
# ==============================================================================
# Note: Authentication handled by Auth0, not Firebase Auth

# Firestore Database Module
module "firestore" {
  source = "./modules/firestore"
  
  project_id                      = google_firebase_project.nexpo_firebase.project
  location_id                     = var.firestore_config.location_id
  database_type                   = var.firestore_config.database_type
  concurrency_mode                = var.firestore_config.concurrency_mode
  app_engine_integration_mode     = var.firestore_config.app_engine_integration_mode
  indexes                         = var.firestore_config.indexes
  create_sample_data              = var.firestore_config.create_sample_data
  backup_retention_days           = var.firestore_config.backup_retention_days
  
  depends_on = [google_firebase_project.nexpo_firebase]
}

# Cloud Storage Module
module "storage" {
  source = "./modules/storage"
  
  project_id              = google_firebase_project.nexpo_firebase.project
  storage_buckets         = var.storage_config.buckets
  create_sample_data      = var.storage_config.create_sample_data
  sample_files            = var.storage_config.sample_files
  
  depends_on = [google_firebase_project.nexpo_firebase]
}

# Cloud Functions Module
module "functions" {
  source = "./modules/functions"
  
  project_id         = google_firebase_project.nexpo_firebase.project
  region            = var.gcp_region
  cloud_functions   = var.functions_config.functions
  create_source_bucket = var.functions_config.create_source_bucket
  source_bucket_name = var.functions_config.source_bucket_name
  
  depends_on = [google_firebase_project.nexpo_firebase]
}

# Realtime Database Module (optional)
module "realtime_database" {
  count  = var.enable_realtime_database ? 1 : 0
  source = "./modules/realtime_database"
  
  project_id   = google_firebase_project.nexpo_firebase.project
  database_id  = var.rtdb_config.database_id
  region       = var.rtdb_config.region
  database_url = var.rtdb_config.database_url
  
  depends_on = [google_firebase_project.nexpo_firebase]
}

# Security Rules Module (Auth0 integration)
module "security_rules" {
  source = "./modules/security_rules"
  
  project_id             = google_firebase_project.nexpo_firebase.project
  firestore_rules_file   = var.security_rules_config.firestore_rules_file
  storage_rules_file     = var.security_rules_config.storage_rules_file
  create_default_rules   = var.security_rules_config.create_default_rules
  
  depends_on = [google_firebase_project.nexpo_firebase]
}

# App Hosting Module (optional)
module "app_hosting" {
  count  = var.enable_app_hosting ? 1 : 0
  source = "./modules/app_hosting"
  
  project_id            = google_firebase_project.nexpo_firebase.project
  backend_id            = var.app_hosting_config.backend_id
  location              = var.gcp_region
  github_repository_url = var.app_hosting_config.github_repository_url
  github_branch         = var.app_hosting_config.github_branch
  root_directory        = var.app_hosting_config.root_directory
  create_initial_build  = var.app_hosting_config.create_initial_build
  
  depends_on = [google_firebase_project.nexpo_firebase]
}
