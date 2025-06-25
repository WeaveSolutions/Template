# ==============================================================================
# API CONFIGURATION MODULE
# ==============================================================================

terraform {
  required_providers {
    supabase = {
      source  = "supabase/supabase"
      version = "~> 1.0"
    }
  }
}

# Development API settings
resource "supabase_settings" "dev_api" {
  count       = var.dev_project_ref != "" ? 1 : 0
  project_ref = var.dev_project_ref
  
  api = jsonencode({
    db_schema            = var.api_settings.db_schema
    db_extra_search_path = var.api_settings.db_extra_search_path
    max_rows             = var.api_settings.max_rows
    jwt_expiry           = var.api_settings.jwt_expiry
  })
}

# Staging API settings
resource "supabase_settings" "staging_api" {
  count       = var.staging_project_ref != "" ? 1 : 0
  project_ref = var.staging_project_ref
  
  api = jsonencode({
    db_schema            = var.api_settings.db_schema
    db_extra_search_path = var.api_settings.db_extra_search_path
    max_rows             = var.api_settings.max_rows
    jwt_expiry           = var.api_settings.jwt_expiry
  })
}

# Production API settings
resource "supabase_settings" "prod_api" {
  count       = var.prod_project_ref != "" ? 1 : 0
  project_ref = var.prod_project_ref
  
  api = jsonencode({
    db_schema            = var.api_settings.db_schema
    db_extra_search_path = var.api_settings.db_extra_search_path
    max_rows             = var.api_settings.max_rows
    jwt_expiry           = var.api_settings.jwt_expiry
  })
}
