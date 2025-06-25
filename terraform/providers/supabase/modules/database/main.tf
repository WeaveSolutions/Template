# ==============================================================================
# DATABASE CONFIGURATION MODULE
# ==============================================================================

terraform {
  required_providers {
    supabase = {
      source  = "supabase/supabase"
      version = "~> 1.0"
    }
  }
}

# Development database settings
resource "supabase_settings" "dev_db" {
  count       = var.enable_db_config && var.dev_project_ref != "" ? 1 : 0
  project_ref = var.dev_project_ref
  
  api = jsonencode({
    db_schema            = var.db_settings.db_schema
    db_extra_search_path = var.db_settings.db_extra_search_path
    max_rows             = var.db_settings.max_rows
  })
}

# Staging database settings
resource "supabase_settings" "staging_db" {
  count       = var.enable_db_config && var.staging_project_ref != "" ? 1 : 0
  project_ref = var.staging_project_ref
  
  api = jsonencode({
    db_schema            = var.db_settings.db_schema
    db_extra_search_path = var.db_settings.db_extra_search_path
    max_rows             = var.db_settings.max_rows
  })
}

# Production database settings
resource "supabase_settings" "prod_db" {
  count       = var.enable_db_config && var.prod_project_ref != "" ? 1 : 0
  project_ref = var.prod_project_ref
  
  api = jsonencode({
    db_schema            = var.db_settings.db_schema
    db_extra_search_path = var.db_settings.db_extra_search_path
    max_rows             = var.db_settings.max_rows
  })
}
