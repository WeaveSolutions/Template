# ==============================================================================
# STORAGE CONFIGURATION MODULE
# ==============================================================================

terraform {
  required_providers {
    supabase = {
      source  = "supabase/supabase"
      version = "~> 1.0"
    }
  }
}

# Development storage buckets
resource "supabase_bucket" "dev_buckets" {
  for_each = var.dev_project_ref != "" ? var.storage_buckets : {}
  
  project_ref         = var.dev_project_ref
  name               = each.value.name
  public             = each.value.public
  file_size_limit    = each.value.file_size_limit
  allowed_mime_types = each.value.allowed_mime_types
}

# Staging storage buckets
resource "supabase_bucket" "staging_buckets" {
  for_each = var.staging_project_ref != "" ? var.storage_buckets : {}
  
  project_ref         = var.staging_project_ref
  name               = each.value.name
  public             = each.value.public
  file_size_limit    = each.value.file_size_limit
  allowed_mime_types = each.value.allowed_mime_types
}

# Production storage buckets
resource "supabase_bucket" "prod_buckets" {
  for_each = var.prod_project_ref != "" ? var.storage_buckets : {}
  
  project_ref         = var.prod_project_ref
  name               = each.value.name
  public             = each.value.public
  file_size_limit    = each.value.file_size_limit
  allowed_mime_types = each.value.allowed_mime_types
}
