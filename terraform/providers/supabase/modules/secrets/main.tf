# ==============================================================================
# SECRETS MANAGEMENT MODULE
# ==============================================================================

terraform {
  required_providers {
    supabase = {
      source  = "supabase/supabase"
      version = "~> 1.0"
    }
  }
}

# Development secrets
resource "supabase_secret" "dev_secrets" {
  for_each = var.dev_project_ref != "" ? var.project_secrets : {}
  
  project_ref = var.dev_project_ref
  name        = each.value.name
  value       = each.value.value
}

# Staging secrets
resource "supabase_secret" "staging_secrets" {
  for_each = var.staging_project_ref != "" ? var.project_secrets : {}
  
  project_ref = var.staging_project_ref
  name        = each.value.name
  value       = each.value.value
}

# Production secrets
resource "supabase_secret" "prod_secrets" {
  for_each = var.prod_project_ref != "" ? var.project_secrets : {}
  
  project_ref = var.prod_project_ref
  name        = each.value.name
  value       = each.value.value
}
