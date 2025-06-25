# ==============================================================================
# EDGE FUNCTIONS MODULE
# ==============================================================================

terraform {
  required_providers {
    supabase = {
      source  = "supabase/supabase"
      version = "~> 1.0"
    }
  }
}

# Development edge functions
resource "supabase_function" "dev_functions" {
  for_each = var.dev_project_ref != "" ? var.edge_functions : {}
  
  project_ref = var.dev_project_ref
  name        = each.value.name
  source_code = each.value.source_code
  import_map  = each.value.import_map
  verify_jwt  = each.value.verify_jwt
}

# Staging edge functions
resource "supabase_function" "staging_functions" {
  for_each = var.staging_project_ref != "" ? var.edge_functions : {}
  
  project_ref = var.staging_project_ref
  name        = each.value.name
  source_code = each.value.source_code
  import_map  = each.value.import_map
  verify_jwt  = each.value.verify_jwt
}

# Production edge functions
resource "supabase_function" "prod_functions" {
  for_each = var.prod_project_ref != "" ? var.edge_functions : {}
  
  project_ref = var.prod_project_ref
  name        = each.value.name
  source_code = each.value.source_code
  import_map  = each.value.import_map
  verify_jwt  = each.value.verify_jwt
}
