# ==============================================================================
# PROJECT OUTPUTS
# ==============================================================================

output "development_project" {
  description = "Development project details"
  value = var.create_development_project ? {
    id                = supabase_project.development[0].id
    name              = supabase_project.development[0].name
    api_url           = supabase_project.development[0].api_url
    db_url            = supabase_project.development[0].db_url
    anon_key          = supabase_project.development[0].anon_key
    service_role_key  = supabase_project.development[0].service_role_key
    region            = supabase_project.development[0].region
  } : null
  sensitive = true
}

output "staging_project" {
  description = "Staging project details"
  value = var.create_staging_project ? {
    id                = supabase_project.staging[0].id
    name              = supabase_project.staging[0].name
    api_url           = supabase_project.staging[0].api_url
    db_url            = supabase_project.staging[0].db_url
    anon_key          = supabase_project.staging[0].anon_key
    service_role_key  = supabase_project.staging[0].service_role_key
    region            = supabase_project.staging[0].region
  } : null
  sensitive = true
}

output "production_project" {
  description = "Production project details"
  value = var.create_production_project ? {
    id                = supabase_project.production[0].id
    name              = supabase_project.production[0].name
    api_url           = supabase_project.production[0].api_url
    db_url            = supabase_project.production[0].db_url
    anon_key          = supabase_project.production[0].anon_key
    service_role_key  = supabase_project.production[0].service_role_key
    region            = supabase_project.production[0].region
  } : null
  sensitive = true
}

# ==============================================================================
# CONVENIENCE OUTPUTS FOR OTHER PROVIDERS
# ==============================================================================

# For use in other Terraform providers (AWS, GCP, etc.)
output "project_urls" {
  description = "Supabase project URLs for environment configuration"
  value = {
    development = var.create_development_project ? supabase_project.development[0].api_url : var.existing_dev_project_ref
    staging     = var.create_staging_project ? supabase_project.staging[0].api_url : var.existing_staging_project_ref
    production  = var.create_production_project ? supabase_project.production[0].api_url : var.existing_prod_project_ref
  }
}

output "anon_keys" {
  description = "Supabase anonymous keys for client-side access"
  value = {
    development = var.create_development_project ? supabase_project.development[0].anon_key : ""
    staging     = var.create_staging_project ? supabase_project.staging[0].anon_key : ""
    production  = var.create_production_project ? supabase_project.production[0].anon_key : ""
  }
  sensitive = true
}

output "service_role_keys" {
  description = "Supabase service role keys for server-side access"
  value = {
    development = var.create_development_project ? supabase_project.development[0].service_role_key : ""
    staging     = var.create_staging_project ? supabase_project.staging[0].service_role_key : ""
    production  = var.create_production_project ? supabase_project.production[0].service_role_key : ""
  }
  sensitive = true
}

# ==============================================================================
# MODULE OUTPUTS - DATA SERVICES ONLY
# ==============================================================================
# Note: Authentication handled by Auth0, not Supabase Auth
# These outputs are for Supabase data services integration

output "database_config" {
  description = "Database configuration status"
  value       = module.database.db_status
}

output "storage_buckets" {
  description = "Created storage buckets"
  value       = module.storage.bucket_info
}

output "edge_functions" {
  description = "Deployed edge functions"
  value       = module.edge_functions.function_info
}
