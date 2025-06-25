output "api_status" {
  description = "API configuration status"
  value = {
    dev_configured     = var.dev_project_ref != "" ? true : false
    staging_configured = var.staging_project_ref != "" ? true : false
    prod_configured    = var.prod_project_ref != "" ? true : false
    
    api_config = {
      db_schema = var.api_settings.db_schema
      search_path = var.api_settings.db_extra_search_path
      max_rows = var.api_settings.max_rows
      jwt_expiry = var.api_settings.jwt_expiry
    }
  }
}
