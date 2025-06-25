output "db_status" {
  description = "Database configuration status"
  value = {
    dev_configured     = var.enable_db_config && var.dev_project_ref != "" ? true : false
    staging_configured = var.enable_db_config && var.staging_project_ref != "" ? true : false
    prod_configured    = var.enable_db_config && var.prod_project_ref != "" ? true : false
    
    schema_config = {
      db_schema = var.db_settings.db_schema
      search_path = var.db_settings.db_extra_search_path
      max_rows = var.db_settings.max_rows
    }
  }
}
