output "function_info" {
  description = "Edge function information"
  value = {
    dev_functions     = var.dev_project_ref != "" ? keys(var.edge_functions) : []
    staging_functions = var.staging_project_ref != "" ? keys(var.edge_functions) : []
    prod_functions    = var.prod_project_ref != "" ? keys(var.edge_functions) : []
    
    function_configs = var.edge_functions
  }
}
