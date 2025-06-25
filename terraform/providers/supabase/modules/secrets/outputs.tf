output "secrets_status" {
  description = "Secrets management status"
  value = {
    dev_secrets_count     = var.dev_project_ref != "" ? length(var.project_secrets) : 0
    staging_secrets_count = var.staging_project_ref != "" ? length(var.project_secrets) : 0
    prod_secrets_count    = var.prod_project_ref != "" ? length(var.project_secrets) : 0
    
    secret_names = keys(var.project_secrets)
  }
}
