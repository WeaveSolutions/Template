output "bucket_info" {
  description = "Storage bucket information"
  value = {
    dev_buckets     = var.dev_project_ref != "" ? keys(var.storage_buckets) : []
    staging_buckets = var.staging_project_ref != "" ? keys(var.storage_buckets) : []
    prod_buckets    = var.prod_project_ref != "" ? keys(var.storage_buckets) : []
    
    bucket_configs = var.storage_buckets
  }
}
