output "assets_bucket_name" {
  description = "Name of the assets bucket"
  value       = google_storage_bucket.assets.name
}

output "assets_bucket_url" {
  description = "URL of the assets bucket"
  value       = google_storage_bucket.assets.url
}

output "uploads_bucket_name" {
  description = "Name of the uploads bucket"
  value       = google_storage_bucket.uploads.name
}

output "uploads_bucket_url" {
  description = "URL of the uploads bucket"
  value       = google_storage_bucket.uploads.url
}

output "backups_bucket_name" {
  description = "Name of the backups bucket"
  value       = google_storage_bucket.backups.name
}

output "storage_service_account_email" {
  description = "Email of the storage service account"
  value       = google_service_account.storage.email
}

output "storage_service_account_key" {
  description = "Key for the storage service account"
  value       = google_service_account_key.storage_key.private_key
  sensitive   = true
}

# Service account key
resource "google_service_account_key" "storage_key" {
  service_account_id = google_service_account.storage.name
}
