output "service_name" {
  description = "Name of the Cloud Run service"
  value       = google_cloud_run_service.main.name
}

output "service_url" {
  description = "URL of the Cloud Run service"
  value       = google_cloud_run_service.main.status[0].url
}

output "service_account_email" {
  description = "Email of the service account"
  value       = google_service_account.cloud_run.email
}

output "artifact_registry_url" {
  description = "URL of the Artifact Registry"
  value       = "${var.region}-docker.pkg.dev/${var.project_id}/${google_artifact_registry_repository.main.repository_id}"
}

output "latest_revision_name" {
  description = "Name of the latest revision"
  value       = google_cloud_run_service.main.status[0].latest_created_revision_name
}
