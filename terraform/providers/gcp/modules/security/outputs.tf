output "kms_key_id" {
  description = "ID of the KMS encryption key"
  value       = google_kms_crypto_key.main.id
}

output "security_service_account_email" {
  description = "Email of the security service account"
  value       = google_service_account.security.email
}

output "secret_ids" {
  description = "IDs of created secrets"
  value       = { for k, v in google_secret_manager_secret.app_secrets : k => v.id }
}

output "scc_source_name" {
  description = "Name of Security Command Center source"
  value       = var.environment == "prod" && var.organization_id != "" ? google_scc_source.custom[0].name : null
}
