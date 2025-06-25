output "app_url" {
  description = "Application URL"
  value       = module.cdn.cdn_url
}

output "cloud_run_service_url" {
  description = "Cloud Run service URL"
  value       = module.compute.service_url
}

output "assets_bucket_url" {
  description = "Assets bucket URL"
  value       = module.storage.assets_bucket_url
}

output "load_balancer_ip" {
  description = "Load balancer IP address"
  value       = module.cdn.load_balancer_ip
}

# Monitoring outputs
output "monitoring_dashboard_url" {
  description = "URL to the monitoring dashboard"
  value       = module.monitoring.dashboard_url
}

# Firebase outputs
output "firebase_web_config" {
  description = "Firebase configuration for web app"
  value       = module.firebase.firebase_web_config
  sensitive   = true
}

output "firebase_android_config" {
  description = "Firebase configuration for Android app"
  value       = module.firebase.firebase_android_config
  sensitive   = true
}

output "firebase_ios_config" {
  description = "Firebase configuration for iOS app"
  value       = module.firebase.firebase_ios_config
  sensitive   = true
}

output "token_exchange_url" {
  description = "URL for Auth0 to Firebase token exchange function"
  value       = module.firebase.token_exchange_url
}

output "firestore_database" {
  description = "Firestore database name"
  value       = module.firebase.firestore_database
}

output "firebase_storage_bucket" {
  description = "Firebase Storage bucket name"
  value       = module.firebase.storage_bucket
}
