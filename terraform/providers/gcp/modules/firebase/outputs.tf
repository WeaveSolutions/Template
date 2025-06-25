# Get Firebase Web App configuration
data "google_firebase_web_app_config" "main" {
  provider   = google-beta
  project    = var.project_id
  web_app_id = google_firebase_web_app.main.app_id
}

# Get Firebase Android App configuration
data "google_firebase_android_app_config" "main" {
  provider = google-beta
  project  = var.project_id
  app_id   = google_firebase_android_app.main.app_id
}

# Firebase configuration outputs
output "firebase_web_config" {
  description = "Firebase configuration for web app"
  value = {
    apiKey            = data.google_firebase_web_app_config.main.api_key
    authDomain        = data.google_firebase_web_app_config.main.auth_domain
    projectId         = var.project_id
    storageBucket     = data.google_firebase_web_app_config.main.storage_bucket
    messagingSenderId = data.google_firebase_web_app_config.main.messaging_sender_id
    appId             = google_firebase_web_app.main.app_id
  }
  sensitive = true
}

output "firebase_android_config" {
  description = "Firebase configuration for Android app"
  value = {
    projectId     = var.project_id
    appId         = google_firebase_android_app.main.app_id
    apiKey        = data.google_firebase_android_app_config.main.config_file_contents
    storageBucket = "${var.project_id}.appspot.com"
  }
  sensitive = true
}

output "firebase_ios_config" {
  description = "Firebase configuration for iOS app"
  value = {
    projectId     = var.project_id
    appId         = google_firebase_apple_app.main.app_id
    storageBucket = "${var.project_id}.appspot.com"
  }
}

output "token_exchange_url" {
  description = "URL for Auth0 to Firebase token exchange function"
  value       = google_cloudfunctions2_function.token_exchange.service_config[0].uri
}

output "firestore_database" {
  description = "Firestore database name"
  value       = google_firestore_database.main.name
}

output "storage_bucket" {
  description = "Firebase Storage bucket name"
  value       = google_firebase_storage_bucket.main.bucket_id
}

output "firebase_admin_service_account" {
  description = "Firebase Admin SDK service account email"
  value       = google_service_account.firebase_admin.email
}

output "firebase_admin_key_secret" {
  description = "Secret Manager secret ID for Firebase Admin key"
  value       = google_secret_manager_secret.firebase_admin_key.secret_id
}
