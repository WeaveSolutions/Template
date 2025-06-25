# ==============================================================================
# FIREBASE TERRAFORM PROVIDER - OUTPUTS
# ==============================================================================

# ==============================================================================
# PROJECT OUTPUTS
# ==============================================================================

output "project_id" {
  description = "Google Cloud Project ID"
  value       = var.create_new_project ? google_project.firebase_project[0].project_id : var.gcp_project_id
}

output "project_number" {
  description = "Google Cloud Project Number"
  value       = var.create_new_project ? google_project.firebase_project[0].number : null
}

output "firebase_project_id" {
  description = "Firebase Project ID"
  value       = google_firebase_project.nexpo_firebase.project
}

# ==============================================================================
# FIREBASE APPLICATION OUTPUTS
# ==============================================================================

output "web_app" {
  description = "Firebase Web App information"
  value = var.enable_web_app ? {
    app_id      = google_firebase_web_app.nexpo_web[0].app_id
    name        = google_firebase_web_app.nexpo_web[0].display_name
    api_key_id  = google_firebase_web_app.nexpo_web[0].api_key_id
  } : null
}

output "android_app" {
  description = "Firebase Android App information"
  value = var.enable_android_app ? {
    app_id       = google_firebase_android_app.nexpo_android[0].app_id
    name         = google_firebase_android_app.nexpo_android[0].display_name
    package_name = google_firebase_android_app.nexpo_android[0].package_name
  } : null
}

output "ios_app" {
  description = "Firebase iOS App information"
  value = var.enable_ios_app ? {
    app_id      = google_firebase_apple_app.nexpo_ios[0].app_id
    name        = google_firebase_apple_app.nexpo_ios[0].display_name
    bundle_id   = google_firebase_apple_app.nexpo_ios[0].bundle_id
  } : null
}

# ==============================================================================
# AUTHENTICATION INTEGRATION
# ==============================================================================
# Authentication handled by Auth0 - no Firebase Auth outputs needed
# Custom tokens provided by CRA service for Firebase data access

# ==============================================================================
# FIRESTORE OUTPUTS
# ==============================================================================

output "firestore_database" {
  description = "Cloud Firestore database information"
  value = {
    name         = module.firestore.database_name
    location_id  = module.firestore.location_id
    database_type = module.firestore.database_type
    uid          = module.firestore.uid
  }
}

output "firestore_indexes" {
  description = "Firestore indexes information"
  value       = module.firestore.indexes_info
}

# ==============================================================================
# STORAGE OUTPUTS
# ==============================================================================

output "storage_buckets" {
  description = "Firebase Storage buckets information"
  value       = module.storage.buckets_info
}

# ==============================================================================
# CLOUD FUNCTIONS OUTPUTS
# ==============================================================================

output "cloud_functions" {
  description = "Cloud Functions information"
  value       = module.functions.functions_info
}

# ==============================================================================
# REALTIME DATABASE OUTPUTS
# ==============================================================================

output "realtime_database" {
  description = "Firebase Realtime Database information"
  value = var.enable_realtime_database ? {
    database_url = module.realtime_database[0].database_url
    region       = module.realtime_database[0].region
  } : null
}

# ==============================================================================
# SECURITY RULES OUTPUTS
# ==============================================================================

output "security_rules" {
  description = "Firebase Security Rules status (Auth0 integrated)"
  value       = module.security_rules.rules_status
}

# ==============================================================================
# APP HOSTING OUTPUTS
# ==============================================================================

output "app_hosting" {
  description = "Firebase App Hosting information"
  value = var.enable_app_hosting ? {
    backend_info = module.app_hosting[0].backend_info
  } : null
}

# ==============================================================================
# CONFIGURATION OUTPUTS FOR CLIENT APPS
# ==============================================================================

output "firebase_config" {
  description = "Firebase configuration for client applications (Auth0 integrated)"
  value = {
    web_config = var.enable_web_app ? {
      apiKey     = "your-web-api-key"
      authDomain = "${var.gcp_project_id}.firebaseapp.com"
      projectId  = var.gcp_project_id
      storageBucket = "${var.gcp_project_id}.appspot.com"
      messagingSenderId = "your-sender-id"
      appId      = google_firebase_web_app.nexpo_web[0].app_id
      # Note: Authentication handled by Auth0, not Firebase Auth
    } : null
    
    android_config = var.enable_android_app ? {
      applicationId = var.android_package_name
      apiKey       = "your-android-api-key"
      projectId    = var.gcp_project_id
      # Note: Authentication handled by Auth0, not Firebase Auth
    } : null
    
    ios_config = var.enable_ios_app ? {
      bundleId  = var.ios_bundle_id
      apiKey    = "your-ios-api-key"
      projectId = var.gcp_project_id
      # Note: Authentication handled by Auth0, not Firebase Auth
    } : null
  }
  sensitive = true
}

# ==============================================================================
# URLS AND ENDPOINTS
# ==============================================================================

output "service_urls" {
  description = "Firebase service URLs"
  value = {
    console_url    = "https://console.firebase.google.com/project/${var.gcp_project_id}"
    auth_domain    = "${var.gcp_project_id}.firebaseapp.com"  # Used for Auth0 integration
    firestore_url  = "https://firestore.googleapis.com/v1/projects/${var.gcp_project_id}/databases/(default)"
    storage_url    = "https://firebasestorage.googleapis.com/v0/b/${var.gcp_project_id}.appspot.com"
    functions_url  = "https://${var.gcp_region}-${var.gcp_project_id}.cloudfunctions.net"
  }
}
