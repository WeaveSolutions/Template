# ==============================================================================
# FIREBASE REALTIME DATABASE MODULE - OUTPUTS
# ==============================================================================

output "database_url" {
  description = "Firebase Realtime Database URL"
  value       = google_firebase_database_instance.nexpo_rtdb.database_url
}

output "database_id" {
  description = "Realtime Database instance ID"
  value       = google_firebase_database_instance.nexpo_rtdb.database_id
}

output "region" {
  description = "Realtime Database region"
  value       = google_firebase_database_instance.nexpo_rtdb.region
}

output "database_type" {
  description = "Realtime Database type"
  value       = google_firebase_database_instance.nexpo_rtdb.type
}

output "state" {
  description = "Current state of the database"
  value       = google_firebase_database_instance.nexpo_rtdb.state
}

output "name" {
  description = "Full resource name of the database"
  value       = google_firebase_database_instance.nexpo_rtdb.name
}

output "connection_info" {
  description = "Connection information for client applications"
  value = {
    project_id   = var.project_id
    database_url = google_firebase_database_instance.nexpo_rtdb.database_url
    region       = google_firebase_database_instance.nexpo_rtdb.region
    
    # Client SDK configuration
    web_config = {
      databaseURL = google_firebase_database_instance.nexpo_rtdb.database_url
      projectId   = var.project_id
    }
    
    android_config = {
      databaseURL = google_firebase_database_instance.nexpo_rtdb.database_url
      projectId   = var.project_id
    }
    
    ios_config = {
      databaseURL = google_firebase_database_instance.nexpo_rtdb.database_url
      projectId   = var.project_id
    }
  }
}

output "security_rules_info" {
  description = "Information about Realtime Database security rules"
  value = {
    default_rules_applied = true
    rules_testing_url     = "https://console.firebase.google.com/project/${var.project_id}/database/${google_firebase_database_instance.nexpo_rtdb.database_id}/rules"
    rules_simulator_url   = "https://console.firebase.google.com/project/${var.project_id}/database/${google_firebase_database_instance.nexpo_rtdb.database_id}/rules/simulator"
  }
}
