# ==============================================================================
# FIREBASE FIRESTORE MODULE - OUTPUTS
# ==============================================================================

output "database_name" {
  description = "Firestore database name"
  value       = google_firestore_database.nexpo_firestore.name
}

output "database_id" {
  description = "Firestore database ID"
  value       = google_firestore_database.nexpo_firestore.id
}

output "location_id" {
  description = "Firestore database location"
  value       = google_firestore_database.nexpo_firestore.location_id
}

output "database_type" {
  description = "Firestore database type"
  value       = google_firestore_database.nexpo_firestore.type
}

output "uid" {
  description = "Firestore database UID"
  value       = google_firestore_database.nexpo_firestore.uid
}

output "concurrency_mode" {
  description = "Firestore concurrency mode"
  value       = google_firestore_database.nexpo_firestore.concurrency_mode
}

output "app_engine_integration_mode" {
  description = "App Engine integration mode"
  value       = google_firestore_database.nexpo_firestore.app_engine_integration_mode
}

output "indexes_info" {
  description = "Information about created Firestore indexes"
  value = {
    count = length(google_firestore_index.indexes)
    indexes = [
      for idx_key, idx in google_firestore_index.indexes : {
        name       = idx.name
        collection = idx.collection
        fields     = idx.fields
        query_scope = idx.query_scope
      }
    ]
  }
}

output "sample_documents" {
  description = "Information about sample documents (if created)"
  value = var.create_sample_data ? {
    user_document = {
      collection  = "users"
      document_id = google_firestore_document.sample_user[0].document_id
      path        = google_firestore_document.sample_user[0].path
    }
    post_document = {
      collection  = "posts"
      document_id = google_firestore_document.sample_post[0].document_id
      path        = google_firestore_document.sample_post[0].path
    }
  } : null
}

output "backup_schedule" {
  description = "Firestore backup schedule information"
  value = var.enable_backup ? {
    name               = google_firestore_backup_schedule.daily_backup[0].name
    retention_seconds  = google_firestore_backup_schedule.daily_backup[0].retention
    retention_days     = var.backup_retention_days
  } : null
}

output "database_url" {
  description = "Firestore database URL"
  value       = "https://firestore.googleapis.com/v1/projects/${var.project_id}/databases/${google_firestore_database.nexpo_firestore.name}"
}

output "connection_info" {
  description = "Firestore connection information for client applications"
  value = {
    project_id    = var.project_id
    database_name = google_firestore_database.nexpo_firestore.name
    location_id   = google_firestore_database.nexpo_firestore.location_id
    endpoint      = "firestore.googleapis.com"
    
    # Client SDK configuration
    web_config = {
      projectId = var.project_id
      databaseURL = "https://${var.project_id}-default-rtdb.${google_firestore_database.nexpo_firestore.location_id}.firebasedatabase.app/"
    }
    
    android_config = {
      projectId = var.project_id
    }
    
    ios_config = {
      projectId = var.project_id
    }
  }
}

output "security_rules_info" {
  description = "Information about Firestore security rules"
  value = {
    default_rules_applied = true
    custom_rules_path     = "firestore.rules"
    rules_testing_url     = "https://console.firebase.google.com/project/${var.project_id}/firestore/rules"
  }
}
