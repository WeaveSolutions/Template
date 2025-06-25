# ==============================================================================
# FIREBASE REALTIME DATABASE MODULE
# ==============================================================================

# ==============================================================================
# REALTIME DATABASE INSTANCE
# ==============================================================================

resource "google_firebase_database_instance" "nexpo_rtdb" {
  provider    = google-beta
  project     = var.project_id
  database_id = var.database_id
  region      = var.region
  type        = var.database_type
  
  # Disable default rules initially for security
  desired_state = "ACTIVE"
}
