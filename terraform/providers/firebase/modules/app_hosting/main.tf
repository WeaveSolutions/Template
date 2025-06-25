# ==============================================================================
# FIREBASE APP HOSTING MODULE
# ==============================================================================

# ==============================================================================
# APP HOSTING BACKEND
# ==============================================================================

resource "google_firebase_app_hosting_backend" "nexpo_backend" {
  project     = var.project_id
  backend_id  = var.backend_id
  location    = var.location
  
  source_code_config {
    repository_config {
      repository_url = var.github_repository_url
      branch         = var.github_branch
      root_directory = var.root_directory
    }
  }
  
  labels = {
    environment = "firebase"
    managed_by  = "terraform"
  }
}

# ==============================================================================
# APP HOSTING BUILD
# ==============================================================================

resource "google_firebase_app_hosting_build" "nexpo_build" {
  count = var.create_initial_build ? 1 : 0
  
  project    = var.project_id
  backend_id = google_firebase_app_hosting_backend.nexpo_backend.backend_id
  location   = var.location
  
  source {
    code_base {
      repository_config {
        repository_url = var.github_repository_url
        branch         = var.github_branch
        root_directory = var.root_directory
      }
    }
  }
  
  depends_on = [google_firebase_app_hosting_backend.nexpo_backend]
}

# ==============================================================================
# APP HOSTING TRAFFIC
# ==============================================================================

resource "google_firebase_app_hosting_traffic" "nexpo_traffic" {
  count = var.create_initial_build ? 1 : 0
  
  project    = var.project_id
  backend_id = google_firebase_app_hosting_backend.nexpo_backend.backend_id
  location   = var.location
  
  traffic_allocations = {
    "100" = google_firebase_app_hosting_build.nexpo_build[0].build_id
  }
  
  depends_on = [google_firebase_app_hosting_build.nexpo_build]
}
