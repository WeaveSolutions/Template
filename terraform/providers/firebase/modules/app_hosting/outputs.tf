# ==============================================================================
# FIREBASE APP HOSTING MODULE - OUTPUTS
# ==============================================================================

output "backend_info" {
  description = "Firebase App Hosting backend information"
  value = {
    backend_id   = google_firebase_app_hosting_backend.nexpo_backend.backend_id
    name         = google_firebase_app_hosting_backend.nexpo_backend.name
    location     = google_firebase_app_hosting_backend.nexpo_backend.location
    create_time  = google_firebase_app_hosting_backend.nexpo_backend.create_time
    update_time  = google_firebase_app_hosting_backend.nexpo_backend.update_time
    uri          = google_firebase_app_hosting_backend.nexpo_backend.uri
    
    # Repository configuration
    repository_url = var.github_repository_url
    branch         = var.github_branch
    root_directory = var.root_directory
  }
}

output "build_info" {
  description = "Firebase App Hosting build information"
  value = var.create_initial_build ? {
    build_id    = google_firebase_app_hosting_build.nexpo_build[0].build_id
    name        = google_firebase_app_hosting_build.nexpo_build[0].name
    create_time = google_firebase_app_hosting_build.nexpo_build[0].create_time
    update_time = google_firebase_app_hosting_build.nexpo_build[0].update_time
    state       = google_firebase_app_hosting_build.nexpo_build[0].state
  } : null
}

output "traffic_info" {
  description = "Firebase App Hosting traffic information"
  value = var.create_initial_build ? {
    name                = google_firebase_app_hosting_traffic.nexpo_traffic[0].name
    traffic_allocations = google_firebase_app_hosting_traffic.nexpo_traffic[0].traffic_allocations
    create_time         = google_firebase_app_hosting_traffic.nexpo_traffic[0].create_time
    update_time         = google_firebase_app_hosting_traffic.nexpo_traffic[0].update_time
  } : null
}

output "deployment_urls" {
  description = "Deployment URLs and endpoints"
  value = {
    backend_url     = google_firebase_app_hosting_backend.nexpo_backend.uri
    console_url     = "https://console.firebase.google.com/project/${var.project_id}/hosting"
    repository_url  = var.github_repository_url
    branch          = var.github_branch
  }
}

output "hosting_configuration" {
  description = "App Hosting configuration for CI/CD"
  value = {
    project_id      = var.project_id
    backend_id      = google_firebase_app_hosting_backend.nexpo_backend.backend_id
    location        = var.location
    
    # GitHub Actions configuration
    github_config = {
      repository_url = var.github_repository_url
      branch         = var.github_branch
      root_directory = var.root_directory
      
      # Example GitHub Actions workflow variables
      firebase_project_id = var.project_id
      firebase_backend_id = google_firebase_app_hosting_backend.nexpo_backend.backend_id
    }
  }
}
