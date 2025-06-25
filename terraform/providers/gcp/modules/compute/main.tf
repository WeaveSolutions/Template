# GCP Compute Module - Cloud Run

# Enable required APIs
resource "google_project_service" "cloud_run_api" {
  service = "run.googleapis.com"
  project = var.project_id
}

resource "google_project_service" "artifactregistry_api" {
  service = "artifactregistry.googleapis.com"
  project = var.project_id
}

# Artifact Registry for Docker images
resource "google_artifact_registry_repository" "main" {
  location      = var.region
  repository_id = "${var.project_name}-${var.environment}"
  description   = "Docker repository for ${var.project_name}"
  format        = "DOCKER"
  project       = var.project_id

  depends_on = [google_project_service.artifactregistry_api]
}

# Service account for Cloud Run
resource "google_service_account" "cloud_run" {
  account_id   = "${var.project_name}-${var.environment}-run"
  display_name = "Cloud Run Service Account"
  project      = var.project_id
}

# IAM bindings for service account
resource "google_project_iam_member" "cloud_run_invoker" {
  project = var.project_id
  role    = "roles/run.invoker"
  member  = "serviceAccount:${google_service_account.cloud_run.email}"
}

# Cloud Run service
resource "google_cloud_run_service" "main" {
  name     = "${var.project_name}-${var.environment}"
  location = var.region
  project  = var.project_id

  template {
    spec {
      service_account_name = google_service_account.cloud_run.email
      
      containers {
        image = var.container_image != "" ? var.container_image : "${var.region}-docker.pkg.dev/${var.project_id}/${google_artifact_registry_repository.main.repository_id}/${var.project_name}:latest"
        
        ports {
          container_port = var.container_port
        }

        resources {
          limits = {
            cpu    = var.cpu_limit
            memory = var.memory_limit
          }
        }

        dynamic "env" {
          for_each = var.environment_variables
          content {
            name  = env.key
            value = env.value
          }
        }

        # Database connection
        env {
          name  = "DATABASE_URL"
          value = "postgresql://${var.db_username}@/${var.db_name}?host=/cloudsql/${var.db_connection_name}"
        }

        # Cloud SQL Proxy sidecar would be configured here if needed
      }

      # Concurrency and scaling
      container_concurrency = var.container_concurrency
      timeout_seconds       = var.timeout_seconds
    }

    metadata {
      annotations = {
        "autoscaling.knative.dev/minScale"      = var.min_instances
        "autoscaling.knative.dev/maxScale"      = var.max_instances
        "run.googleapis.com/vpc-access-connector" = var.vpc_connector_name
        "run.googleapis.com/vpc-access-egress"    = "all-traffic"
      }
    }
  }

  traffic {
    percent         = 100
    latest_revision = true
  }

  autogenerate_revision_name = true

  depends_on = [google_project_service.cloud_run_api]
}

# Cloud Run domain mapping (if custom domain provided)
resource "google_cloud_run_domain_mapping" "main" {
  count    = var.custom_domain != "" ? 1 : 0
  location = google_cloud_run_service.main.location
  name     = var.custom_domain
  project  = var.project_id

  metadata {
    namespace = var.project_id
  }

  spec {
    route_name = google_cloud_run_service.main.name
  }
}

# Allow unauthenticated access (adjust based on requirements)
resource "google_cloud_run_service_iam_member" "public_access" {
  count    = var.allow_unauthenticated ? 1 : 0
  service  = google_cloud_run_service.main.name
  location = google_cloud_run_service.main.location
  role     = "roles/run.invoker"
  member   = "allUsers"
  project  = var.project_id
}

# Cloud Scheduler for warming (optional)
resource "google_cloud_scheduler_job" "warming" {
  count    = var.enable_warming ? 1 : 0
  name     = "${var.project_name}-${var.environment}-warming"
  schedule = "*/5 * * * *"  # Every 5 minutes
  project  = var.project_id
  region   = var.region

  http_target {
    http_method = "GET"
    uri         = google_cloud_run_service.main.status[0].url

    oidc_token {
      service_account_email = google_service_account.cloud_run.email
    }
  }
}

# Cloud Build trigger for CI/CD
resource "google_cloudbuild_trigger" "deploy" {
  count    = var.enable_ci_cd ? 1 : 0
  name     = "${var.project_name}-${var.environment}-deploy"
  project  = var.project_id
  location = var.region

  github {
    owner = var.github_owner
    name  = var.github_repo
    push {
      branch = var.environment == "prod" ? "^main$" : "^develop$"
    }
  }

  build {
    step {
      name = "gcr.io/cloud-builders/docker"
      args = [
        "build",
        "-t", "${var.region}-docker.pkg.dev/${var.project_id}/${google_artifact_registry_repository.main.repository_id}/${var.project_name}:$SHORT_SHA",
        "-t", "${var.region}-docker.pkg.dev/${var.project_id}/${google_artifact_registry_repository.main.repository_id}/${var.project_name}:latest",
        "."
      ]
    }

    step {
      name = "gcr.io/cloud-builders/docker"
      args = [
        "push",
        "--all-tags",
        "${var.region}-docker.pkg.dev/${var.project_id}/${google_artifact_registry_repository.main.repository_id}/${var.project_name}"
      ]
    }

    step {
      name = "gcr.io/google.com/cloudsdktool/cloud-sdk"
      args = [
        "run", "deploy", google_cloud_run_service.main.name,
        "--image", "${var.region}-docker.pkg.dev/${var.project_id}/${google_artifact_registry_repository.main.repository_id}/${var.project_name}:$SHORT_SHA",
        "--region", var.region,
        "--platform", "managed"
      ]
    }
  }
}
