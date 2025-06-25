# GCP Storage Module

# Storage bucket for assets
resource "google_storage_bucket" "assets" {
  name          = "${var.project_name}-${var.environment}-assets"
  location      = var.region
  project       = var.project_id
  storage_class = var.storage_class

  versioning {
    enabled = true
  }

  lifecycle_rule {
    condition {
      age = 30
    }
    action {
      type = "Delete"
    }
  }

  lifecycle_rule {
    condition {
      num_newer_versions = 3
    }
    action {
      type = "Delete"
    }
  }

  cors {
    origin          = var.cors_origins
    method          = ["GET", "HEAD"]
    response_header = ["*"]
    max_age_seconds = 3600
  }

  uniform_bucket_level_access = true
}

# Storage bucket for uploads
resource "google_storage_bucket" "uploads" {
  name          = "${var.project_name}-${var.environment}-uploads"
  location      = var.region
  project       = var.project_id
  storage_class = var.storage_class

  versioning {
    enabled = true
  }

  lifecycle_rule {
    condition {
      age = 90
    }
    action {
      type = "SetStorageClass"
      storage_class = "NEARLINE"
    }
  }

  lifecycle_rule {
    condition {
      age = 365
    }
    action {
      type = "SetStorageClass"
      storage_class = "COLDLINE"
    }
  }

  cors {
    origin          = var.cors_origins
    method          = ["GET", "HEAD", "PUT", "POST", "DELETE"]
    response_header = ["*"]
    max_age_seconds = 3600
  }

  uniform_bucket_level_access = true
}

# Storage bucket for backups
resource "google_storage_bucket" "backups" {
  name          = "${var.project_name}-${var.environment}-backups"
  location      = var.region
  project       = var.project_id
  storage_class = "NEARLINE"

  versioning {
    enabled = true
  }

  lifecycle_rule {
    condition {
      age = var.backup_retention_days
    }
    action {
      type = "Delete"
    }
  }

  uniform_bucket_level_access = true
}

# IAM bindings for assets bucket (public read)
resource "google_storage_bucket_iam_member" "assets_public" {
  bucket = google_storage_bucket.assets.name
  role   = "roles/storage.objectViewer"
  member = "allUsers"
}

# Service account for bucket access
resource "google_service_account" "storage" {
  account_id   = "${var.project_name}-${var.environment}-storage"
  display_name = "Storage Service Account"
  project      = var.project_id
}

# IAM bindings for service account
resource "google_storage_bucket_iam_member" "assets_admin" {
  bucket = google_storage_bucket.assets.name
  role   = "roles/storage.objectAdmin"
  member = "serviceAccount:${google_service_account.storage.email}"
}

resource "google_storage_bucket_iam_member" "uploads_admin" {
  bucket = google_storage_bucket.uploads.name
  role   = "roles/storage.objectAdmin"
  member = "serviceAccount:${google_service_account.storage.email}"
}

resource "google_storage_bucket_iam_member" "backups_admin" {
  bucket = google_storage_bucket.backups.name
  role   = "roles/storage.objectAdmin"
  member = "serviceAccount:${google_service_account.storage.email}"
}

# Bucket for Terraform state (if not already exists)
resource "google_storage_bucket" "terraform_state" {
  count         = var.create_state_bucket ? 1 : 0
  name          = "${var.project_name}-terraform-state"
  location      = var.region
  project       = var.project_id
  storage_class = "STANDARD"

  versioning {
    enabled = true
  }

  lifecycle_rule {
    condition {
      num_newer_versions = 10
    }
    action {
      type = "Delete"
    }
  }

  uniform_bucket_level_access = true
}
