# ==============================================================================
# FIREBASE STORAGE MODULE
# ==============================================================================

# ==============================================================================
# CLOUD STORAGE BUCKETS
# ==============================================================================

# Create Cloud Storage buckets
resource "google_storage_bucket" "firebase_buckets" {
  for_each = { for idx, bucket in var.storage_buckets : bucket.name => bucket }
  
  name          = "${var.project_id}-${each.value.name}"
  project       = var.project_id
  location      = each.value.location
  force_destroy = false
  
  # Enable uniform bucket-level access
  uniform_bucket_level_access = true
  
  # Enable versioning
  versioning {
    enabled = each.value.versioning_enabled
  }
  
  # Lifecycle management
  dynamic "lifecycle_rule" {
    for_each = each.value.lifecycle_rules
    content {
      condition {
        age                   = lifecycle_rule.value.age
        matches_storage_class = lifecycle_rule.value.matches_storage_class
      }
      action {
        type          = lifecycle_rule.value.action_type
        storage_class = lifecycle_rule.value.action_storage_class
      }
    }
  }
  
  # CORS configuration for web apps
  cors {
    origin          = each.value.cors_origins
    method          = each.value.cors_methods
    response_header = each.value.cors_headers
    max_age_seconds = each.value.cors_max_age
  }
  
  labels = {
    environment = "firebase"
    managed_by  = "terraform"
    purpose     = each.value.purpose
  }
}

# Make buckets accessible to Firebase
resource "google_firebase_storage_bucket" "firebase_storage_buckets" {
  provider  = google-beta
  for_each  = { for idx, bucket in var.storage_buckets : bucket.name => bucket }
  
  project   = var.project_id
  bucket_id = google_storage_bucket.firebase_buckets[each.key].name
  
  depends_on = [google_storage_bucket.firebase_buckets]
}

# ==============================================================================
# SAMPLE STORAGE OBJECTS (for development)
# ==============================================================================

# Upload sample files for development
resource "google_storage_bucket_object" "sample_images" {
  for_each = var.create_sample_data ? var.sample_files : {}
  
  name   = each.value.name
  bucket = google_storage_bucket.firebase_buckets[each.value.bucket].name
  source = each.value.source_path
  
  # Set content type
  content_type = each.value.content_type
  
  # Metadata
  metadata = each.value.metadata
  
  depends_on = [google_storage_bucket.firebase_buckets]
}

# ==============================================================================
# IAM BINDINGS FOR STORAGE
# ==============================================================================

# Allow Firebase service account to manage storage
resource "google_storage_bucket_iam_binding" "firebase_storage_admin" {
  for_each = { for idx, bucket in var.storage_buckets : bucket.name => bucket }
  
  bucket = google_storage_bucket.firebase_buckets[each.key].name
  role   = "roles/storage.admin"
  
  members = [
    "serviceAccount:firebase-adminsdk-${random_string.service_account_suffix.result}@${var.project_id}.iam.gserviceaccount.com",
  ]
  
  depends_on = [google_storage_bucket.firebase_buckets]
}

# Public read access for public buckets
resource "google_storage_bucket_iam_binding" "public_read" {
  for_each = { 
    for idx, bucket in var.storage_buckets : bucket.name => bucket 
    if bucket.public_read_access 
  }
  
  bucket = google_storage_bucket.firebase_buckets[each.key].name
  role   = "roles/storage.objectViewer"
  
  members = [
    "allUsers",
  ]
  
  depends_on = [google_storage_bucket.firebase_buckets]
}

resource "random_string" "service_account_suffix" {
  length  = 6
  special = false
  upper   = false
}
