# Firebase Module - Real-time services with Auth0 integration

# Enable required APIs
resource "google_project_service" "firebase" {
  service = "firebase.googleapis.com"
  project = var.project_id
}

resource "google_project_service" "firestore" {
  service = "firestore.googleapis.com"
  project = var.project_id
}

resource "google_project_service" "firebasestorage" {
  service = "firebasestorage.googleapis.com"
  project = var.project_id
}

resource "google_project_service" "fcm" {
  count   = var.enable_cloud_messaging ? 1 : 0
  service = "fcm.googleapis.com"
  project = var.project_id
}

# Initialize Firebase project
resource "google_firebase_project" "main" {
  provider = google-beta
  project  = var.project_id

  depends_on = [google_project_service.firebase]
}

# Firestore Database
resource "google_firestore_database" "main" {
  project     = var.project_id
  name        = "(default)"
  location_id = var.firestore_location
  type        = "FIRESTORE_NATIVE"

  # Production settings
  concurrency_mode = var.environment == "prod" ? "OPTIMISTIC" : "PESSIMISTIC"
  
  depends_on = [
    google_firebase_project.main,
    google_project_service.firestore
  ]
}

# Firestore indexes for common queries
resource "google_firestore_index" "user_email" {
  project    = var.project_id
  database   = google_firestore_database.main.name
  collection = "users"

  fields {
    field_path = "email"
    order      = "ASCENDING"
  }

  fields {
    field_path = "__name__"
    order      = "ASCENDING"
  }
}

resource "google_firestore_index" "chat_participants" {
  project    = var.project_id
  database   = google_firestore_database.main.name
  collection = "chats"

  fields {
    field_path   = "participants"
    array_config = "CONTAINS"
  }

  fields {
    field_path = "lastMessageAt"
    order      = "DESCENDING"
  }
}

# Firebase Storage
resource "google_firebase_storage_bucket" "main" {
  provider  = google-beta
  project   = var.project_id
  bucket_id = "${var.project_id}.appspot.com"

  depends_on = [
    google_firebase_project.main,
    google_project_service.firebasestorage
  ]
}

# Firebase Web App
resource "google_firebase_web_app" "main" {
  provider     = google-beta
  project      = var.project_id
  display_name = "${var.project_name}-${var.environment}-web"

  depends_on = [google_firebase_project.main]
}

# Firebase Android App (for React Native)
resource "google_firebase_android_app" "main" {
  provider     = google-beta
  project      = var.project_id
  display_name = "${var.project_name}-${var.environment}-android"
  package_name = "com.${replace(var.project_name, "-", "")}.${var.environment}"

  depends_on = [google_firebase_project.main]
}

# Firebase iOS App (for React Native)
resource "google_firebase_apple_app" "main" {
  provider     = google-beta
  project      = var.project_id
  display_name = "${var.project_name}-${var.environment}-ios"
  bundle_id    = "com.${replace(var.project_name, "-", "")}.${var.environment}"

  depends_on = [google_firebase_project.main]
}

# Service Account for Firebase Admin SDK (token exchange)
resource "google_service_account" "firebase_admin" {
  account_id   = "${var.project_name}-${var.environment}-fb-admin"
  display_name = "Firebase Admin SDK Service Account"
  project      = var.project_id
}

# Grant Firebase Admin permissions
resource "google_project_iam_member" "firebase_admin" {
  project = var.project_id
  role    = "roles/firebase.admin"
  member  = "serviceAccount:${google_service_account.firebase_admin.email}"
}

# Generate service account key for Admin SDK
resource "google_service_account_key" "firebase_admin" {
  service_account_id = google_service_account.firebase_admin.name
}

# Store service account key in Secret Manager
resource "google_secret_manager_secret" "firebase_admin_key" {
  secret_id = "${var.project_name}-${var.environment}-firebase-admin-key"
  project   = var.project_id

  replication {
    automatic = true
  }
}

resource "google_secret_manager_secret_version" "firebase_admin_key" {
  secret = google_secret_manager_secret.firebase_admin_key.id
  secret_data = base64decode(google_service_account_key.firebase_admin.private_key)
}

# Cloud Function for Auth0 to Firebase token exchange
resource "google_storage_bucket" "functions" {
  name     = "${var.project_id}-${var.environment}-functions"
  location = var.region
  project  = var.project_id
}

resource "google_storage_bucket_object" "token_exchange_function" {
  name   = "token-exchange-${timestamp()}.zip"
  bucket = google_storage_bucket.functions.name
  source = "${path.module}/functions/token-exchange.zip"
}

resource "google_cloudfunctions2_function" "token_exchange" {
  name        = "${var.project_name}-${var.environment}-token-exchange"
  location    = var.region
  project     = var.project_id
  description = "Exchange Auth0 tokens for Firebase custom tokens"

  build_config {
    runtime     = "nodejs20"
    entry_point = "exchangeToken"
    source {
      storage_source {
        bucket = google_storage_bucket.functions.name
        object = google_storage_bucket_object.token_exchange_function.name
      }
    }
  }

  service_config {
    max_instance_count               = 100
    min_instance_count               = 0
    available_memory                 = "256M"
    timeout_seconds                  = 60
    environment_variables = {
      AUTH0_DOMAIN         = var.auth0_domain
      AUTH0_API_IDENTIFIER = var.auth0_api_identifier
      FIREBASE_PROJECT_ID  = var.project_id
    }
    secret_environment_variables {
      key        = "FIREBASE_ADMIN_KEY"
      project_id = var.project_id
      secret     = google_secret_manager_secret.firebase_admin_key.secret_id
      version    = "latest"
    }
  }

  depends_on = [
    google_firebase_project.main,
    google_secret_manager_secret_version.firebase_admin_key
  ]
}

# Allow unauthenticated invocations for token exchange
resource "google_cloud_run_service_iam_member" "token_exchange_invoker" {
  project  = google_cloudfunctions2_function.token_exchange.project
  location = google_cloudfunctions2_function.token_exchange.location
  service  = google_cloudfunctions2_function.token_exchange.name
  role     = "roles/run.invoker"
  member   = "allUsers"
}

# Firestore Security Rules
resource "google_firebaserules_ruleset" "firestore" {
  source {
    files {
      content = file("${path.module}/rules/firestore.rules")
      name    = "firestore.rules"
    }
  }
  project = var.project_id

  depends_on = [google_firestore_database.main]
}

resource "google_firebaserules_release" "firestore" {
  name         = "cloud.firestore"
  ruleset_name = google_firebaserules_ruleset.firestore.name
  project      = var.project_id

  depends_on = [google_firebaserules_ruleset.firestore]
}

# Storage Security Rules
resource "google_firebaserules_ruleset" "storage" {
  source {
    files {
      content = file("${path.module}/rules/storage.rules")
      name    = "storage.rules"
    }
  }
  project = var.project_id

  depends_on = [google_firebase_storage_bucket.main]
}

resource "google_firebaserules_release" "storage" {
  name         = "firebase.storage/${google_firebase_storage_bucket.main.bucket_id}"
  ruleset_name = google_firebaserules_ruleset.storage.name
  project      = var.project_id

  depends_on = [google_firebaserules_ruleset.storage]
}
