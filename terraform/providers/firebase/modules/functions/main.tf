# ==============================================================================
# FIREBASE CLOUD FUNCTIONS MODULE
# ==============================================================================

# ==============================================================================
# CLOUD FUNCTIONS
# ==============================================================================

# Create Cloud Functions
resource "google_cloudfunctions_function" "firebase_functions" {
  for_each = { for idx, func in var.cloud_functions : func.name => func }
  
  name        = each.value.name
  project     = var.project_id
  region      = var.region
  description = each.value.description
  
  runtime     = each.value.runtime
  entry_point = each.value.entry_point
  timeout     = each.value.timeout
  
  available_memory_mb = each.value.memory
  
  source_archive_bucket = each.value.source_bucket
  source_archive_object = each.value.source_object
  
  # HTTP trigger for most functions
  dynamic "https_trigger" {
    for_each = each.value.trigger_type == "HTTP" ? [1] : []
    content {
      security_level = "SECURE_ALWAYS"
    }
  }
  
  # Pub/Sub trigger
  dynamic "event_trigger" {
    for_each = each.value.trigger_type == "PUBSUB" ? [1] : []
    content {
      event_type = "providers/cloud.pubsub/eventTypes/topic.publish"
      resource   = "projects/${var.project_id}/topics/${each.value.pubsub_topic}"
    }
  }
  
  # Firestore trigger
  dynamic "event_trigger" {
    for_each = each.value.trigger_type == "FIRESTORE" ? [1] : []
    content {
      event_type = each.value.firestore_event_type
      resource   = "projects/${var.project_id}/databases/(default)/documents/${each.value.firestore_path}"
    }
  }
  
  # Storage trigger
  dynamic "event_trigger" {
    for_each = each.value.trigger_type == "STORAGE" ? [1] : []
    content {
      event_type = each.value.storage_event_type
      resource   = each.value.storage_bucket
    }
  }
  
  # Environment variables
  environment_variables = merge(
    each.value.environment_variables,
    {
      FIREBASE_PROJECT_ID = var.project_id
      GCP_PROJECT        = var.project_id
      FUNCTION_REGION    = var.region
    }
  )
  
  labels = {
    environment = "firebase"
    managed_by  = "terraform"
    purpose     = each.value.purpose
  }
}

# ==============================================================================
# IAM BINDINGS FOR FUNCTIONS
# ==============================================================================

# Allow functions to be invoked publicly (for HTTP triggers)
resource "google_cloudfunctions_function_iam_binding" "public_invoker" {
  for_each = { 
    for idx, func in var.cloud_functions : func.name => func 
    if func.trigger_type == "HTTP" && func.allow_public_access 
  }
  
  project        = var.project_id
  region         = var.region
  cloud_function = google_cloudfunctions_function.firebase_functions[each.key].name
  role           = "roles/cloudfunctions.invoker"
  
  members = [
    "allUsers",
  ]
}

# Allow authenticated users to invoke functions
resource "google_cloudfunctions_function_iam_binding" "authenticated_invoker" {
  for_each = { 
    for idx, func in var.cloud_functions : func.name => func 
    if func.trigger_type == "HTTP" && func.allow_authenticated_access 
  }
  
  project        = var.project_id
  region         = var.region
  cloud_function = google_cloudfunctions_function.firebase_functions[each.key].name
  role           = "roles/cloudfunctions.invoker"
  
  members = [
    "allAuthenticatedUsers",
  ]
}

# ==============================================================================
# FIREBASE FUNCTIONS CONFIGURATION
# ==============================================================================

# Service account for Firebase Functions
resource "google_service_account" "firebase_functions_sa" {
  account_id   = "firebase-functions-sa"
  display_name = "Firebase Functions Service Account"
  description  = "Service account for Firebase Cloud Functions"
  project      = var.project_id
}

# Grant necessary permissions to the service account
resource "google_project_iam_binding" "functions_sa_bindings" {
  for_each = toset([
    "roles/cloudsql.client",
    "roles/datastore.user",
    "roles/firebase.admin",
    "roles/storage.admin",
  ])
  
  project = var.project_id
  role    = each.value
  
  members = [
    "serviceAccount:${google_service_account.firebase_functions_sa.email}",
  ]
}

# ==============================================================================
# CLOUD SCHEDULER JOBS (for scheduled functions)
# ==============================================================================

resource "google_cloud_scheduler_job" "scheduled_functions" {
  for_each = { 
    for idx, func in var.cloud_functions : func.name => func 
    if func.trigger_type == "SCHEDULE" 
  }
  
  name        = "${each.value.name}-scheduler"
  project     = var.project_id
  region      = var.region
  description = "Scheduled trigger for ${each.value.name}"
  schedule    = each.value.schedule_expression
  time_zone   = each.value.schedule_timezone
  
  http_target {
    http_method = "POST"
    uri         = google_cloudfunctions_function.firebase_functions[each.key].https_trigger_url
    
    headers = {
      "Content-Type" = "application/json"
    }
    
    body = base64encode(jsonencode(each.value.schedule_payload))
  }
}
