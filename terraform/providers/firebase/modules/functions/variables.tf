# ==============================================================================
# FIREBASE CLOUD FUNCTIONS MODULE - VARIABLES
# ==============================================================================

variable "project_id" {
  description = "Firebase project ID"
  type        = string
}

variable "region" {
  description = "GCP region for Cloud Functions"
  type        = string
  default     = "us-central1"
}

# ==============================================================================
# CLOUD FUNCTIONS CONFIGURATION
# ==============================================================================

variable "cloud_functions" {
  description = "List of Cloud Functions to deploy"
  type = list(object({
    name         = string
    description  = optional(string, "")
    runtime      = optional(string, "nodejs20")
    entry_point  = optional(string, "main")
    timeout      = optional(number, 60)
    memory       = optional(number, 256)
    purpose      = optional(string, "general")
    
    # Source code configuration
    source_bucket = string
    source_object = string
    
    # Trigger configuration
    trigger_type = optional(string, "HTTP") # HTTP, PUBSUB, FIRESTORE, STORAGE, SCHEDULE
    
    # HTTP trigger specific
    allow_public_access        = optional(bool, false)
    allow_authenticated_access = optional(bool, true)
    
    # Pub/Sub trigger specific
    pubsub_topic = optional(string, "")
    
    # Firestore trigger specific
    firestore_event_type = optional(string, "providers/cloud.firestore/eventTypes/document.write")
    firestore_path       = optional(string, "")
    
    # Storage trigger specific
    storage_event_type = optional(string, "google.storage.object.finalize")
    storage_bucket     = optional(string, "")
    
    # Schedule trigger specific
    schedule_expression = optional(string, "")
    schedule_timezone   = optional(string, "UTC")
    schedule_payload    = optional(map(any), {})
    
    # Environment variables
    environment_variables = optional(map(string), {})
  }))
  
  default = [
    {
      name        = "helloWorld"
      description = "Simple Hello World function"
      runtime     = "nodejs20"
      entry_point = "helloWorld"
      source_bucket = "nexpo-functions-source"
      source_object = "hello-world.zip"
      trigger_type = "HTTP"
      allow_authenticated_access = true
      environment_variables = {
        NODE_ENV = "production"
      }
    },
    {
      name        = "processImage"
      description = "Process uploaded images"
      runtime     = "nodejs20"
      entry_point = "processImage"
      memory      = 512
      timeout     = 120
      source_bucket = "nexpo-functions-source"
      source_object = "process-image.zip"
      trigger_type = "STORAGE"
      storage_event_type = "google.storage.object.finalize"
      storage_bucket = "nexpo-uploads"
      environment_variables = {
        NODE_ENV = "production"
        MAX_IMAGE_SIZE = "5242880"
      }
    },
    {
      name        = "sendNotification"
      description = "Send push notifications"
      runtime     = "nodejs20"
      entry_point = "sendNotification"
      source_bucket = "nexpo-functions-source"
      source_object = "send-notification.zip"
      trigger_type = "FIRESTORE"
      firestore_event_type = "providers/cloud.firestore/eventTypes/document.create"
      firestore_path = "notifications/{documentId}"
      environment_variables = {
        NODE_ENV = "production"
        FCM_SERVER_KEY = "your-fcm-server-key"
      }
    },
    {
      name        = "dailyBackup"
      description = "Daily backup function"
      runtime     = "nodejs20"
      entry_point = "dailyBackup"
      source_bucket = "nexpo-functions-source"
      source_object = "daily-backup.zip"
      trigger_type = "SCHEDULE"
      schedule_expression = "0 2 * * *"
      schedule_timezone = "UTC"
      schedule_payload = {
        action = "backup"
        retention_days = 30
      }
      environment_variables = {
        NODE_ENV = "production"
        BACKUP_BUCKET = "nexpo-backups"
      }
    }
  ]
}

# ==============================================================================
# FUNCTION SOURCE BUCKET CONFIGURATION
# ==============================================================================

variable "create_source_bucket" {
  description = "Create a source bucket for function deployment packages"
  type        = bool
  default     = true
}

variable "source_bucket_name" {
  description = "Name of the source bucket for function packages"
  type        = string
  default     = "functions-source"
}

variable "source_bucket_location" {
  description = "Location of the source bucket"
  type        = string
  default     = "US"
}
