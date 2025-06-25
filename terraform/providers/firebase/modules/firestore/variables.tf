# ==============================================================================
# FIREBASE FIRESTORE MODULE - VARIABLES
# ==============================================================================

variable "project_id" {
  description = "Firebase project ID"
  type        = string
}

# ==============================================================================
# FIRESTORE DATABASE CONFIGURATION
# ==============================================================================

variable "location_id" {
  description = "Firestore database location"
  type        = string
  default     = "nam5"
  
  validation {
    condition = contains([
      "nam5", "us-central", "us-east1", "us-east4", "us-west1", "us-west2", "us-west3", "us-west4",
      "southamerica-east1", "europe-west1", "europe-west2", "europe-west3", "europe-west6",
      "asia-east1", "asia-east2", "asia-northeast1", "asia-northeast2", "asia-south1",
      "asia-southeast1", "asia-southeast2", "australia-southeast1"
    ], var.location_id)
    error_message = "Invalid Firestore location. Must be a valid Firestore location."
  }
}

variable "database_type" {
  description = "Firestore database type"
  type        = string
  default     = "FIRESTORE_NATIVE"
  
  validation {
    condition     = contains(["FIRESTORE_NATIVE", "DATASTORE_MODE"], var.database_type)
    error_message = "Database type must be either FIRESTORE_NATIVE or DATASTORE_MODE."
  }
}

variable "concurrency_mode" {
  description = "Firestore concurrency mode"
  type        = string
  default     = "OPTIMISTIC"
  
  validation {
    condition     = contains(["OPTIMISTIC", "PESSIMISTIC", "OPTIMISTIC_WITH_ENTITY_GROUPS"], var.concurrency_mode)
    error_message = "Concurrency mode must be OPTIMISTIC, PESSIMISTIC, or OPTIMISTIC_WITH_ENTITY_GROUPS."
  }
}

variable "app_engine_integration_mode" {
  description = "App Engine integration mode"
  type        = string
  default     = "DISABLED"
  
  validation {
    condition     = contains(["ENABLED", "DISABLED"], var.app_engine_integration_mode)
    error_message = "App Engine integration mode must be either ENABLED or DISABLED."
  }
}

# ==============================================================================
# FIRESTORE INDEXES CONFIGURATION
# ==============================================================================

variable "firestore_indexes" {
  description = "List of Firestore indexes to create"
  type = list(object({
    collection = string
    fields = list(object({
      field_path   = string
      order        = optional(string, "ASCENDING")
      array_config = optional(string, null)
    }))
    query_scope = optional(string, "COLLECTION")
  }))
  default = [
    {
      collection = "users"
      fields = [
        {
          field_path = "email"
          order      = "ASCENDING"
        },
        {
          field_path = "created_at"
          order      = "DESCENDING"
        }
      ]
    },
    {
      collection = "posts"
      fields = [
        {
          field_path = "published"
          order      = "ASCENDING"
        },
        {
          field_path = "created_at"
          order      = "DESCENDING"
        }
      ]
    },
    {
      collection = "posts"
      fields = [
        {
          field_path = "author"
          order      = "ASCENDING"
        },
        {
          field_path = "created_at"
          order      = "DESCENDING"
        }
      ]
    },
    {
      collection = "posts"
      fields = [
        {
          field_path = "tags"
          array_config = "CONTAINS"
        },
        {
          field_path = "created_at"
          order      = "DESCENDING"
        }
      ]
    }
  ]
}

# ==============================================================================
# SAMPLE DATA CONFIGURATION
# ==============================================================================

variable "create_sample_data" {
  description = "Create sample data for development purposes"
  type        = bool
  default     = false
}

# ==============================================================================
# BACKUP CONFIGURATION
# ==============================================================================

variable "enable_backup" {
  description = "Enable automatic Firestore backups"
  type        = bool
  default     = true
}

variable "backup_retention_days" {
  description = "Number of days to retain backups"
  type        = number
  default     = 30
  
  validation {
    condition     = var.backup_retention_days >= 1 && var.backup_retention_days <= 365
    error_message = "Backup retention must be between 1 and 365 days."
  }
}
