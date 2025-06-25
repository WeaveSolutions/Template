# ==============================================================================
# FIREBASE FIRESTORE MODULE
# ==============================================================================

# ==============================================================================
# FIRESTORE DATABASE
# ==============================================================================

resource "google_firestore_database" "nexpo_firestore" {
  provider                        = google-beta
  project                         = var.project_id
  name                           = "(default)"
  location_id                    = var.location_id
  type                           = var.database_type
  concurrency_mode               = var.concurrency_mode
  app_engine_integration_mode    = var.app_engine_integration_mode
  
  # Enable point-in-time recovery (PITR) - available for certain regions
  point_in_time_recovery_enablement = "POINT_IN_TIME_RECOVERY_ENABLED"
  
  # Prevent accidental deletion
  deletion_policy = "DELETE"
}

# ==============================================================================
# FIRESTORE INDEXES
# ==============================================================================

# Create indexes for efficient queries
resource "google_firestore_index" "indexes" {
  for_each = { for idx, index in var.firestore_indexes : idx => index }
  
  project    = var.project_id
  database   = google_firestore_database.nexpo_firestore.name
  collection = each.value.collection
  
  dynamic "fields" {
    for_each = each.value.fields
    content {
      field_path   = fields.value.field_path
      order        = fields.value.order
      array_config = fields.value.array_config
    }
  }
  
  query_scope = each.value.query_scope
  
  depends_on = [google_firestore_database.nexpo_firestore]
}

# ==============================================================================
# SAMPLE SEED DATA (Optional - for development)
# ==============================================================================

# Create sample collections and documents for development
resource "google_firestore_document" "sample_user" {
  count       = var.create_sample_data ? 1 : 0
  project     = var.project_id
  database    = google_firestore_database.nexpo_firestore.name
  collection  = "users"
  document_id = "sample_user_${random_id.sample_suffix[0].hex}"
  
  fields = jsonencode({
    email = {
      stringValue = "user@example.com"
    }
    name = {
      stringValue = "Sample User"
    }
    created_at = {
      timestampValue = timestamp()
    }
    active = {
      booleanValue = true
    }
    preferences = {
      mapValue = {
        fields = {
          theme = {
            stringValue = "dark"
          }
          notifications = {
            booleanValue = true
          }
        }
      }
    }
  })
  
  depends_on = [google_firestore_database.nexpo_firestore]
}

resource "google_firestore_document" "sample_post" {
  count       = var.create_sample_data ? 1 : 0
  project     = var.project_id
  database    = google_firestore_database.nexpo_firestore.name
  collection  = "posts"
  document_id = "sample_post_${random_id.sample_suffix[0].hex}"
  
  fields = jsonencode({
    title = {
      stringValue = "Welcome to Nexpo!"
    }
    content = {
      stringValue = "This is a sample post created by Terraform."
    }
    author = {
      stringValue = "admin"
    }
    published = {
      booleanValue = true
    }
    created_at = {
      timestampValue = timestamp()
    }
    tags = {
      arrayValue = {
        values = [
          { stringValue = "welcome" },
          { stringValue = "sample" },
          { stringValue = "terraform" }
        ]
      }
    }
    metadata = {
      mapValue = {
        fields = {
          category = {
            stringValue = "announcement"
          }
          priority = {
            integerValue = "1"
          }
        }
      }
    }
  })
  
  depends_on = [google_firestore_database.nexpo_firestore]
}

resource "random_id" "sample_suffix" {
  count       = var.create_sample_data ? 1 : 0
  byte_length = 4
}

# ==============================================================================
# FIRESTORE BACKUP CONFIGURATION
# ==============================================================================

# Create backup schedule (requires appropriate IAM permissions)
resource "google_firestore_backup_schedule" "daily_backup" {
  count    = var.enable_backup ? 1 : 0
  project  = var.project_id
  database = google_firestore_database.nexpo_firestore.name
  
  retention = var.backup_retention_days * 24 * 60 * 60 # Convert days to seconds
  
  daily_recurrence {}
  
  depends_on = [google_firestore_database.nexpo_firestore]
}
