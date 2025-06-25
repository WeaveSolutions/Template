# ==============================================================================
# FIREBASE STORAGE MODULE - VARIABLES
# ==============================================================================

variable "project_id" {
  description = "Firebase project ID"
  type        = string
}

# ==============================================================================
# STORAGE BUCKETS CONFIGURATION
# ==============================================================================

variable "storage_buckets" {
  description = "List of Firebase Storage buckets to create"
  type = list(object({
    name                = string
    location           = optional(string, "US")
    purpose            = optional(string, "general")
    versioning_enabled = optional(bool, false)
    public_read_access = optional(bool, false)
    
    # CORS configuration
    cors_origins  = optional(list(string), ["*"])
    cors_methods  = optional(list(string), ["GET", "HEAD", "PUT", "POST", "DELETE"])
    cors_headers  = optional(list(string), ["Authorization", "Content-Type", "x-goog-resumable"])
    cors_max_age  = optional(number, 3600)
    
    # Lifecycle rules
    lifecycle_rules = optional(list(object({
      age                    = optional(number, 30)
      matches_storage_class  = optional(list(string), [])
      action_type           = optional(string, "Delete")
      action_storage_class  = optional(string, null)
    })), [])
  }))
  
  default = [
    {
      name               = "uploads"
      location          = "US"
      purpose           = "user-uploads"
      versioning_enabled = true
      public_read_access = false
      lifecycle_rules = [
        {
          age         = 90
          action_type = "SetStorageClass"
          action_storage_class = "COLDLINE"
        }
      ]
    },
    {
      name               = "public-assets"
      location          = "US"
      purpose           = "public-content"
      versioning_enabled = false
      public_read_access = true
      lifecycle_rules = [
        {
          age         = 365
          action_type = "Delete"
        }
      ]
    }
  ]
}

# ==============================================================================
# SAMPLE DATA CONFIGURATION
# ==============================================================================

variable "create_sample_data" {
  description = "Create sample files in storage buckets for development"
  type        = bool
  default     = false
}

variable "sample_files" {
  description = "Sample files to upload for development"
  type = map(object({
    name         = string
    bucket       = string
    source_path  = string
    content_type = string
    metadata     = optional(map(string), {})
  }))
  default = {
    sample_image = {
      name         = "samples/welcome.jpg"
      bucket       = "public-assets"
      source_path  = "/dev/null"  # Replace with actual file path
      content_type = "image/jpeg"
      metadata = {
        description = "Sample welcome image"
        category    = "welcome"
      }
    }
    sample_doc = {
      name         = "docs/readme.txt"
      bucket       = "public-assets"
      source_path  = "/dev/null"  # Replace with actual file path
      content_type = "text/plain"
      metadata = {
        description = "Sample readme document"
        category    = "documentation"
      }
    }
  }
}
