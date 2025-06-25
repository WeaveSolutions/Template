# ==============================================================================
# FIREBASE STORAGE MODULE - OUTPUTS
# ==============================================================================

output "buckets_info" {
  description = "Information about created Firebase Storage buckets"
  value = {
    count = length(google_storage_bucket.firebase_buckets)
    buckets = {
      for name, bucket in google_storage_bucket.firebase_buckets : name => {
        name                = bucket.name
        location           = bucket.location
        url                = bucket.url
        self_link          = bucket.self_link
        versioning_enabled = bucket.versioning[0].enabled
        public_access      = contains(keys(google_storage_bucket_iam_binding.public_read), name)
        
        # Firebase-specific information
        firebase_bucket_id = google_firebase_storage_bucket.firebase_storage_buckets[name].bucket_id
      }
    }
  }
}

output "bucket_urls" {
  description = "URLs for accessing storage buckets"
  value = {
    for name, bucket in google_storage_bucket.firebase_buckets : name => {
      gs_url       = "gs://${bucket.name}"
      https_url    = "https://storage.googleapis.com/${bucket.name}"
      firebase_url = "https://firebasestorage.googleapis.com/v0/b/${bucket.name}"
    }
  }
}

output "sample_files_info" {
  description = "Information about sample files (if created)"
  value = var.create_sample_data ? {
    files = {
      for name, file in google_storage_bucket_object.sample_images : name => {
        name         = file.name
        bucket       = file.bucket
        size         = file.size
        content_type = file.content_type
        md5hash      = file.md5hash
        media_link   = file.media_link
        self_link    = file.self_link
      }
    }
  } : null
}

output "storage_configuration" {
  description = "Storage configuration for client applications"
  value = {
    project_id = var.project_id
    buckets = {
      for name, config in var.storage_buckets : name => {
        bucket_name = google_storage_bucket.firebase_buckets[name].name
        location    = config.location
        purpose     = config.purpose
        
        # Client SDK configuration
        web_config = {
          storageBucket = "${google_storage_bucket.firebase_buckets[name].name}"
          apiKey        = "your-firebase-api-key"
          authDomain    = "${var.project_id}.firebaseapp.com"
          projectId     = var.project_id
        }
        
        android_config = {
          storageBucket = "${google_storage_bucket.firebase_buckets[name].name}"
          projectId     = var.project_id
        }
        
        ios_config = {
          storageBucket = "${google_storage_bucket.firebase_buckets[name].name}"
          projectId     = var.project_id
        }
      }
    }
  }
}

output "cors_configuration" {
  description = "CORS configuration for storage buckets"
  value = {
    for name, bucket in google_storage_bucket.firebase_buckets : name => {
      origins     = bucket.cors[0].origin
      methods     = bucket.cors[0].method
      headers     = bucket.cors[0].response_header
      max_age     = bucket.cors[0].max_age_seconds
    }
  }
}

output "lifecycle_rules" {
  description = "Lifecycle rules applied to storage buckets"
  value = {
    for name, bucket in google_storage_bucket.firebase_buckets : name => [
      for rule in bucket.lifecycle_rule : {
        condition = {
          age                   = rule.condition[0].age
          matches_storage_class = rule.condition[0].matches_storage_class
        }
        action = {
          type          = rule.action[0].type
          storage_class = rule.action[0].storage_class
        }
      }
    ]
  }
}

output "iam_bindings" {
  description = "IAM bindings for storage buckets"
  value = {
    admin_bindings = {
      for name, binding in google_storage_bucket_iam_binding.firebase_storage_admin : name => {
        bucket  = binding.bucket
        role    = binding.role
        members = binding.members
      }
    }
    public_read_bindings = {
      for name, binding in google_storage_bucket_iam_binding.public_read : name => {
        bucket  = binding.bucket
        role    = binding.role
        members = binding.members
      }
    }
  }
}
