# ==============================================================================
# FIREBASE CLOUD FUNCTIONS MODULE - OUTPUTS
# ==============================================================================

output "functions_info" {
  description = "Information about deployed Cloud Functions"
  value = {
    count = length(google_cloudfunctions_function.firebase_functions)
    functions = {
      for name, func in google_cloudfunctions_function.firebase_functions : name => {
        name        = func.name
        description = func.description
        runtime     = func.runtime
        entry_point = func.entry_point
        region      = func.region
        timeout     = func.timeout
        memory      = func.available_memory_mb
        
        # URLs and triggers
        https_trigger_url = func.https_trigger_url
        trigger_type     = func.event_trigger != null ? "EVENT" : "HTTP"
        
        # Status and metadata
        status            = func.status
        version_id        = func.version_id
        source_archive_url = func.source_archive_url
        
        # Labels and environment
        labels                = func.labels
        environment_variables = func.environment_variables
      }
    }
  }
}

output "function_urls" {
  description = "HTTP trigger URLs for functions"
  value = {
    for name, func in google_cloudfunctions_function.firebase_functions : name => {
      https_url = func.https_trigger_url
      region    = func.region
    }
  }
}

output "service_account_info" {
  description = "Firebase Functions service account information"
  value = {
    email       = google_service_account.firebase_functions_sa.email
    name        = google_service_account.firebase_functions_sa.name
    unique_id   = google_service_account.firebase_functions_sa.unique_id
    display_name = google_service_account.firebase_functions_sa.display_name
  }
}

output "iam_bindings" {
  description = "IAM bindings for Cloud Functions"
  value = {
    public_invokers = {
      for name, binding in google_cloudfunctions_function_iam_binding.public_invoker : name => {
        function = binding.cloud_function
        role     = binding.role
        members  = binding.members
      }
    }
    authenticated_invokers = {
      for name, binding in google_cloudfunctions_function_iam_binding.authenticated_invoker : name => {
        function = binding.cloud_function
        role     = binding.role
        members  = binding.members
      }
    }
  }
}

output "scheduled_jobs" {
  description = "Cloud Scheduler jobs for scheduled functions"
  value = {
    for name, job in google_cloud_scheduler_job.scheduled_functions : name => {
      name        = job.name
      schedule    = job.schedule
      time_zone   = job.time_zone
      description = job.description
      state       = job.state
    }
  }
}

output "function_configuration" {
  description = "Configuration information for client applications"
  value = {
    project_id = var.project_id
    region     = var.region
    
    # Client SDK configuration
    web_config = {
      projectId    = var.project_id
      functionURL  = "https://${var.region}-${var.project_id}.cloudfunctions.net"
      
      functions = {
        for name, func in google_cloudfunctions_function.firebase_functions : name => {
          url = func.https_trigger_url
        }
      }
    }
    
    android_config = {
      projectId = var.project_id
      region    = var.region
    }
    
    ios_config = {
      projectId = var.project_id
      region    = var.region
    }
  }
}

output "deployment_info" {
  description = "Function deployment information"
  value = {
    for name, func in google_cloudfunctions_function.firebase_functions : name => {
      source_bucket = func.source_archive_bucket
      source_object = func.source_archive_object
      build_id      = func.build_id
      version_id    = func.version_id
      update_time   = func.update_time
    }
  }
}

output "monitoring_info" {
  description = "Monitoring and logging information"
  value = {
    project_id = var.project_id
    region     = var.region
    
    # Logging URLs
    logs_url = "https://console.cloud.google.com/logs/query;query=resource.type%3D%22cloud_function%22?project=${var.project_id}"
    
    # Monitoring URLs
    monitoring_url = "https://console.cloud.google.com/monitoring?project=${var.project_id}"
    
    # Function-specific log filters
    log_filters = {
      for name, func in google_cloudfunctions_function.firebase_functions : name => {
        filter = "resource.type=\"cloud_function\" AND resource.labels.function_name=\"${func.name}\""
      }
    }
  }
}
