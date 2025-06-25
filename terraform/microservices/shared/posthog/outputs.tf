# Shared PostHog Module Outputs

output "posthog_project_id" {
  description = "PostHog project ID for the service"
  value       = var.enable_posthog ? posthog_project.service[0].id : null
}

output "posthog_project_api_key" {
  description = "PostHog project API key"
  value       = var.enable_posthog ? posthog_project.service[0].api_token : null
  sensitive   = true
}

output "posthog_host" {
  description = "PostHog host URL"
  value       = var.posthog_host
}

output "service_name" {
  description = "Name of the service this PostHog project is for"
  value       = var.service_name
}

# Feature Flags
output "feature_flags" {
  description = "Map of feature flag keys for the service"
  value = var.enable_posthog ? {
    analytics_enabled     = posthog_feature_flag.service_analytics_enabled[0].key
    debug_logging        = posthog_feature_flag.debug_logging[0].key
    performance_monitoring = posthog_feature_flag.performance_monitoring[0].key
  } : {}
}

# Analytics Actions
output "analytics_actions" {
  description = "Configured analytics actions for the service"
  value = var.enable_posthog ? {
    service_request    = posthog_action.service_request[0].name
    service_error     = posthog_action.service_error[0].name
    service_performance = posthog_action.service_performance[0].name
  } : {}
}

output "custom_actions" {
  description = "Custom analytics actions for the service"
  value = var.enable_posthog ? {
    for key, action in posthog_action.custom_events : key => action.name
  } : {}
}

# Cohorts
output "cohorts" {
  description = "User cohorts for the service"
  value = var.enable_posthog ? {
    active_users = posthog_cohort.service_active_users[0].name
  } : {}
}

# Dashboard
output "dashboard_url" {
  description = "URL to the service analytics dashboard"
  value = var.enable_posthog ? "${var.posthog_host}/project/${posthog_project.service[0].id}/dashboard/${posthog_dashboard.service_analytics[0].id}" : null
}

# Webhook Configuration
output "webhook_configured" {
  description = "Whether webhook is configured for the service"
  value       = var.enable_posthog && var.webhook_endpoint != "" ? true : false
}

# Environment Variables for Application Integration
output "environment_variables" {
  description = "Environment variables for service PostHog integration"
  value = var.enable_posthog ? {
    POSTHOG_PROJECT_API_KEY = posthog_project.service[0].api_token
    POSTHOG_HOST           = var.posthog_host
    POSTHOG_PROJECT_ID     = posthog_project.service[0].id
    POSTHOG_SERVICE_NAME   = var.service_name
    
    # Feature flags
    POSTHOG_FEATURE_FLAGS_ENABLED = "true"
    
    # Common settings
    POSTHOG_CAPTURE_PAGEVIEW      = tostring(var.enable_pageview_capture)
    POSTHOG_SESSION_RECORDING     = tostring(var.enable_session_recording)
    POSTHOG_ANONYMIZE_IPS         = tostring(var.anonymize_ips)
    
    # Performance settings
    POSTHOG_SAMPLING_RATE  = tostring(var.sampling_rate)
    POSTHOG_BATCH_SIZE     = tostring(var.batch_size)
    POSTHOG_FLUSH_INTERVAL = tostring(var.flush_interval_ms)
    
    # Service features
    POSTHOG_DEBUG_LOGGING         = tostring(var.enable_debug_logging)
    POSTHOG_PERFORMANCE_MONITORING = tostring(var.enable_performance_monitoring)
    POSTHOG_ERROR_TRACKING        = tostring(var.enable_error_tracking)
    POSTHOG_USER_IDENTIFICATION   = tostring(var.enable_user_identification)
    
    # Integration
    POSTHOG_PARENT_PROJECT_ID = var.parent_project_id
    POSTHOG_CROSS_SERVICE_TRACKING = tostring(var.enable_cross_service_tracking)
    
    # Development
    POSTHOG_LOG_LEVEL = var.log_level
    
  } : {}
  
  sensitive = true
}

# Shared Configuration for Other Services
output "shared_config" {
  description = "Shared PostHog configuration that can be used by other services"
  value = var.enable_posthog ? {
    project_id      = posthog_project.service[0].id
    api_key         = posthog_project.service[0].api_token
    host           = var.posthog_host
    organization_id = var.posthog_organization_id
    
    # Settings
    anonymize_ips    = var.anonymize_ips
    sampling_rate    = var.sampling_rate
    batch_size       = var.batch_size
    flush_interval   = var.flush_interval_ms
    data_retention   = var.data_retention_days
    
    # Feature flags
    analytics_enabled     = posthog_feature_flag.service_analytics_enabled[0].key
    debug_logging        = posthog_feature_flag.debug_logging[0].key
    performance_monitoring = posthog_feature_flag.performance_monitoring[0].key
    
  } : null
  
  sensitive = true
}

# Cross-Service Integration
output "cross_service_project_id" {
  description = "Cross-service project ID for analytics integration"
  value = var.enable_posthog && var.parent_project_id != "" ? posthog_project.cross_service_link[0].id : null
}

# Event Schema Documentation
output "event_schema" {
  description = "Standard event schema for the service"
  value = {
    service_request = {
      event_name = "${var.service_name}_request"
      properties = {
        endpoint      = "string"
        method        = "string"
        status_code   = "number"
        response_time = "number"
        user_id       = "string"  # CRA root_id
        session_id    = "string"
      }
    }
    
    service_error = {
      event_name = "${var.service_name}_error"
      properties = {
        error_type    = "string"
        error_message = "string"
        endpoint      = "string"
        user_id       = "string"
        session_id    = "string"
        stack_trace   = "string"  # if debug logging enabled
      }
    }
    
    service_performance = {
      event_name = "${var.service_name}_performance"
      properties = {
        operation     = "string"
        duration_ms   = "number"
        memory_usage  = "number"
        cpu_usage     = "number"
        user_id       = "string"
      }
    }
  }
}

# Secret Manager Outputs (for different cloud providers)
output "aws_ssm_parameters" {
  description = "AWS SSM parameter names for PostHog configuration"
  value = var.provider == "aws" && var.enable_posthog ? {
    for key, value in local.service_posthog_config : key => "/nexpo/${var.service_name}/posthog/${key}"
  } : {}
}

output "gcp_secret_manager_secrets" {
  description = "GCP Secret Manager secret names for PostHog configuration"
  value = var.provider == "gcp" && var.enable_posthog ? {
    for key, value in local.service_posthog_config : key => "nexpo-${var.service_name}-posthog-${replace(lower(key), "_", "-")}"
  } : {}
}

output "azure_key_vault_secrets" {
  description = "Azure Key Vault secret names for PostHog configuration"
  value = var.provider == "azure" && var.enable_posthog ? {
    for key, value in local.service_posthog_config : key => "nexpo-${var.service_name}-posthog-${replace(lower(key), "_", "-")}"
  } : {}
}

# Terraform State Information
output "terraform_state" {
  description = "Terraform state information for PostHog resources"
  value = var.enable_posthog ? {
    service_name        = var.service_name
    project_created     = true
    feature_flags_count = 3
    actions_count       = 3 + length(var.custom_events)
    cohorts_count       = 1
    dashboard_created   = true
    webhook_enabled     = var.webhook_endpoint != ""
    cross_service_link  = var.parent_project_id != ""
    custom_events_count = length(var.custom_events)
  } : {
    service_name    = var.service_name
    project_created = false
  }
}

# Integration Helper
output "integration_config" {
  description = "Configuration for integrating with application code"
  value = var.enable_posthog ? {
    # SDK Configuration
    sdk_config = {
      api_key = posthog_project.service[0].api_token
      host    = var.posthog_host
      
      # Options
      capture_pageview    = var.enable_pageview_capture
      session_recording   = var.enable_session_recording
      capture_console_logs = var.enable_console_logs
      anonymize_ips       = var.anonymize_ips
      
      # Performance
      sampling_rate    = var.sampling_rate
      batch_size      = var.batch_size
      flush_interval  = var.flush_interval_ms
    }
    
    # Feature Flags
    feature_flags = {
      analytics_enabled     = "${var.service_name}-analytics-enabled"
      debug_logging        = "${var.service_name}-debug-logging"
      performance_monitoring = "${var.service_name}-performance-monitoring"
    }
    
    # Standard Events
    events = {
      request     = "${var.service_name}_request"
      error       = "${var.service_name}_error"
      performance = "${var.service_name}_performance"
    }
    
    # User Properties (should align with CRA schema)
    user_properties = {
      user_id           = "root_id"  # From CRA
      subscription_plan = "subscription.plan_id"
      user_role        = "security.access_level"
      created_at       = "identity.created_at"
    }
    
  } : null
  
  sensitive = true
}
