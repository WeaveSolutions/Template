# PostHog Integration Outputs for API Gateway

output "posthog_project_id" {
  description = "PostHog project ID for the API Gateway"
  value       = var.enable_posthog ? posthog_project.api_gateway[0].id : null
}

output "posthog_project_api_key" {
  description = "PostHog project API key"
  value       = var.enable_posthog ? posthog_project.api_gateway[0].api_token : null
  sensitive   = true
}

output "posthog_host" {
  description = "PostHog host URL"
  value       = var.posthog_host
}

output "feature_flags" {
  description = "Map of feature flag keys and their current status"
  value = var.enable_posthog ? {
    api_analytics_enabled = posthog_feature_flag.api_analytics_enabled[0].key
    detailed_api_logging  = posthog_feature_flag.detailed_api_logging[0].key
    rate_limit_analytics  = posthog_feature_flag.rate_limit_analytics[0].key
  } : {}
}

output "analytics_actions" {
  description = "List of configured PostHog actions for analytics"
  value = var.enable_posthog ? {
    api_request        = posthog_action.api_request[0].name
    authentication    = posthog_action.authentication_event[0].name
    error_tracking    = posthog_action.error_event[0].name
  } : {}
}

output "cohorts" {
  description = "Configured user cohorts for segmentation"
  value = var.enable_posthog ? {
    api_power_users     = posthog_cohort.api_power_users[0].name
    premium_subscribers = posthog_cohort.premium_subscribers[0].name
  } : {}
}

output "dashboard_url" {
  description = "URL to the PostHog dashboard"
  value = var.enable_posthog ? "${var.posthog_host}/project/${posthog_project.api_gateway[0].id}/dashboard/${posthog_dashboard.api_gateway_analytics[0].id}" : null
}

output "webhook_configured" {
  description = "Whether webhook is configured for real-time events"
  value       = var.enable_posthog && var.webhook_endpoint != "" ? true : false
}

# Environment Variables for Application Integration
output "environment_variables" {
  description = "Environment variables for application PostHog integration"
  value = var.enable_posthog ? {
    POSTHOG_PROJECT_API_KEY = posthog_project.api_gateway[0].api_token
    POSTHOG_HOST           = var.posthog_host
    POSTHOG_PROJECT_ID     = posthog_project.api_gateway[0].id
    
    # Feature flags
    POSTHOG_FEATURE_FLAGS_ENABLED = "true"
    
    # Analytics settings
    POSTHOG_CAPTURE_PAGEVIEW      = "false"
    POSTHOG_CAPTURE_API_REQUESTS  = "true"
    POSTHOG_SESSION_RECORDING     = tostring(var.enable_session_recording)
    POSTHOG_ANONYMIZE_IPS         = tostring(var.anonymize_ips)
    
    # Performance settings
    POSTHOG_SAMPLING_RATE  = tostring(var.sampling_rate)
    POSTHOG_BATCH_SIZE     = tostring(var.batch_size)
    POSTHOG_FLUSH_INTERVAL = tostring(var.flush_interval_ms)
    
    # Analytics features
    POSTHOG_API_ANALYTICS         = tostring(var.enable_api_analytics)
    POSTHOG_ERROR_TRACKING        = tostring(var.enable_error_tracking)
    POSTHOG_PERFORMANCE_MONITORING = tostring(var.enable_performance_monitoring)
    POSTHOG_BUSINESS_METRICS      = tostring(var.enable_business_metrics)
    
    # Social login tracking
    POSTHOG_SOCIAL_LOGIN_TRACKING = tostring(var.track_social_login_providers)
    POSTHOG_CONVERSION_TRACKING   = tostring(var.social_login_conversion_tracking)
    
  } : {}
  
  sensitive = true
}

# Configuration for other microservices
output "shared_posthog_config" {
  description = "Shared PostHog configuration for other microservices"
  value = var.enable_posthog ? {
    project_id      = posthog_project.api_gateway[0].id
    api_key         = posthog_project.api_gateway[0].api_token
    host           = var.posthog_host
    anonymize_ips  = var.anonymize_ips
    sampling_rate  = var.sampling_rate
    batch_size     = var.batch_size
    flush_interval = var.flush_interval_ms
  } : null
  
  sensitive = true
}

# Analytics Schema for Documentation
output "event_schema" {
  description = "Standard event schema for API Gateway analytics"
  value = {
    api_request = {
      event_name = "api_request"
      properties = {
        endpoint      = "string"
        method        = "string" 
        status_code   = "number"
        response_time = "number"
        user_agent    = "string"
        ip_address    = "string"  # anonymized if enabled
        user_id       = "string"  # CRA root_id
        session_id    = "string"
        request_size  = "number"
        response_size = "number"
      }
    }
    
    auth_attempt = {
      event_name = "auth_attempt"
      properties = {
        provider      = "string"  # google, apple, facebook, etc.
        success       = "boolean"
        failure_reason = "string"
        user_id       = "string"
        session_id    = "string"
        ip_address    = "string"
        user_agent    = "string"
      }
    }
    
    api_error = {
      event_name = "api_error"
      properties = {
        error_code    = "string"
        error_message = "string"
        endpoint      = "string"
        method        = "string"
        user_id       = "string"
        session_id    = "string"
        stack_trace   = "string"  # if detailed logging enabled
      }
    }
    
    rate_limit_exceeded = {
      event_name = "rate_limit_exceeded"
      properties = {
        endpoint       = "string"
        user_id        = "string"
        current_limit  = "number"
        window_seconds = "number"
        requests_count = "number"
      }
    }
  }
}

# Terraform State Information
output "terraform_state" {
  description = "Terraform state information for PostHog resources"
  value = var.enable_posthog ? {
    project_created     = true
    feature_flags_count = 3
    actions_count       = 3
    cohorts_count       = 2
    dashboard_created   = true
    webhook_enabled     = var.webhook_endpoint != ""
  } : {
    project_created = false
  }
}
