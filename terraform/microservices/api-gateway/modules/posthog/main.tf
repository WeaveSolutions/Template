# PostHog Analytics Integration for API Gateway
# This module configures PostHog analytics for tracking API usage, user behavior,
# and business metrics across all microservices

terraform {
  required_providers {
    posthog = {
      source  = "PostHog/posthog"
      version = "~> 0.1"
    }
  }
}

# PostHog Project Configuration
resource "posthog_project" "api_gateway" {
  count = var.enable_posthog ? 1 : 0
  
  name = "${var.project_name}-api-gateway"
  organization_id = var.posthog_organization_id
  
  # Configure project settings
  session_recording_opt_in = var.enable_session_recording
  capture_console_log_opt_in = var.enable_console_logs
  inject_web_apps = var.enable_web_injection
  
  # Data retention settings
  data_attributes = var.data_retention_days
  
  # GDPR compliance
  anonymize_ips = var.anonymize_ips
  
  tags = merge(var.common_tags, {
    Service     = "api-gateway"
    Analytics   = "posthog"
    Environment = var.environment
  })
}

# Feature Flags for API Gateway Analytics
resource "posthog_feature_flag" "api_analytics_enabled" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = posthog_project.api_gateway[0].id
  key        = "api-analytics-enabled"
  name       = "API Analytics Tracking"
  
  filters {
    groups {
      properties {
        key   = "environment"
        value = var.environment
        type  = "person"
      }
      rollout_percentage = var.analytics_rollout_percentage
    }
  }
  
  active = true
}

resource "posthog_feature_flag" "detailed_api_logging" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = posthog_project.api_gateway[0].id
  key        = "detailed-api-logging"
  name       = "Detailed API Request Logging"
  
  filters {
    groups {
      properties {
        key   = "user_role"
        value = ["admin", "developer"]
        type  = "person"
      }
      rollout_percentage = 100
    }
  }
  
  active = var.enable_detailed_logging
}

resource "posthog_feature_flag" "rate_limit_analytics" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = posthog_project.api_gateway[0].id
  key        = "rate-limit-analytics"
  name       = "Rate Limiting Analytics"
  
  filters {
    groups {
      properties {
        key   = "subscription_plan"
        value = ["premium", "enterprise"]
        type  = "person"
      }
      rollout_percentage = 100
    }
  }
  
  active = true
}

# Analytics Events Configuration
resource "posthog_action" "api_request" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = posthog_project.api_gateway[0].id
  name       = "API Request"
  description = "Track all API requests through the gateway"
  
  steps {
    event = "api_request"
    properties {
      key   = "endpoint"
      value = "is_set"
    }
    properties {
      key   = "method"
      value = "is_set"
    }
    properties {
      key   = "status_code"
      value = "is_set"
    }
  }
  
  tags = ["api", "gateway", "requests"]
}

resource "posthog_action" "authentication_event" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = posthog_project.api_gateway[0].id
  name       = "Authentication Event"
  description = "Track authentication attempts and successes"
  
  steps {
    event = "auth_attempt"
    properties {
      key   = "provider"
      value = "is_set"
    }
    properties {
      key   = "success"
      value = "is_set"
    }
  }
  
  tags = ["auth", "security", "login"]
}

resource "posthog_action" "error_event" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = posthog_project.api_gateway[0].id
  name       = "API Error"
  description = "Track API errors and failures"
  
  steps {
    event = "api_error"
    properties {
      key   = "error_code"
      value = "is_set"
    }
    properties {
      key   = "endpoint"
      value = "is_set"
    }
  }
  
  tags = ["errors", "monitoring", "api"]
}

# Dashboard Configuration
resource "posthog_dashboard" "api_gateway_analytics" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = posthog_project.api_gateway[0].id
  name       = "API Gateway Analytics"
  description = "Real-time analytics for API Gateway performance and usage"
  
  tags = ["api-gateway", "analytics", "monitoring"]
}

# Cohorts for User Segmentation
resource "posthog_cohort" "api_power_users" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = posthog_project.api_gateway[0].id
  name       = "API Power Users"
  description = "Users with high API usage"
  
  filters {
    properties {
      key      = "api_calls_count"
      value    = 100
      operator = "gte"
      type     = "person"
    }
  }
  
  is_static = false
}

resource "posthog_cohort" "premium_subscribers" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = posthog_project.api_gateway[0].id
  name       = "Premium Subscribers"
  description = "Users with premium or enterprise subscriptions"
  
  filters {
    properties {
      key      = "subscription_plan"
      value    = ["premium", "enterprise"]
      operator = "in"
      type     = "person"
    }
  }
  
  is_static = false
}

# Webhook for Real-time Events
resource "posthog_webhook" "api_gateway_events" {
  count = var.enable_posthog && var.webhook_endpoint != "" ? 1 : 0
  
  project_id = posthog_project.api_gateway[0].id
  target     = var.webhook_endpoint
  
  # Configure which events to send
  event_names = [
    "api_request",
    "auth_attempt",
    "api_error",
    "rate_limit_exceeded"
  ]
  
  # Security
  secret_token = var.webhook_secret
  
  # Only send to webhook in production
  enabled = var.environment == "production"
}

# Environment Variables for Application
locals {
  posthog_config = var.enable_posthog ? {
    POSTHOG_PROJECT_API_KEY = posthog_project.api_gateway[0].api_token
    POSTHOG_HOST           = var.posthog_host
    POSTHOG_PROJECT_ID     = posthog_project.api_gateway[0].id
    
    # Feature flags
    POSTHOG_FEATURE_FLAGS_ENABLED = "true"
    
    # Analytics settings
    POSTHOG_CAPTURE_PAGEVIEW      = "false"  # API Gateway doesn't have pages
    POSTHOG_CAPTURE_API_REQUESTS  = "true"
    POSTHOG_SESSION_RECORDING     = tostring(var.enable_session_recording)
    POSTHOG_ANONYMIZE_IPS         = tostring(var.anonymize_ips)
    
    # Sampling for high-volume APIs
    POSTHOG_SAMPLING_RATE = var.sampling_rate
    
    # Batch settings for performance
    POSTHOG_BATCH_SIZE     = var.batch_size
    POSTHOG_FLUSH_INTERVAL = var.flush_interval_ms
    
  } : {}
}

# Store configuration in parameter store/secret manager
resource "aws_ssm_parameter" "posthog_config" {
  for_each = var.provider == "aws" && var.enable_posthog ? local.posthog_config : {}
  
  name  = "/nexpo/api-gateway/posthog/${each.key}"
  type  = contains(["POSTHOG_PROJECT_API_KEY"], each.key) ? "SecureString" : "String"
  value = each.value
  
  tags = merge(var.common_tags, {
    Service   = "api-gateway"
    Component = "posthog-config"
  })
}
