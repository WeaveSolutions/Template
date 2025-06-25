# Shared PostHog Analytics Module for Microservices
# This module provides a common PostHog integration that can be used
# across all microservices in the Nexpo application

terraform {
  required_providers {
    posthog = {
      source  = "PostHog/posthog"
      version = "~> 0.1"
    }
  }
}

# Service-specific PostHog Project (inherits from main project)
resource "posthog_project" "service" {
  count = var.enable_posthog ? 1 : 0
  
  name = "${var.project_name}-${var.service_name}"
  organization_id = var.posthog_organization_id != "" ? var.posthog_organization_id : var.parent_organization_id
  
  # Inherit settings from parent project if provided
  session_recording_opt_in = var.enable_session_recording
  capture_console_log_opt_in = var.enable_console_logs
  inject_web_apps = var.enable_web_injection
  
  # Data retention
  data_attributes = var.data_retention_days
  
  # Privacy settings
  anonymize_ips = var.anonymize_ips
  
  tags = merge(var.common_tags, {
    Service     = var.service_name
    Analytics   = "posthog"
    Environment = var.environment
    Parent      = var.parent_project_id != "" ? "inherited" : "standalone"
  })
}

# Common Feature Flags for All Services
resource "posthog_feature_flag" "service_analytics_enabled" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = posthog_project.service[0].id
  key        = "${var.service_name}-analytics-enabled"
  name       = "${title(var.service_name)} Service Analytics"
  
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

resource "posthog_feature_flag" "debug_logging" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = posthog_project.service[0].id
  key        = "${var.service_name}-debug-logging"
  name       = "${title(var.service_name)} Debug Logging"
  
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
  
  active = var.enable_debug_logging
}

resource "posthog_feature_flag" "performance_monitoring" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = posthog_project.service[0].id
  key        = "${var.service_name}-performance-monitoring"
  name       = "${title(var.service_name)} Performance Monitoring"
  
  filters {
    groups {
      properties {
        key   = "environment"
        value = ["staging", "prod"]
        type  = "person"
      }
      rollout_percentage = 100
    }
  }
  
  active = var.enable_performance_monitoring
}

# Common Analytics Actions
resource "posthog_action" "service_request" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = posthog_project.service[0].id
  name       = "${title(var.service_name)} Request"
  description = "Track requests to ${var.service_name} service"
  
  steps {
    event = "${var.service_name}_request"
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
  
  tags = [var.service_name, "request", "api"]
}

resource "posthog_action" "service_error" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = posthog_project.service[0].id
  name       = "${title(var.service_name)} Error"
  description = "Track errors in ${var.service_name} service"
  
  steps {
    event = "${var.service_name}_error"
    properties {
      key   = "error_type"
      value = "is_set"
    }
    properties {
      key   = "error_message"
      value = "is_set"
    }
  }
  
  tags = [var.service_name, "error", "monitoring"]
}

resource "posthog_action" "service_performance" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = posthog_project.service[0].id
  name       = "${title(var.service_name)} Performance"
  description = "Track performance metrics for ${var.service_name} service"
  
  steps {
    event = "${var.service_name}_performance"
    properties {
      key   = "operation"
      value = "is_set"
    }
    properties {
      key   = "duration_ms"
      value = "is_set"
    }
  }
  
  tags = [var.service_name, "performance", "metrics"]
}

# Service Dashboard
resource "posthog_dashboard" "service_analytics" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = posthog_project.service[0].id
  name       = "${title(var.service_name)} Service Analytics"
  description = "Analytics dashboard for ${var.service_name} microservice"
  
  tags = [var.service_name, "analytics", "dashboard"]
}

# Common Cohorts
resource "posthog_cohort" "service_active_users" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = posthog_project.service[0].id
  name       = "${title(var.service_name)} Active Users"
  description = "Users actively using ${var.service_name} service"
  
  filters {
    properties {
      key      = "${var.service_name}_last_active"
      value    = 7
      operator = "relative_date_range"
      type     = "person"
    }
  }
  
  is_static = false
}

# Webhook for Service Events
resource "posthog_webhook" "service_events" {
  count = var.enable_posthog && var.webhook_endpoint != "" ? 1 : 0
  
  project_id = posthog_project.service[0].id
  target     = var.webhook_endpoint
  
  # Service-specific critical events
  event_names = concat(
    ["${var.service_name}_error"],
    var.additional_webhook_events
  )
  
  # Security
  secret_token = var.webhook_secret
  
  # Enable based on environment
  enabled = contains(["staging", "prod"], var.environment)
}

# Environment Variables for Service
locals {
  service_posthog_config = var.enable_posthog ? {
    POSTHOG_PROJECT_API_KEY = posthog_project.service[0].api_token
    POSTHOG_HOST           = var.posthog_host
    POSTHOG_PROJECT_ID     = posthog_project.service[0].id
    
    # Service identification
    POSTHOG_SERVICE_NAME = var.service_name
    
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
    
    # Service-specific toggles
    POSTHOG_DEBUG_LOGGING         = tostring(var.enable_debug_logging)
    POSTHOG_PERFORMANCE_MONITORING = tostring(var.enable_performance_monitoring)
    POSTHOG_ERROR_TRACKING        = tostring(var.enable_error_tracking)
    
    # Parent project integration
    POSTHOG_PARENT_PROJECT_ID = var.parent_project_id
    
  } : {}
}

# Store configuration based on cloud provider
resource "aws_ssm_parameter" "service_posthog_config" {
  for_each = var.provider == "aws" && var.enable_posthog ? local.service_posthog_config : {}
  
  name  = "/nexpo/${var.service_name}/posthog/${each.key}"
  type  = contains(["POSTHOG_PROJECT_API_KEY"], each.key) ? "SecureString" : "String"
  value = each.value
  
  tags = merge(var.common_tags, {
    Service   = var.service_name
    Component = "posthog-config"
  })
}

resource "google_secret_manager_secret" "service_posthog_config" {
  for_each = var.provider == "gcp" && var.enable_posthog ? local.service_posthog_config : {}
  
  secret_id = "nexpo-${var.service_name}-posthog-${replace(lower(each.key), "_", "-")}"
  
  replication {
    automatic = true
  }
  
  labels = merge(var.common_tags, {
    service   = var.service_name
    component = "posthog-config"
  })
}

resource "google_secret_manager_secret_version" "service_posthog_config" {
  for_each = var.provider == "gcp" && var.enable_posthog ? local.service_posthog_config : {}
  
  secret = google_secret_manager_secret.service_posthog_config[each.key].id
  secret_data = each.value
}

resource "azurerm_key_vault_secret" "service_posthog_config" {
  for_each = var.provider == "azure" && var.enable_posthog ? local.service_posthog_config : {}
  
  name         = "nexpo-${var.service_name}-posthog-${replace(lower(each.key), "_", "-")}"
  value        = each.value
  key_vault_id = var.azure_key_vault_id
  
  tags = merge(var.common_tags, {
    service   = var.service_name
    component = "posthog-config"
  })
}

# Service-specific Custom Events (defined by variables)
resource "posthog_action" "custom_events" {
  for_each = var.enable_posthog ? var.custom_events : {}
  
  project_id = posthog_project.service[0].id
  name       = each.value.name
  description = each.value.description
  
  steps {
    event = each.key
    
    dynamic "properties" {
      for_each = each.value.required_properties
      content {
        key   = properties.value
        value = "is_set"
      }
    }
  }
  
  tags = concat([var.service_name], each.value.tags)
}

# Cross-Service Integration
resource "posthog_project" "cross_service_link" {
  count = var.enable_posthog && var.parent_project_id != "" ? 1 : 0
  
  # Create a link to parent project for cross-service analytics
  name = "${var.project_name}-${var.service_name}-link"
  organization_id = var.posthog_organization_id
  
  # Reference parent project settings
  session_recording_opt_in = false
  capture_console_log_opt_in = false
  inject_web_apps = false
  
  tags = merge(var.common_tags, {
    Service = var.service_name
    Type    = "cross-service-link"
    Parent  = var.parent_project_id
  })
}
