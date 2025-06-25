# Shared PostHog Module Variables

variable "enable_posthog" {
  description = "Enable PostHog analytics integration"
  type        = bool
  default     = false
}

variable "service_name" {
  description = "Name of the microservice (auth, user, payments, etc.)"
  type        = string
  
  validation {
    condition = can(regex("^[a-z][a-z0-9-]*[a-z0-9]$", var.service_name))
    error_message = "Service name must be lowercase, start with a letter, and contain only letters, numbers, and hyphens."
  }
}

variable "project_name" {
  description = "Name of the main project"
  type        = string
  default     = "nexpo"
}

variable "environment" {
  description = "Environment (dev, staging, prod)"
  type        = string
  
  validation {
    condition     = contains(["dev", "staging", "prod"], var.environment)
    error_message = "Environment must be one of: dev, staging, prod."
  }
}

variable "provider" {
  description = "Cloud provider (aws, gcp, azure)"
  type        = string
  default     = "aws"
  
  validation {
    condition     = contains(["aws", "gcp", "azure"], var.provider)
    error_message = "Provider must be one of: aws, gcp, azure."
  }
}

# PostHog Configuration
variable "posthog_organization_id" {
  description = "PostHog organization ID"
  type        = string
  default     = ""
}

variable "parent_organization_id" {
  description = "Parent PostHog organization ID (fallback if posthog_organization_id not set)"
  type        = string
  default     = ""
}

variable "posthog_host" {
  description = "PostHog host URL"
  type        = string
  default     = "https://app.posthog.com"
}

variable "parent_project_id" {
  description = "Parent PostHog project ID for cross-service analytics"
  type        = string
  default     = ""
}

# Analytics Configuration
variable "analytics_rollout_percentage" {
  description = "Percentage of users to enable analytics for"
  type        = number
  default     = 100
  
  validation {
    condition     = var.analytics_rollout_percentage >= 0 && var.analytics_rollout_percentage <= 100
    error_message = "Analytics rollout percentage must be between 0 and 100."
  }
}

variable "sampling_rate" {
  description = "Sampling rate for analytics events (0.0 to 1.0)"
  type        = number
  default     = 1.0
  
  validation {
    condition     = var.sampling_rate >= 0 && var.sampling_rate <= 1
    error_message = "Sampling rate must be between 0.0 and 1.0."
  }
}

# Feature Toggles
variable "enable_session_recording" {
  description = "Enable session recording"
  type        = bool
  default     = false
}

variable "enable_console_logs" {
  description = "Enable console log capture"
  type        = bool
  default     = false
}

variable "enable_web_injection" {
  description = "Enable PostHog web app injection"
  type        = bool
  default     = false
}

variable "enable_pageview_capture" {
  description = "Enable automatic pageview capture"
  type        = bool
  default     = false
}

variable "enable_debug_logging" {
  description = "Enable debug logging for troubleshooting"
  type        = bool
  default     = false
}

variable "enable_performance_monitoring" {
  description = "Enable performance monitoring"
  type        = bool
  default     = true
}

variable "enable_error_tracking" {
  description = "Enable error tracking"
  type        = bool
  default     = true
}

# Privacy Settings
variable "anonymize_ips" {
  description = "Anonymize IP addresses for privacy compliance"
  type        = bool
  default     = true
}

variable "data_retention_days" {
  description = "Number of days to retain analytics data"
  type        = number
  default     = 365
  
  validation {
    condition     = var.data_retention_days > 0
    error_message = "Data retention days must be greater than 0."
  }
}

# Performance Settings
variable "batch_size" {
  description = "Number of events to batch before sending"
  type        = number
  default     = 50
  
  validation {
    condition     = var.batch_size > 0 && var.batch_size <= 1000
    error_message = "Batch size must be between 1 and 1000."
  }
}

variable "flush_interval_ms" {
  description = "Interval in milliseconds to flush events"
  type        = number
  default     = 5000
  
  validation {
    condition     = var.flush_interval_ms >= 1000
    error_message = "Flush interval must be at least 1000ms."
  }
}

# Webhook Configuration
variable "webhook_endpoint" {
  description = "Webhook endpoint for real-time events"
  type        = string
  default     = ""
}

variable "webhook_secret" {
  description = "Secret token for webhook security"
  type        = string
  default     = ""
  sensitive   = true
}

variable "additional_webhook_events" {
  description = "Additional events to send to webhook (beyond service_error)"
  type        = list(string)
  default     = []
}

# Custom Events Configuration
variable "custom_events" {
  description = "Custom events to track for this service"
  type = map(object({
    name                = string
    description         = string
    required_properties = list(string)
    tags               = list(string)
  }))
  default = {}
}

# Cloud Provider Specific
variable "azure_key_vault_id" {
  description = "Azure Key Vault ID for storing secrets (required if provider = azure)"
  type        = string
  default     = ""
}

# Tagging
variable "common_tags" {
  description = "Common tags to apply to all resources"
  type        = map(string)
  default     = {}
}

# Service-Specific Features
variable "enable_user_identification" {
  description = "Enable user identification with CRA root_id"
  type        = bool
  default     = true
}

variable "enable_cross_service_tracking" {
  description = "Enable tracking across multiple microservices"
  type        = bool
  default     = true
}

variable "enable_business_metrics" {
  description = "Enable business-specific metric tracking"
  type        = bool
  default     = false
}

# Integration Settings
variable "integrate_with_api_gateway" {
  description = "Integrate with API Gateway PostHog project"
  type        = bool
  default     = true
}

variable "share_user_cohorts" {
  description = "Share user cohorts with parent project"
  type        = bool
  default     = true
}

# Development Settings
variable "enable_development_features" {
  description = "Enable features useful for development (debug logs, detailed events)"
  type        = bool
  default     = false
}

variable "log_level" {
  description = "PostHog logging level (error, warn, info, debug)"
  type        = string
  default     = "info"
  
  validation {
    condition     = contains(["error", "warn", "info", "debug"], var.log_level)
    error_message = "Log level must be one of: error, warn, info, debug."
  }
}
