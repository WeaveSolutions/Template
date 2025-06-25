# User Service PostHog Module Variables

variable "enable_posthog" {
  description = "Enable PostHog analytics integration for user service"
  type        = bool
  default     = false
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

# Feature Toggles
variable "enable_session_recording" {
  description = "Enable session recording for user interactions"
  type        = bool
  default     = false
}

variable "enable_console_logs" {
  description = "Enable console log capture"
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

# User-Specific Feature Toggles
variable "enable_profile_analytics" {
  description = "Enable user profile analytics"
  type        = bool
  default     = true
}

variable "enable_engagement_tracking" {
  description = "Enable user engagement tracking"
  type        = bool
  default     = true
}

variable "enable_subscription_analytics" {
  description = "Enable subscription analytics"
  type        = bool
  default     = true
}

variable "enable_churn_prediction" {
  description = "Enable churn prediction analytics"
  type        = bool
  default     = true
}

variable "profile_analytics_rollout" {
  description = "Percentage rollout for profile analytics"
  type        = number
  default     = 100
  
  validation {
    condition     = var.profile_analytics_rollout >= 0 && var.profile_analytics_rollout <= 100
    error_message = "Profile analytics rollout must be between 0 and 100."
  }
}

# Privacy and GDPR Settings
variable "enable_gdpr_tracking" {
  description = "Enable GDPR compliance tracking"
  type        = bool
  default     = true
}

variable "enable_consent_tracking" {
  description = "Enable user consent tracking"
  type        = bool
  default     = true
}

# User Lifecycle Tracking
variable "enable_lifecycle_tracking" {
  description = "Enable user lifecycle tracking"
  type        = bool
  default     = true
}

variable "enable_engagement_scoring" {
  description = "Enable user engagement scoring"
  type        = bool
  default     = true
}

variable "enable_churn_risk_alerts" {
  description = "Enable churn risk alerts"
  type        = bool
  default     = true
}

# Feature Adoption Tracking
variable "track_feature_adoption" {
  description = "Track feature adoption rates"
  type        = bool
  default     = true
}

variable "track_onboarding_funnel" {
  description = "Track user onboarding funnel"
  type        = bool
  default     = true
}

# Business Metrics
variable "enable_business_metrics" {
  description = "Enable business-specific metric tracking"
  type        = bool
  default     = true
}

# Integration Settings
variable "integrate_with_api_gateway" {
  description = "Integrate with API Gateway PostHog project"
  type        = bool
  default     = true
}

variable "enable_identity_merge" {
  description = "Enable identity merging for user tracking"
  type        = bool
  default     = true
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

# Tagging
variable "common_tags" {
  description = "Common tags to apply to all resources"
  type        = map(string)
  default     = {}
}
