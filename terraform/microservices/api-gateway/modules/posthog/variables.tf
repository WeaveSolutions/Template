# PostHog Integration Variables for API Gateway

variable "enable_posthog" {
  description = "Enable PostHog analytics integration"
  type        = bool
  default     = false
}

variable "project_name" {
  description = "Name of the project for PostHog configuration"
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
}

variable "posthog_organization_id" {
  description = "PostHog organization ID"
  type        = string
  default     = ""
}

variable "posthog_host" {
  description = "PostHog host URL (use https://app.posthog.com for cloud)"
  type        = string
  default     = "https://app.posthog.com"
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
  description = "Sampling rate for high-volume API analytics (0.0 to 1.0)"
  type        = number
  default     = 1.0
  
  validation {
    condition     = var.sampling_rate >= 0 && var.sampling_rate <= 1
    error_message = "Sampling rate must be between 0.0 and 1.0."
  }
}

# Feature Toggles
variable "enable_session_recording" {
  description = "Enable session recording (not applicable for API Gateway, but kept for consistency)"
  type        = bool
  default     = false
}

variable "enable_console_logs" {
  description = "Enable console log capture"
  type        = bool
  default     = true
}

variable "enable_web_injection" {
  description = "Enable PostHog web app injection"
  type        = bool
  default     = false
}

variable "enable_detailed_logging" {
  description = "Enable detailed API request logging for debugging"
  type        = bool
  default     = false
}

# Privacy and Compliance
variable "anonymize_ips" {
  description = "Anonymize IP addresses for GDPR compliance"
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
  description = "Number of events to batch before sending to PostHog"
  type        = number
  default     = 50
  
  validation {
    condition     = var.batch_size > 0 && var.batch_size <= 1000
    error_message = "Batch size must be between 1 and 1000."
  }
}

variable "flush_interval_ms" {
  description = "Interval in milliseconds to flush events to PostHog"
  type        = number
  default     = 5000
  
  validation {
    condition     = var.flush_interval_ms >= 1000
    error_message = "Flush interval must be at least 1000ms."
  }
}

# Webhook Configuration
variable "webhook_endpoint" {
  description = "Webhook endpoint for real-time PostHog events"
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

# API Gateway Specific
variable "enable_api_analytics" {
  description = "Enable detailed API request/response analytics"
  type        = bool
  default     = true
}

variable "track_user_agents" {
  description = "Track user agent strings for client analytics"
  type        = bool
  default     = true
}

variable "track_geolocation" {
  description = "Track user geolocation (respecting privacy settings)"
  type        = bool
  default     = false
}

variable "enable_error_tracking" {
  description = "Enable detailed error tracking and monitoring"
  type        = bool
  default     = true
}

variable "enable_performance_monitoring" {
  description = "Enable API performance monitoring (response times, etc.)"
  type        = bool
  default     = true
}

# Social Login Analytics
variable "track_social_login_providers" {
  description = "Track which social login providers are used"
  type        = bool
  default     = true
}

variable "social_login_conversion_tracking" {
  description = "Track conversion rates for different social login providers"
  type        = bool
  default     = true
}

# Business Metrics
variable "enable_business_metrics" {
  description = "Enable business-specific metric tracking"
  type        = bool
  default     = true
}

variable "track_subscription_events" {
  description = "Track subscription-related events (upgrades, downgrades, cancellations)"
  type        = bool
  default     = true
}

variable "enable_revenue_tracking" {
  description = "Enable revenue and monetization tracking"
  type        = bool
  default     = false  # Disabled by default for privacy
}
