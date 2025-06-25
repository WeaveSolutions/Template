# PostHog Integration Variables for Payments Service

variable "enable_posthog" {
  description = "Enable PostHog analytics integration for payments"
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

# Revenue and Business Analytics
variable "enable_revenue_tracking" {
  description = "Enable revenue and monetization tracking"
  type        = bool
  default     = true
}

variable "revenue_tracking_rollout" {
  description = "Percentage of events to include in revenue tracking"
  type        = number
  default     = 100
  
  validation {
    condition     = var.revenue_tracking_rollout >= 0 && var.revenue_tracking_rollout <= 100
    error_message = "Revenue tracking rollout must be between 0 and 100."
  }
}

variable "enable_subscription_analytics" {
  description = "Enable detailed subscription lifecycle analytics"
  type        = bool
  default     = true
}

variable "enable_provider_analytics" {
  description = "Enable payment provider performance analytics"
  type        = bool
  default     = true
}

variable "enable_churn_prediction" {
  description = "Enable customer churn prediction analytics"
  type        = bool
  default     = true
}

variable "churn_prediction_rollout" {
  description = "Percentage of customers to include in churn prediction"
  type        = number
  default     = 100
  
  validation {
    condition     = var.churn_prediction_rollout >= 0 && var.churn_prediction_rollout <= 100
    error_message = "Churn prediction rollout must be between 0 and 100."
  }
}

# Financial Data Privacy
variable "anonymize_amounts" {
  description = "Anonymize payment amounts for privacy (recommended for GDPR)"
  type        = bool
  default     = false
}

variable "track_payment_methods" {
  description = "Track payment method types (card, bank, mobile, etc.)"
  type        = bool
  default     = true
}

variable "track_billing_countries" {
  description = "Track billing country for geographic revenue analysis"
  type        = bool
  default     = true
}

variable "enable_fraud_detection_analytics" {
  description = "Enable fraud detection and risk analytics"
  type        = bool
  default     = true
}

# Compliance and Retention
variable "data_retention_days" {
  description = "Number of days to retain payment analytics data (minimum 2555 days for PCI compliance)"
  type        = number
  default     = 2555  # 7 years for financial records
  
  validation {
    condition     = var.data_retention_days >= 2555
    error_message = "Financial data must be retained for at least 7 years (2555 days) for compliance."
  }
}

variable "enable_pci_compliance_mode" {
  description = "Enable PCI compliance mode with enhanced security"
  type        = bool
  default     = true
}

# Performance Settings
variable "sampling_rate" {
  description = "Sampling rate for payment analytics (0.0 to 1.0)"
  type        = number
  default     = 1.0
  
  validation {
    condition     = var.sampling_rate >= 0 && var.sampling_rate <= 1
    error_message = "Sampling rate must be between 0.0 and 1.0."
  }
}

variable "batch_size" {
  description = "Number of events to batch before sending to PostHog"
  type        = number
  default     = 25  # Smaller batches for financial data
  
  validation {
    condition     = var.batch_size > 0 && var.batch_size <= 100
    error_message = "Batch size must be between 1 and 100 for payments."
  }
}

variable "flush_interval_ms" {
  description = "Interval in milliseconds to flush events to PostHog"
  type        = number
  default     = 2000  # Faster flush for financial events
  
  validation {
    condition     = var.flush_interval_ms >= 1000
    error_message = "Flush interval must be at least 1000ms."
  }
}

# Feature Toggles
variable "enable_console_logs" {
  description = "Enable console log capture (disabled in production for security)"
  type        = bool
  default     = false
}

# Webhook Configuration
variable "webhook_endpoint" {
  description = "Webhook endpoint for critical payment events"
  type        = string
  default     = ""
}

variable "webhook_secret" {
  description = "Secret token for webhook security"
  type        = string
  default     = ""
  sensitive   = true
}

# Subscription Analytics
variable "track_plan_changes" {
  description = "Track subscription plan upgrades and downgrades"
  type        = bool
  default     = true
}

variable "track_billing_cycles" {
  description = "Track billing cycle events and renewals"
  type        = bool
  default     = true
}

variable "track_proration_events" {
  description = "Track proration calculations for plan changes"
  type        = bool
  default     = true
}

variable "enable_mrr_analytics" {
  description = "Enable Monthly Recurring Revenue (MRR) analytics"
  type        = bool
  default     = true
}

variable "enable_ltv_analytics" {
  description = "Enable Customer Lifetime Value (LTV) analytics"
  type        = bool
  default     = true
}

# Payment Provider Specific
variable "stripe_analytics_enabled" {
  description = "Enable Stripe-specific analytics"
  type        = bool
  default     = true
}

variable "qonversion_analytics_enabled" {
  description = "Enable Qonversion (mobile IAP) analytics"
  type        = bool
  default     = true
}

variable "polar_analytics_enabled" {
  description = "Enable Polar subscription analytics"
  type        = bool
  default     = true
}

# Mobile IAP Analytics
variable "enable_app_store_analytics" {
  description = "Enable Apple App Store transaction analytics"
  type        = bool
  default     = true
}

variable "enable_play_store_analytics" {
  description = "Enable Google Play Store transaction analytics"
  type        = bool
  default     = true
}

variable "track_iap_receipt_validation" {
  description = "Track in-app purchase receipt validation events"
  type        = bool
  default     = true
}

# Business Intelligence
variable "enable_cohort_revenue_analysis" {
  description = "Enable revenue analysis by customer cohorts"
  type        = bool
  default     = true
}

variable "enable_geographic_revenue_analysis" {
  description = "Enable revenue analysis by geographic regions"
  type        = bool
  default     = true
}

variable "enable_seasonal_analytics" {
  description = "Enable seasonal revenue pattern analytics"
  type        = bool
  default     = true
}

# Tagging
variable "common_tags" {
  description = "Common tags to apply to all resources"
  type        = map(string)
  default     = {}
}

# Integration Settings
variable "integrate_with_api_gateway" {
  description = "Integrate with API Gateway PostHog project"
  type        = bool
  default     = true
}

variable "api_gateway_posthog_project_id" {
  description = "API Gateway PostHog project ID for cross-service analytics"
  type        = string
  default     = ""
}

# Alert Configuration
variable "enable_payment_failure_alerts" {
  description = "Enable real-time alerts for payment failures"
  type        = bool
  default     = true
}

variable "enable_churn_alerts" {
  description = "Enable alerts for high-risk churn events"
  type        = bool
  default     = true
}

variable "enable_revenue_anomaly_detection" {
  description = "Enable anomaly detection for revenue patterns"
  type        = bool
  default     = true
}
