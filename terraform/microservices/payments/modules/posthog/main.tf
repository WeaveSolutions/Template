# PostHog Analytics Integration for Payments Service
# This module configures PostHog analytics for tracking subscription events,
# payment processing, revenue metrics, and billing analytics

terraform {
  required_providers {
    posthog = {
      source  = "PostHog/posthog"
      version = "~> 0.1"
    }
  }
}

# PostHog Project Configuration for Payments
resource "posthog_project" "payments" {
  count = var.enable_posthog ? 1 : 0
  
  name = "${var.project_name}-payments"
  organization_id = var.posthog_organization_id
  
  # Configure project settings
  session_recording_opt_in = false  # Not applicable for payments service
  capture_console_log_opt_in = var.enable_console_logs
  inject_web_apps = false
  
  # Data retention settings (longer for financial data)
  data_attributes = var.data_retention_days
  
  # Enhanced privacy for financial data
  anonymize_ips = true  # Always true for payments
  
  tags = merge(var.common_tags, {
    Service     = "payments"
    Analytics   = "posthog"
    Environment = var.environment
    DataClass   = "financial"
  })
}

# Feature Flags for Payments Analytics
resource "posthog_feature_flag" "revenue_tracking" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = posthog_project.payments[0].id
  key        = "revenue-tracking-enabled"
  name       = "Revenue and Monetization Tracking"
  
  filters {
    groups {
      properties {
        key   = "environment"
        value = var.environment
        type  = "person"
      }
      rollout_percentage = var.revenue_tracking_rollout
    }
  }
  
  active = var.enable_revenue_tracking
}

resource "posthog_feature_flag" "subscription_analytics" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = posthog_project.payments[0].id
  key        = "subscription-analytics-enabled"
  name       = "Subscription Lifecycle Analytics"
  
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
  
  active = var.enable_subscription_analytics
}

resource "posthog_feature_flag" "payment_provider_analytics" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = posthog_project.payments[0].id
  key        = "payment-provider-analytics"
  name       = "Payment Provider Performance Analytics"
  
  filters {
    groups {
      properties {
        key   = "user_role"
        value = ["admin", "finance", "analytics"]
        type  = "person"
      }
      rollout_percentage = 100
    }
  }
  
  active = var.enable_provider_analytics
}

resource "posthog_feature_flag" "churn_prediction" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = posthog_project.payments[0].id
  key        = "churn-prediction-enabled"
  name       = "Customer Churn Prediction Analytics"
  
  filters {
    groups {
      properties {
        key   = "environment"
        value = "prod"
        type  = "person"
      }
      rollout_percentage = var.churn_prediction_rollout
    }
  }
  
  active = var.enable_churn_prediction
}

# Payment Analytics Events
resource "posthog_action" "subscription_created" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = posthog_project.payments[0].id
  name       = "Subscription Created"
  description = "Track new subscription creation events"
  
  steps {
    event = "subscription_created"
    properties {
      key   = "plan_id"
      value = "is_set"
    }
    properties {
      key   = "payment_provider"
      value = "is_set"
    }
    properties {
      key   = "amount"
      value = "is_set"
    }
  }
  
  tags = ["subscription", "revenue", "conversion"]
}

resource "posthog_action" "payment_completed" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = posthog_project.payments[0].id
  name       = "Payment Completed"
  description = "Track successful payment transactions"
  
  steps {
    event = "payment_completed"
    properties {
      key   = "amount"
      value = "is_set"
    }
    properties {
      key   = "currency"
      value = "is_set"
    }
    properties {
      key   = "payment_method"
      value = "is_set"
    }
  }
  
  tags = ["payment", "revenue", "success"]
}

resource "posthog_action" "payment_failed" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = posthog_project.payments[0].id
  name       = "Payment Failed"
  description = "Track failed payment attempts for analysis"
  
  steps {
    event = "payment_failed"
    properties {
      key   = "failure_reason"
      value = "is_set"
    }
    properties {
      key   = "payment_provider"
      value = "is_set"
    }
    properties {
      key   = "retry_count"
      value = "is_set"
    }
  }
  
  tags = ["payment", "failure", "analysis"]
}

resource "posthog_action" "subscription_cancelled" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = posthog_project.payments[0].id
  name       = "Subscription Cancelled"
  description = "Track subscription cancellation events"
  
  steps {
    event = "subscription_cancelled"
    properties {
      key   = "cancellation_reason"
      value = "is_set"
    }
    properties {
      key   = "plan_id"
      value = "is_set"
    }
    properties {
      key   = "tenure_days"
      value = "is_set"
    }
  }
  
  tags = ["subscription", "churn", "cancellation"]
}

resource "posthog_action" "refund_processed" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = posthog_project.payments[0].id
  name       = "Refund Processed"
  description = "Track refund transactions"
  
  steps {
    event = "refund_processed"
    properties {
      key   = "refund_amount"
      value = "is_set"
    }
    properties {
      key   = "refund_reason"
      value = "is_set"
    }
    properties {
      key   = "original_payment_id"
      value = "is_set"
    }
  }
  
  tags = ["refund", "revenue", "customer_service"]
}

# Revenue Analytics Dashboard
resource "posthog_dashboard" "payments_analytics" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = posthog_project.payments[0].id
  name       = "Payments & Revenue Analytics"
  description = "Comprehensive dashboard for payment and subscription analytics"
  
  tags = ["payments", "revenue", "subscriptions", "analytics"]
}

# Customer Cohorts for Revenue Analysis
resource "posthog_cohort" "high_value_customers" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = posthog_project.payments[0].id
  name       = "High Value Customers"
  description = "Customers with high lifetime value or enterprise plans"
  
  filters {
    properties {
      key      = "subscription_plan"
      value    = ["enterprise", "premium"]
      operator = "in"
      type     = "person"
    }
  }
  
  is_static = false
}

resource "posthog_cohort" "at_risk_customers" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = posthog_project.payments[0].id
  name       = "At-Risk Customers"
  description = "Customers with payment failures or engagement drops"
  
  filters {
    properties {
      key      = "payment_failures_count"
      value    = 2
      operator = "gte"
      type     = "person"
    }
  }
  
  is_static = false
}

resource "posthog_cohort" "new_subscribers" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = posthog_project.payments[0].id
  name       = "New Subscribers"
  description = "Users who subscribed in the last 30 days"
  
  filters {
    properties {
      key      = "first_subscription_date"
      value    = 30
      operator = "relative_date_range"
      type     = "person"
    }
  }
  
  is_static = false
}

resource "posthog_cohort" "churned_customers" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = posthog_project.payments[0].id
  name       = "Churned Customers"
  description = "Customers who cancelled their subscriptions"
  
  filters {
    properties {
      key      = "subscription_status"
      value    = "cancelled"
      operator = "exact"
      type     = "person"
    }
  }
  
  is_static = false
}

# Webhook for Critical Payment Events
resource "posthog_webhook" "payment_events" {
  count = var.enable_posthog && var.webhook_endpoint != "" ? 1 : 0
  
  project_id = posthog_project.payments[0].id
  target     = var.webhook_endpoint
  
  # Critical events for immediate action
  event_names = [
    "payment_failed",
    "subscription_cancelled",
    "refund_processed",
    "fraud_detected"
  ]
  
  # Security
  secret_token = var.webhook_secret
  
  # Enable in production for critical payment events
  enabled = var.environment == "production"
}

# Environment Variables for Payments Service
locals {
  payments_posthog_config = var.enable_posthog ? {
    POSTHOG_PROJECT_API_KEY = posthog_project.payments[0].api_token
    POSTHOG_HOST           = var.posthog_host
    POSTHOG_PROJECT_ID     = posthog_project.payments[0].id
    
    # Feature flags
    POSTHOG_FEATURE_FLAGS_ENABLED = "true"
    
    # Payments-specific settings
    POSTHOG_REVENUE_TRACKING      = tostring(var.enable_revenue_tracking)
    POSTHOG_SUBSCRIPTION_ANALYTICS = tostring(var.enable_subscription_analytics)
    POSTHOG_PROVIDER_ANALYTICS    = tostring(var.enable_provider_analytics)
    POSTHOG_CHURN_PREDICTION      = tostring(var.enable_churn_prediction)
    
    # Privacy (always enforced for financial data)
    POSTHOG_ANONYMIZE_IPS         = "true"
    POSTHOG_ANONYMIZE_AMOUNTS     = tostring(var.anonymize_amounts)
    
    # Performance settings
    POSTHOG_SAMPLING_RATE  = tostring(var.sampling_rate)
    POSTHOG_BATCH_SIZE     = tostring(var.batch_size)
    POSTHOG_FLUSH_INTERVAL = tostring(var.flush_interval_ms)
    
    # Data retention for financial compliance
    POSTHOG_DATA_RETENTION_DAYS = tostring(var.data_retention_days)
    
  } : {}
}

# Store secure configuration
resource "aws_ssm_parameter" "payments_posthog_config" {
  for_each = var.provider == "aws" && var.enable_posthog ? local.payments_posthog_config : {}
  
  name  = "/nexpo/payments/posthog/${each.key}"
  type  = contains(["POSTHOG_PROJECT_API_KEY"], each.key) ? "SecureString" : "String"
  value = each.value
  
  tags = merge(var.common_tags, {
    Service   = "payments"
    Component = "posthog-config"
    DataClass = "financial"
  })
}
