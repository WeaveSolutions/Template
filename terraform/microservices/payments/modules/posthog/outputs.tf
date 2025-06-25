# Payments Service PostHog Module Outputs

output "posthog_project_id" {
  description = "PostHog project ID for the payments service"
  value       = var.enable_posthog ? posthog_project.payments[0].id : null
}

output "posthog_project_api_key" {
  description = "PostHog project API key for payments service"
  value       = var.enable_posthog ? posthog_project.payments[0].api_token : null
  sensitive   = true
}

output "posthog_host" {
  description = "PostHog host URL"
  value       = var.posthog_host
}

output "service_name" {
  description = "Name of the service (payments)"
  value       = "payments"
}

# Payments-Specific Feature Flags
output "payments_feature_flags" {
  description = "Payments-specific feature flags"
  value = var.enable_posthog ? {
    revenue_tracking          = posthog_feature_flag.revenue_tracking[0].key
    subscription_analytics    = posthog_feature_flag.subscription_analytics[0].key
    churn_prediction         = posthog_feature_flag.churn_prediction[0].key
    payment_provider_analytics = posthog_feature_flag.payment_provider_analytics[0].key
    fraud_detection          = posthog_feature_flag.fraud_detection[0].key
  } : {}
}

# Analytics Actions
output "payments_analytics_actions" {
  description = "Payments service analytics actions"
  value = var.enable_posthog ? {
    subscription_created     = posthog_action.subscription_created[0].name
    subscription_cancelled   = posthog_action.subscription_cancelled[0].name
    payment_completed       = posthog_action.payment_completed[0].name
    payment_failed          = posthog_action.payment_failed[0].name
    refund_processed        = posthog_action.refund_processed[0].name
    churn_risk_identified   = posthog_action.churn_risk_identified[0].name
  } : {}
}

# Payments Cohorts
output "payments_cohorts" {
  description = "Payments service user cohorts"
  value = var.enable_posthog ? {
    high_value_customers = posthog_cohort.high_value_customers[0].name
    at_risk_customers   = posthog_cohort.at_risk_customers[0].name
    new_subscribers     = posthog_cohort.new_subscribers[0].name
    churned_customers   = posthog_cohort.churned_customers[0].name
  } : {}
}

# Dashboard
output "dashboard_url" {
  description = "URL to the payments analytics dashboard"
  value = var.enable_posthog ? "${var.posthog_host}/project/${posthog_project.payments[0].id}/dashboard/${posthog_dashboard.payments_analytics[0].id}" : null
}

# Environment Variables for Payments Service
output "environment_variables" {
  description = "Environment variables for payments service PostHog integration"
  value = var.enable_posthog ? merge(
    {
      POSTHOG_PROJECT_API_KEY = posthog_project.payments[0].api_token
      POSTHOG_HOST           = var.posthog_host
      POSTHOG_PROJECT_ID     = posthog_project.payments[0].id
      POSTHOG_SERVICE_NAME   = "payments"
      
      # Feature flags
      POSTHOG_FEATURE_FLAGS_ENABLED = "true"
      
      # Payments-specific settings
      POSTHOG_REVENUE_TRACKING          = tostring(var.enable_revenue_tracking)
      POSTHOG_SUBSCRIPTION_ANALYTICS    = tostring(var.enable_subscription_analytics)
      POSTHOG_CHURN_PREDICTION         = tostring(var.enable_churn_prediction)
      POSTHOG_PAYMENT_PROVIDER_ANALYTICS = tostring(var.enable_payment_provider_analytics)
      POSTHOG_FRAUD_DETECTION          = tostring(var.enable_fraud_detection)
      
      # Privacy and compliance
      POSTHOG_ANONYMIZE_IPS    = "true"  # Always true for financial data
      POSTHOG_PCI_COMPLIANCE   = tostring(var.enable_pci_compliance)
      POSTHOG_GDPR_TRACKING    = tostring(var.enable_gdpr_tracking)
      
      # Performance settings
      POSTHOG_SAMPLING_RATE    = tostring(var.sampling_rate)
      POSTHOG_BATCH_SIZE       = tostring(var.batch_size)
      POSTHOG_FLUSH_INTERVAL   = tostring(var.flush_interval_ms)
      
      # Business intelligence
      POSTHOG_TRACK_SUBSCRIPTION_LIFECYCLE = tostring(var.track_subscription_lifecycle)
      POSTHOG_TRACK_PAYMENT_METHODS        = tostring(var.track_payment_methods)
      POSTHOG_TRACK_REVENUE_METRICS        = tostring(var.track_revenue_metrics)
      POSTHOG_TRACK_CHURN_INDICATORS       = tostring(var.track_churn_indicators)
      
      # Integration
      POSTHOG_PARENT_PROJECT_ID = var.parent_project_id
      POSTHOG_STRIPE_INTEGRATION = tostring(var.enable_stripe_integration)
      POSTHOG_PAYPAL_INTEGRATION = tostring(var.enable_paypal_integration)
      
      # Data retention for financial compliance
      POSTHOG_DATA_RETENTION_DAYS = tostring(var.data_retention_days)
      
    },
    local.payments_specific_config
  ) : {}
  
  sensitive = true
}

# Payments Event Schema
output "payments_event_schema" {
  description = "Event schema for payments service"
  value = {
    subscription_created = {
      event_name = "subscription_created"
      properties = {
        user_id           = "string"  # CRA root_id
        subscription_id   = "string"
        plan_id          = "string"
        plan_name        = "string"
        billing_cycle    = "string"  # monthly, yearly
        amount           = "number"  # in cents
        currency         = "string"
        payment_method   = "string"  # card, paypal, bank_transfer
        trial_period_days = "number"
        discount_applied = "number"
        tax_amount      = "number"
        total_amount    = "number"
        payment_provider = "string"  # stripe, paypal, etc.
        customer_type   = "string"   # new, existing, reactivated
      }
    }
    
    subscription_cancelled = {
      event_name = "subscription_cancelled"
      properties = {
        user_id          = "string"
        subscription_id  = "string"
        plan_id         = "string"
        cancellation_reason = "string"
        cancellation_source = "string"  # user, admin, failed_payment
        effective_date  = "date"
        refund_amount   = "number"
        subscription_duration_days = "number"
        ltv_at_cancellation = "number"
      }
    }
    
    payment_completed = {
      event_name = "payment_completed"
      properties = {
        user_id          = "string"
        payment_id       = "string"
        subscription_id  = "string"
        amount          = "number"
        currency        = "string"
        payment_method  = "string"
        payment_provider = "string"
        transaction_fee = "number"
        processing_time_ms = "number"
        payment_type    = "string"  # subscription, one_time, upgrade
        invoice_id      = "string"
      }
    }
    
    payment_failed = {
      event_name = "payment_failed"
      properties = {
        user_id          = "string"
        payment_id       = "string"
        subscription_id  = "string"
        amount          = "number"
        currency        = "string"
        payment_method  = "string"
        payment_provider = "string"
        failure_reason  = "string"
        failure_code    = "string"
        retry_attempt   = "number"
        next_retry_date = "date"
      }
    }
    
    refund_processed = {
      event_name = "refund_processed"
      properties = {
        user_id         = "string"
        refund_id       = "string"
        payment_id      = "string"
        subscription_id = "string"
        refund_amount   = "number"
        refund_reason   = "string"
        refund_type     = "string"  # full, partial
        processing_fee  = "number"
        initiated_by    = "string"  # user, admin, automated
      }
    }
    
    churn_prediction_event = {
      event_name = "churn_prediction_event"
      properties = {
        user_id           = "string"
        churn_risk_score  = "number"  # 0-100
        risk_level        = "string"  # low, medium, high
        contributing_factors = "array"
        prediction_confidence = "number"
        recommended_actions = "array"
        subscription_id   = "string"
        days_to_predicted_churn = "number"
      }
    }
    
    revenue_milestone = {
      event_name = "revenue_milestone"
      properties = {
        milestone_type   = "string"  # monthly_revenue, annual_revenue, customer_ltv
        milestone_value  = "number"
        achievement_date = "date"
        contributing_customers = "number"
        growth_rate     = "number"
        previous_milestone = "number"
      }
    }
    
    payment_provider_event = {
      event_name = "payment_provider_event"
      properties = {
        provider_name    = "string"
        event_type      = "string"  # performance, outage, rate_change
        success_rate    = "number"
        average_processing_time = "number"
        cost_per_transaction = "number"
        volume_processed = "number"
        error_rate      = "number"
      }
    }
  }
}

# Integration Configuration
output "payments_integration_config" {
  description = "Configuration for integrating payments service with PostHog"
  value = var.enable_posthog ? {
    # SDK Configuration for Payments Service
    sdk_config = {
      api_key = posthog_project.payments[0].api_token
      host    = var.posthog_host
      
      # Payments-specific options
      capture_pageview    = false  # Backend service
      session_recording   = false  # Not applicable for payments
      capture_console_logs = var.enable_console_logs
      anonymize_ips       = true   # Always true for financial data
      
      # Performance and compliance
      sampling_rate       = var.sampling_rate
      batch_size         = var.batch_size
      flush_interval     = var.flush_interval_ms
      data_retention_days = var.data_retention_days
    }
    
    # Payments-specific Feature Flags
    feature_flags = {
      revenue_tracking          = "revenue-tracking"
      subscription_analytics    = "subscription-analytics"
      churn_prediction         = "churn-prediction"
      payment_provider_analytics = "payment-provider-analytics"
      fraud_detection          = "fraud-detection"
    }
    
    # Payment Events
    events = {
      subscription_created    = "subscription_created"
      subscription_cancelled  = "subscription_cancelled"
      payment_completed      = "payment_completed"
      payment_failed         = "payment_failed"
      refund_processed       = "refund_processed"
      churn_prediction       = "churn_prediction_event"
      revenue_milestone      = "revenue_milestone"
      provider_event         = "payment_provider_event"
    }
    
    # Financial User Properties
    user_properties = {
      user_id              = "root_id"              # From CRA
      customer_type        = "customer_type"        # new, existing, reactivated
      subscription_plan    = "subscription.plan_id"
      billing_cycle        = "subscription.billing_cycle"
      payment_method       = "primary_payment_method"
      lifetime_value       = "calculated_ltv"
      total_spent          = "total_amount_spent"
      subscription_start   = "subscription_start_date"
      last_payment_date    = "last_successful_payment"
      payment_failures     = "failed_payment_count"
      churn_risk_score     = "churn_risk_score"
      avg_monthly_spend    = "average_monthly_spend"
      subscription_status  = "subscription_status"  # active, cancelled, past_due
    }
    
    # Revenue Segments
    revenue_segments = {
      high_value    = "lifetime_value >= 1000"
      medium_value  = "lifetime_value >= 100 AND lifetime_value < 1000"
      low_value     = "lifetime_value < 100"
      at_risk       = "churn_risk_score >= 70"
      new_customer  = "subscription_start_date within last 30 days"
      loyal_customer = "subscription_duration_months >= 12"
    }
    
  } : null
  
  sensitive = true
}

# Business Intelligence Outputs
output "business_intelligence_config" {
  description = "Business intelligence configuration for payments analytics"
  value = var.enable_posthog ? {
    # Key Revenue Metrics
    revenue_metrics = {
      monthly_recurring_revenue = "sum of active subscription amounts"
      annual_recurring_revenue  = "MRR * 12"
      average_revenue_per_user  = "total_revenue / active_users"
      customer_lifetime_value   = "average_revenue_per_user / churn_rate"
      churn_rate               = "cancelled_subscriptions / total_subscriptions"
      revenue_growth_rate      = "(current_month_revenue - last_month_revenue) / last_month_revenue"
    }
    
    # Subscription Metrics
    subscription_metrics = {
      subscription_conversion_rate = "paid_subscriptions / trial_users"
      upgrade_rate                = "upgrades / total_subscriptions"
      downgrade_rate              = "downgrades / total_subscriptions"
      reactivation_rate          = "reactivated_users / churned_users"
      trial_conversion_rate      = "trial_to_paid / trial_starts"
    }
    
    # Payment Performance
    payment_performance = {
      payment_success_rate    = "successful_payments / total_payment_attempts"
      average_processing_time = "avg(payment_processing_time)"
      failed_payment_rate     = "failed_payments / total_payment_attempts"
      refund_rate            = "refunds / successful_payments"
      transaction_cost_ratio = "total_fees / total_revenue"
    }
    
    # Cohort Analysis
    cohort_definitions = {
      monthly_cohorts   = "group users by signup month"
      revenue_cohorts   = "group users by lifetime value brackets"
      behavior_cohorts  = "group users by engagement patterns"
      churn_cohorts     = "group users by churn risk score"
    }
    
  } : null
}

# AWS SSM Parameters for Payments Service
output "aws_ssm_parameters" {
  description = "AWS SSM parameter names for payments PostHog configuration"
  value = var.provider == "aws" && var.enable_posthog ? {
    for key, value in local.payments_specific_config : key => "/nexpo/payments/posthog/${key}"
  } : {}
}

# Cross-Service Integration
output "cross_service_config" {
  description = "Configuration for cross-service integration"
  value = var.enable_posthog ? {
    project_id      = posthog_project.payments[0].id
    parent_project  = var.parent_project_id
    service_name    = "payments"
    
    # Shared financial properties that other services can use
    shared_user_properties = {
      root_id              = "string"  # Primary user identifier
      subscription_plan    = "string"
      subscription_status  = "string"
      lifetime_value       = "number"
      churn_risk_score     = "number"
      payment_method       = "string"
      billing_cycle        = "string"
      last_payment_date    = "date"
    }
    
    # Events that other services should be aware of
    shareable_events = [
      "subscription_created",
      "subscription_cancelled",
      "payment_completed",
      "payment_failed",
      "churn_prediction_event"
    ]
    
    # Revenue cohorts that can be used by other services
    shareable_cohorts = [
      "high_value_customers",
      "at_risk_customers",
      "new_subscribers"
    ]
    
  } : null
}

# Webhook Status
output "webhook_configured" {
  description = "Whether webhook is configured for payments service"
  value       = var.enable_posthog && var.webhook_endpoint != "" ? true : false
}

# Terraform State Information
output "terraform_state" {
  description = "Terraform state information for payments PostHog resources"
  value = var.enable_posthog ? {
    service_name              = "payments"
    project_created           = true
    payments_feature_flags_count = 5
    payments_cohorts_count    = 4
    payments_actions_count    = 6
    dashboard_created         = true
    webhook_enabled           = var.webhook_endpoint != ""
    custom_events_count       = 8
    pci_compliance_enabled    = var.enable_pci_compliance
    gdpr_compliance_enabled   = var.enable_gdpr_tracking
    data_retention_days       = var.data_retention_days
  } : {
    service_name = "payments"
    project_created = false
  }
}
