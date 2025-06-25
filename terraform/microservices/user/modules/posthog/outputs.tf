# User Service PostHog Module Outputs

# Base Module Outputs
output "posthog_project_id" {
  description = "PostHog project ID for the user service"
  value       = var.enable_posthog ? module.user_posthog_base.posthog_project_id : null
}

output "posthog_project_api_key" {
  description = "PostHog project API key for user service"
  value       = var.enable_posthog ? module.user_posthog_base.posthog_project_api_key : null
  sensitive   = true
}

output "posthog_host" {
  description = "PostHog host URL"
  value       = var.posthog_host
}

output "service_name" {
  description = "Name of the service (user)"
  value       = "user"
}

# User-Specific Feature Flags
output "user_feature_flags" {
  description = "User-specific feature flags"
  value = var.enable_posthog ? merge(
    module.user_posthog_base.feature_flags,
    {
      profile_analytics      = posthog_feature_flag.user_profile_analytics[0].key
      engagement_tracking    = posthog_feature_flag.user_engagement_tracking[0].key
      subscription_analytics = posthog_feature_flag.subscription_analytics[0].key
      churn_prediction      = posthog_feature_flag.churn_prediction[0].key
    }
  ) : {}
}

# Analytics Actions
output "user_analytics_actions" {
  description = "User service analytics actions"
  value = var.enable_posthog ? merge(
    module.user_posthog_base.analytics_actions,
    {
      profile_completion  = posthog_action.profile_completion[0].name
      subscription_upgrade = posthog_action.subscription_upgrade[0].name
      feature_adoption    = posthog_action.feature_adoption[0].name
    }
  ) : {}
}

# User Cohorts
output "user_cohorts" {
  description = "User service user cohorts"
  value = var.enable_posthog ? merge(
    module.user_posthog_base.cohorts,
    {
      new_users     = posthog_cohort.new_users[0].name
      active_users  = posthog_cohort.active_users[0].name
      premium_users = posthog_cohort.premium_users[0].name
      at_risk_users = posthog_cohort.at_risk_users[0].name
      power_users   = posthog_cohort.power_users[0].name
    }
  ) : {}
}

# Dashboard
output "dashboard_url" {
  description = "URL to the user analytics dashboard"
  value = var.enable_posthog ? "${var.posthog_host}/project/${module.user_posthog_base.posthog_project_id}/dashboard/${posthog_dashboard.user_analytics[0].id}" : null
}

# Environment Variables for User Service
output "environment_variables" {
  description = "Environment variables for user service PostHog integration"
  value = var.enable_posthog ? merge(
    module.user_posthog_base.environment_variables,
    local.user_specific_config
  ) : {}
  
  sensitive = true
}

# User Event Schema
output "user_event_schema" {
  description = "Event schema for user service"
  value = {
    user_profile_created = {
      event_name = "user_profile_created"
      properties = {
        user_id      = "string"  # CRA root_id
        profile_type = "string"  # basic, premium, enterprise
        source      = "string"   # signup, admin, import
        fields_completed = "array"
        completion_percentage = "number"
        referrer    = "string"
        utm_source  = "string"
        utm_medium  = "string"
        utm_campaign = "string"
      }
    }
    
    user_profile_updated = {
      event_name = "user_profile_updated"
      properties = {
        user_id         = "string"
        fields_updated  = "array"   # list of field names updated
        update_source   = "string"  # user, admin, api, sync
        previous_values = "object"  # for tracking changes
        new_values     = "object"
        update_reason  = "string"   # manual, automated, compliance
      }
    }
    
    user_preferences_changed = {
      event_name = "user_preferences_changed"
      properties = {
        user_id         = "string"
        preference_type = "string"  # notification, privacy, ui, language
        preference_key  = "string"
        new_value      = "string"
        previous_value = "string"
        source         = "string"   # settings_page, onboarding, api
      }
    }
    
    user_subscription_event = {
      event_name = "user_subscription_event"
      properties = {
        user_id          = "string"
        event_type       = "string"  # created, upgraded, downgraded, cancelled, renewed
        subscription_plan = "string"  # free, basic, premium, enterprise
        previous_plan    = "string"
        billing_cycle    = "string"  # monthly, yearly
        amount          = "number"
        currency        = "string"
        payment_method  = "string"
      }
    }
    
    user_engagement_milestone = {
      event_name = "user_engagement_milestone"
      properties = {
        user_id          = "string"
        milestone_type   = "string"  # days_active, features_used, data_uploaded
        milestone_value  = "number"
        achievement_date = "date"
        engagement_score = "number"
        previous_score   = "number"
      }
    }
    
    user_data_export = {
      event_name = "user_data_export"
      properties = {
        user_id        = "string"
        export_type    = "string"  # full, partial, specific_service
        request_source = "string"  # user_request, admin, automated
        data_types     = "array"   # profile, activity, files, etc.
        file_format    = "string"  # json, csv, pdf
        file_size_mb   = "number"
      }
    }
    
    user_data_deletion = {
      event_name = "user_data_deletion"
      properties = {
        user_id         = "string"
        deletion_type   = "string"  # soft_delete, hard_delete, anonymize
        request_source  = "string"  # user_request, admin, automated, legal
        data_types      = "array"
        retention_period = "number" # days before permanent deletion
        deletion_reason = "string"
      }
    }
    
    user_consent_event = {
      event_name = "user_consent_event"
      properties = {
        user_id        = "string"
        consent_type   = "string"  # marketing, analytics, cookies, data_processing
        consent_granted = "boolean"
        consent_version = "string"  # version of consent form
        consent_method = "string"   # explicit, implied, updated
        privacy_policy_version = "string"
      }
    }
  }
}

# Integration Configuration
output "user_integration_config" {
  description = "Configuration for integrating user service with PostHog"
  value = var.enable_posthog ? {
    # SDK Configuration for User Service
    sdk_config = {
      api_key = module.user_posthog_base.posthog_project_api_key
      host    = var.posthog_host
      
      # User-specific options
      capture_pageview    = false  # Backend service
      session_recording   = var.enable_session_recording
      capture_console_logs = var.enable_console_logs
      anonymize_ips       = var.anonymize_ips
      
      # Performance
      sampling_rate    = var.sampling_rate
      batch_size      = var.batch_size
      flush_interval  = var.flush_interval_ms
    }
    
    # User-specific Feature Flags
    feature_flags = {
      profile_analytics      = "user-profile-analytics"
      engagement_tracking    = "user-engagement-tracking"
      subscription_analytics = "subscription-analytics"
      churn_prediction      = "churn-prediction"
    }
    
    # User Events
    events = {
      profile_created      = "user_profile_created"
      profile_updated      = "user_profile_updated"
      preferences_changed  = "user_preferences_changed"
      subscription_event   = "user_subscription_event"
      engagement_milestone = "user_engagement_milestone"
      data_export         = "user_data_export"
      data_deletion       = "user_data_deletion"
      consent_event       = "user_consent_event"
    }
    
    # User Properties for User Service Context
    user_properties = {
      user_id              = "root_id"              # From CRA
      profile_completion   = "profile_completion_percentage"
      subscription_plan    = "subscription.plan_id"
      account_type        = "account_type"          # personal, business
      engagement_score    = "engagement_score"
      last_activity_at    = "last_activity_at"
      signup_date         = "identity.created_at"
      profile_verified    = "profile_verified"
      gdpr_consent        = "gdpr_consent_granted"
      marketing_consent   = "marketing_consent_granted"
      data_processing_consent = "data_processing_consent"
      churn_risk_score    = "churn_risk_score"
      lifetime_value      = "calculated_ltv"
    }
    
    # Lifecycle Stages
    lifecycle_stages = {
      new        = "0-30 days since signup"
      onboarding = "Profile completion < 80%"
      active     = "Activity in last 7 days"
      engaged    = "Engagement score > 60"
      at_risk    = "No activity in 14+ days"
      churned    = "No activity in 30+ days"
      reactivated = "Returned after churn"
    }
    
  } : null
  
  sensitive = true
}

# User Segmentation
output "user_segmentation_config" {
  description = "User segmentation configuration for analytics"
  value = var.enable_posthog ? {
    # Demographic segments
    demographics = {
      new_users = {
        criteria = "signup_date within last 30 days"
        cohort_name = "new_users"
      }
      power_users = {
        criteria = "engagement_score >= 80"
        cohort_name = "power_users"
      }
      premium_users = {
        criteria = "subscription_plan in [premium, enterprise]"
        cohort_name = "premium_users"
      }
      at_risk_users = {
        criteria = "last_activity_at > 14 days AND subscription_plan != free"
        cohort_name = "at_risk_users"
      }
    }
    
    # Behavioral segments
    behavior = {
      profile_complete = "profile_completion_percentage = 100"
      feature_adopters = "features_used_count >= 5"
      mobile_users    = "primary_device_type = mobile"
      desktop_users   = "primary_device_type = desktop"
    }
    
    # Business segments
    business = {
      high_value    = "lifetime_value >= 1000"
      trial_users   = "subscription_plan = trial"
      paying_users  = "subscription_plan != free"
      churned_users = "account_status = churned"
    }
    
  } : null
}

# AWS SSM Parameters for User Service
output "aws_ssm_parameters" {
  description = "AWS SSM parameter names for user PostHog configuration"
  value = var.provider == "aws" && var.enable_posthog ? {
    for key, value in local.user_specific_config : key => "/nexpo/user/posthog/${key}"
  } : {}
}

# Cross-Service Integration
output "cross_service_config" {
  description = "Configuration for cross-service integration"
  value = var.enable_posthog ? {
    project_id      = module.user_posthog_base.posthog_project_id
    parent_project  = var.parent_project_id
    service_name    = "user"
    
    # Shared user properties that other services can use
    shared_user_properties = {
      root_id              = "string"  # Primary user identifier
      profile_completion   = "number"  # 0-100 percentage
      subscription_plan    = "string"
      engagement_score     = "number"
      churn_risk_score    = "number"
      lifetime_value      = "number"
      account_type        = "string"
      last_activity_at    = "date"
    }
    
    # Events that other services should be aware of
    shareable_events = [
      "user_profile_created",
      "user_subscription_event",
      "user_engagement_milestone",
      "user_consent_event"
    ]
    
    # User cohorts that can be used by other services
    shareable_cohorts = [
      "new_users",
      "premium_users",
      "power_users",
      "at_risk_users"
    ]
    
  } : null
}

# Webhook Status
output "webhook_configured" {
  description = "Whether webhook is configured for user service"
  value       = var.enable_posthog && var.webhook_endpoint != "" ? true : false
}

# Terraform State Information
output "terraform_state" {
  description = "Terraform state information for user PostHog resources"
  value = var.enable_posthog ? {
    service_name            = "user"
    base_project_created    = true
    user_feature_flags_count = 4
    user_cohorts_count      = 5
    user_actions_count      = 3
    dashboard_created       = true
    webhook_enabled         = var.webhook_endpoint != ""
    custom_events_count     = 8
    gdpr_compliance_enabled = var.enable_gdpr_tracking
    consent_tracking_enabled = var.enable_consent_tracking
  } : {
    service_name = "user"
    base_project_created = false
  }
}
