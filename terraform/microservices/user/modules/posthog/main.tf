# PostHog Analytics Integration for User Service
# This module configures PostHog analytics for tracking user lifecycle events,
# profile management, preferences, and user behavior patterns

terraform {
  required_providers {
    posthog = {
      source  = "PostHog/posthog"
      version = "~> 0.1"
    }
  }
}

# Use shared PostHog module as base
module "user_posthog_base" {
  source = "../../shared/posthog"
  
  enable_posthog          = var.enable_posthog
  service_name           = "user"
  project_name           = var.project_name
  environment            = var.environment
  provider               = var.provider
  
  # PostHog configuration
  posthog_organization_id = var.posthog_organization_id
  parent_organization_id  = var.parent_organization_id
  posthog_host           = var.posthog_host
  parent_project_id      = var.parent_project_id
  
  # Analytics settings
  analytics_rollout_percentage = var.analytics_rollout_percentage
  sampling_rate               = var.sampling_rate
  
  # Privacy settings
  anonymize_ips         = var.anonymize_ips
  data_retention_days   = var.data_retention_days
  
  # Performance settings
  batch_size        = var.batch_size
  flush_interval_ms = var.flush_interval_ms
  
  # Feature toggles
  enable_session_recording     = var.enable_session_recording
  enable_console_logs         = var.enable_console_logs
  enable_debug_logging        = var.enable_debug_logging
  enable_performance_monitoring = var.enable_performance_monitoring
  enable_error_tracking       = var.enable_error_tracking
  enable_user_identification   = true  # Always enabled for user service
  
  # Webhook configuration
  webhook_endpoint = var.webhook_endpoint
  webhook_secret   = var.webhook_secret
  additional_webhook_events = [
    "user_profile_updated",
    "user_subscription_changed",
    "user_engagement_event",
    "user_churn_risk"
  ]
  
  # Custom events for user service
  custom_events = {
    user_profile_created = {
      name                = "User Profile Created"
      description         = "Track when new user profiles are created"
      required_properties = ["user_id", "profile_type", "source"]
      tags               = ["user", "profile", "creation"]
    }
    
    user_profile_updated = {
      name                = "User Profile Updated"
      description         = "Track user profile modifications"
      required_properties = ["user_id", "fields_updated", "update_source"]
      tags               = ["user", "profile", "modification"]
    }
    
    user_preferences_changed = {
      name                = "User Preferences Changed"
      description         = "Track changes to user preferences and settings"
      required_properties = ["user_id", "preference_type", "new_value"]
      tags               = ["user", "preferences", "settings"]
    }
    
    user_subscription_event = {
      name                = "User Subscription Event"
      description         = "Track subscription lifecycle events"
      required_properties = ["user_id", "event_type", "subscription_plan"]
      tags               = ["user", "subscription", "lifecycle"]
    }
    
    user_engagement_milestone = {
      name                = "User Engagement Milestone"
      description         = "Track user engagement milestones and achievements"
      required_properties = ["user_id", "milestone_type", "milestone_value"]
      tags               = ["user", "engagement", "milestone"]
    }
    
    user_data_export = {
      name                = "User Data Export Request"
      description         = "Track GDPR data export requests"
      required_properties = ["user_id", "export_type", "request_source"]
      tags               = ["user", "gdpr", "export"]
    }
    
    user_data_deletion = {
      name                = "User Data Deletion Request"
      description         = "Track GDPR data deletion requests"
      required_properties = ["user_id", "deletion_type", "request_source"]
      tags               = ["user", "gdpr", "deletion"]
    }
    
    user_consent_event = {
      name                = "User Consent Event"
      description         = "Track user consent for data processing"
      required_properties = ["user_id", "consent_type", "consent_granted"]
      tags               = ["user", "consent", "privacy"]
    }
  }
  
  # Integration settings
  integrate_with_api_gateway = var.integrate_with_api_gateway
  enable_cross_service_tracking = true
  enable_business_metrics = var.enable_business_metrics
  
  # Common tags
  common_tags = merge(var.common_tags, {
    Service = "user"
    DataClass = "user_data"
  })
}

# User-specific Feature Flags
resource "posthog_feature_flag" "user_profile_analytics" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = module.user_posthog_base.posthog_project_id
  key        = "user-profile-analytics"
  name       = "User Profile Analytics"
  
  filters {
    groups {
      properties {
        key   = "environment"
        value = var.environment
        type  = "person"
      }
      rollout_percentage = var.profile_analytics_rollout
    }
  }
  
  active = var.enable_profile_analytics
}

resource "posthog_feature_flag" "user_engagement_tracking" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = module.user_posthog_base.posthog_project_id
  key        = "user-engagement-tracking"
  name       = "User Engagement Tracking"
  
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
  
  active = var.enable_engagement_tracking
}

resource "posthog_feature_flag" "subscription_analytics" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = module.user_posthog_base.posthog_project_id
  key        = "subscription-analytics"
  name       = "Subscription Analytics"
  
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

resource "posthog_feature_flag" "churn_prediction" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = module.user_posthog_base.posthog_project_id
  key        = "churn-prediction"
  name       = "Churn Prediction Analytics"
  
  filters {
    groups {
      properties {
        key   = "user_role"
        value = ["admin", "analytics"]
        type  = "person"
      }
      rollout_percentage = 100
    }
  }
  
  active = var.enable_churn_prediction
}

# User Lifecycle Cohorts
resource "posthog_cohort" "new_users" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = module.user_posthog_base.posthog_project_id
  name       = "New Users"
  description = "Users who created their profile in the last 30 days"
  
  filters {
    properties {
      key      = "profile_created_at"
      value    = 30
      operator = "relative_date_range"
      type     = "person"
    }
  }
  
  is_static = false
}

resource "posthog_cohort" "active_users" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = module.user_posthog_base.posthog_project_id
  name       = "Active Users"
  description = "Users who have engaged with the platform in the last 7 days"
  
  filters {
    properties {
      key      = "last_activity_at"
      value    = 7
      operator = "relative_date_range"
      type     = "person"
    }
  }
  
  is_static = false
}

resource "posthog_cohort" "premium_users" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = module.user_posthog_base.posthog_project_id
  name       = "Premium Users"
  description = "Users with premium or enterprise subscription plans"
  
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

resource "posthog_cohort" "at_risk_users" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = module.user_posthog_base.posthog_project_id
  name       = "At-Risk Users"
  description = "Users who haven't been active recently and may churn"
  
  filters {
    properties {
      key      = "last_activity_at"
      value    = 14
      operator = "relative_date_range"
      type     = "person"
    }
    
    properties {
      key      = "subscription_plan"
      value    = "free"
      operator = "not_equal"
      type     = "person"
    }
  }
  
  is_static = false
}

resource "posthog_cohort" "power_users" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = module.user_posthog_base.posthog_project_id
  name       = "Power Users"
  description = "Users with high engagement and feature usage"
  
  filters {
    properties {
      key      = "engagement_score"
      value    = 80
      operator = "gte"
      type     = "person"
    }
  }
  
  is_static = false
}

# User Service Analytics Dashboard
resource "posthog_dashboard" "user_analytics" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = module.user_posthog_base.posthog_project_id
  name       = "User Service Analytics"
  description = "Comprehensive user lifecycle and engagement analytics dashboard"
  
  tags = ["user", "lifecycle", "engagement", "churn"]
}

# User Lifecycle Actions
resource "posthog_action" "profile_completion" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = module.user_posthog_base.posthog_project_id
  name       = "Profile Completion"
  description = "Track when users complete their profile setup"
  
  steps {
    event = "user_profile_updated"
    properties {
      key   = "profile_completion_rate"
      value = 100
    }
  }
  
  tags = ["user", "onboarding", "profile"]
}

resource "posthog_action" "subscription_upgrade" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = module.user_posthog_base.posthog_project_id
  name       = "Subscription Upgrade"
  description = "Track when users upgrade their subscription"
  
  steps {
    event = "user_subscription_event"
    properties {
      key   = "event_type"
      value = "upgrade"
    }
  }
  
  tags = ["user", "subscription", "conversion"]
}

resource "posthog_action" "feature_adoption" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = module.user_posthog_base.posthog_project_id
  name       = "Feature Adoption"
  description = "Track adoption of new features by users"
  
  steps {
    event = "user_engagement_milestone"
    properties {
      key   = "milestone_type"
      value = "feature_adoption"
    }
  }
  
  tags = ["user", "feature", "adoption"]
}

# Environment Variables specific to User Service
locals {
  user_specific_config = var.enable_posthog ? {
    # User-specific feature flags
    POSTHOG_PROFILE_ANALYTICS      = tostring(var.enable_profile_analytics)
    POSTHOG_ENGAGEMENT_TRACKING    = tostring(var.enable_engagement_tracking)
    POSTHOG_SUBSCRIPTION_ANALYTICS = tostring(var.enable_subscription_analytics)
    POSTHOG_CHURN_PREDICTION      = tostring(var.enable_churn_prediction)
    
    # Privacy and GDPR settings
    POSTHOG_GDPR_TRACKING         = tostring(var.enable_gdpr_tracking)
    POSTHOG_CONSENT_TRACKING      = tostring(var.enable_consent_tracking)
    POSTHOG_DATA_RETENTION_DAYS   = tostring(var.data_retention_days)
    
    # User lifecycle tracking
    POSTHOG_LIFECYCLE_TRACKING    = tostring(var.enable_lifecycle_tracking)
    POSTHOG_ENGAGEMENT_SCORING    = tostring(var.enable_engagement_scoring)
    POSTHOG_CHURN_RISK_ALERTS     = tostring(var.enable_churn_risk_alerts)
    
    # Feature adoption tracking
    POSTHOG_FEATURE_ADOPTION      = tostring(var.track_feature_adoption)
    POSTHOG_ONBOARDING_FUNNEL     = tostring(var.track_onboarding_funnel)
    
    # Integration with CRA
    POSTHOG_CRA_USER_SYNC         = "true"
    POSTHOG_USER_IDENTITY_MERGE   = tostring(var.enable_identity_merge)
    
  } : {}
}

# Store user-specific configuration
resource "aws_ssm_parameter" "user_posthog_config" {
  for_each = var.provider == "aws" && var.enable_posthog ? local.user_specific_config : {}
  
  name  = "/nexpo/user/posthog/${each.key}"
  type  = "String"
  value = each.value
  
  tags = merge(var.common_tags, {
    Service   = "user"
    Component = "posthog-user-config"
  })
}
