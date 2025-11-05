# PostHog Analytics Integration for Auth Service
# This module configures PostHog analytics for tracking authentication events,
# social login providers, security events, and user onboarding flows

terraform {
  required_providers {
    posthog = {
      source  = "PostHog/posthog"
      version = "~> 0.1"
    }
  }
}

# Use shared PostHog module as base
module "auth_posthog_base" {
  source = "../../shared/posthog"
  
  enable_posthog          = var.enable_posthog
  service_name           = "auth"
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
  
  # Privacy settings (enhanced for auth service)
  anonymize_ips         = true  # Always true for auth
  data_retention_days   = var.data_retention_days
  
  # Performance settings
  batch_size        = var.batch_size
  flush_interval_ms = var.flush_interval_ms
  
  # Feature toggles
  enable_session_recording     = false  # Not applicable for auth service
  enable_console_logs         = var.enable_console_logs
  enable_debug_logging        = var.enable_debug_logging
  enable_performance_monitoring = var.enable_performance_monitoring
  enable_error_tracking       = var.enable_error_tracking
  
  # Webhook configuration
  webhook_endpoint = var.webhook_endpoint
  webhook_secret   = var.webhook_secret
  additional_webhook_events = [
    "auth_login_success",
    "auth_login_failed",
    "auth_signup_completed",
    "auth_security_event"
  ]
  
  # Custom events for auth service
  custom_events = {
    auth_login_attempt = {
      name                = "Authentication Login Attempt"
      description         = "Track login attempts with provider and success status"
      required_properties = ["provider", "success", "user_id"]
      tags               = ["auth", "login", "security"]
    }
    
    auth_signup_started = {
      name                = "Authentication Signup Started"
      description         = "Track when users start the signup process"
      required_properties = ["provider", "signup_method"]
      tags               = ["auth", "signup", "onboarding"]
    }
    
    auth_signup_completed = {
      name                = "Authentication Signup Completed"
      description         = "Track successful user registrations"
      required_properties = ["provider", "user_id", "verification_required"]
      tags               = ["auth", "signup", "conversion"]
    }
    
    auth_password_reset = {
      name                = "Password Reset Requested"
      description         = "Track password reset requests"
      required_properties = ["user_id", "method"]
      tags               = ["auth", "password", "security"]
    }
    
    auth_social_login = {
      name                = "Social Login Event"
      description         = "Track social authentication provider usage"
      required_properties = ["provider", "success", "new_user"]
      tags               = ["auth", "social", "providers"]
    }
    
    auth_mfa_event = {
      name                = "Multi-Factor Authentication Event"
      description         = "Track MFA setup and usage"
      required_properties = ["mfa_type", "action", "success"]
      tags               = ["auth", "mfa", "security"]
    }
    
    auth_session_event = {
      name                = "Session Management Event"
      description         = "Track session creation, refresh, and termination"
      required_properties = ["action", "session_id", "user_id"]
      tags               = ["auth", "session", "management"]
    }
    
    auth_security_event = {
      name                = "Security Event"
      description         = "Track security-related events and anomalies"
      required_properties = ["event_type", "severity", "user_id"]
      tags               = ["auth", "security", "monitoring"]
    }
  }
  
  # Integration settings
  integrate_with_api_gateway = var.integrate_with_api_gateway
  enable_cross_service_tracking = true
  enable_user_identification = true
  
  # Common tags
  common_tags = merge(var.common_tags, {
    Service = "auth"
    DataClass = "authentication"
  })
}

# Auth-specific Feature Flags
resource "posthog_feature_flag" "social_login_analytics" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = module.auth_posthog_base.posthog_project_id
  key        = "social-login-analytics"
  name       = "Social Login Provider Analytics"
  
  filters {
    groups {
      properties {
        key   = "environment"
        value = var.environment
        type  = "person"
      }
      rollout_percentage = var.social_login_analytics_rollout
    }
  }
  
  active = var.enable_social_login_analytics
}

resource "posthog_feature_flag" "security_monitoring" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = module.auth_posthog_base.posthog_project_id
  key        = "security-monitoring"
  name       = "Enhanced Security Monitoring"
  
  filters {
    groups {
      properties {
        key   = "user_role"
        value = ["admin", "security"]
        type  = "person"
      }
      rollout_percentage = 100
    }
  }
  
  active = var.enable_security_monitoring
}

resource "posthog_feature_flag" "onboarding_analytics" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = module.auth_posthog_base.posthog_project_id
  key        = "onboarding-analytics"
  name       = "User Onboarding Analytics"
  
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
  
  active = var.enable_onboarding_analytics
}

# Auth-specific Cohorts
resource "posthog_cohort" "social_login_users" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = module.auth_posthog_base.posthog_project_id
  name       = "Social Login Users"
  description = "Users who signed up using social authentication"
  
  filters {
    properties {
      key      = "signup_method"
      value    = ["google", "apple", "facebook", "microsoft", "github"]
      operator = "in"
      type     = "person"
    }
  }
  
  is_static = false
}

resource "posthog_cohort" "password_users" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = module.auth_posthog_base.posthog_project_id
  name       = "Password Authentication Users"
  description = "Users who use traditional email/password authentication"
  
  filters {
    properties {
      key      = "signup_method"
      value    = "password"
      operator = "exact"
      type     = "person"
    }
  }
  
  is_static = false
}

resource "posthog_cohort" "mfa_enabled_users" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = module.auth_posthog_base.posthog_project_id
  name       = "MFA Enabled Users"
  description = "Users with multi-factor authentication enabled"
  
  filters {
    properties {
      key      = "mfa_enabled"
      value    = true
      operator = "exact"
      type     = "person"
    }
  }
  
  is_static = false
}

resource "posthog_cohort" "new_signups" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = module.auth_posthog_base.posthog_project_id
  name       = "Recent Signups"
  description = "Users who signed up in the last 7 days"
  
  filters {
    properties {
      key      = "signup_date"
      value    = 7
      operator = "relative_date_range"
      type     = "person"
    }
  }
  
  is_static = false
}

# Auth Service Analytics Dashboard
resource "posthog_dashboard" "auth_analytics" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = module.auth_posthog_base.posthog_project_id
  name       = "Authentication Analytics"
  description = "Comprehensive authentication and security analytics dashboard"
  
  tags = ["auth", "authentication", "security", "social-login"]
}

# Social Login Provider Actions
resource "posthog_action" "google_login" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = module.auth_posthog_base.posthog_project_id
  name       = "Google Login"
  description = "Track Google OAuth authentication events"
  
  steps {
    event = "auth_social_login"
    properties {
      key   = "provider"
      value = "google"
    }
  }
  
  tags = ["auth", "social", "google"]
}

resource "posthog_action" "apple_login" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = module.auth_posthog_base.posthog_project_id
  name       = "Apple Login"
  description = "Track Apple Sign-In authentication events"
  
  steps {
    event = "auth_social_login"
    properties {
      key   = "provider"
      value = "apple"
    }
  }
  
  tags = ["auth", "social", "apple"]
}

# Security Events Action
resource "posthog_action" "failed_login_attempts" {
  count = var.enable_posthog ? 1 : 0
  
  project_id = module.auth_posthog_base.posthog_project_id
  name       = "Failed Login Attempts"
  description = "Track failed authentication attempts for security monitoring"
  
  steps {
    event = "auth_login_attempt"
    properties {
      key   = "success"
      value = false
    }
  }
  
  tags = ["auth", "security", "monitoring"]
}

# Environment Variables specific to Auth Service
locals {
  auth_specific_config = var.enable_posthog ? {
    # Auth-specific feature flags
    POSTHOG_SOCIAL_LOGIN_ANALYTICS = tostring(var.enable_social_login_analytics)
    POSTHOG_SECURITY_MONITORING    = tostring(var.enable_security_monitoring)
    POSTHOG_ONBOARDING_ANALYTICS   = tostring(var.enable_onboarding_analytics)
    
    # Social login providers tracking
    POSTHOG_TRACK_GOOGLE_LOGIN     = tostring(var.track_google_login)
    POSTHOG_TRACK_APPLE_LOGIN      = tostring(var.track_apple_login)
    POSTHOG_TRACK_FACEBOOK_LOGIN   = tostring(var.track_facebook_login)
    POSTHOG_TRACK_MICROSOFT_LOGIN  = tostring(var.track_microsoft_login)
    POSTHOG_TRACK_GITHUB_LOGIN     = tostring(var.track_github_login)
    
    # Security settings
    POSTHOG_TRACK_FAILED_LOGINS    = tostring(var.track_failed_logins)
    POSTHOG_TRACK_MFA_EVENTS       = tostring(var.track_mfa_events)
    POSTHOG_TRACK_SESSION_EVENTS   = tostring(var.track_session_events)
    
    # Onboarding tracking
    POSTHOG_TRACK_SIGNUP_FUNNEL    = tostring(var.track_signup_funnel)
    POSTHOG_TRACK_VERIFICATION_FLOW = tostring(var.track_verification_flow)
    
    # Integration
    POSTHOG_CRA_INTEGRATION        = "true"
    POSTHOG_AUTH0_INTEGRATION      = tostring(var.enable_auth0_integration)
    
  } : {}
}

# Store auth-specific configuration
resource "aws_ssm_parameter" "auth_posthog_config" {
  for_each = var.provider == "aws" && var.enable_posthog ? local.auth_specific_config : {}
  
  name  = "/nexpo/auth/posthog/${each.key}"
  type  = "String"
  value = each.value
  
  tags = merge(var.common_tags, {
    Service   = "auth"
    Component = "posthog-auth-config"
  })
}
