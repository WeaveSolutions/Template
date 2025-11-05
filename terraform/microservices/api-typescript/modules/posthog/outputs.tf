# Auth Service PostHog Module Outputs

# Base Module Outputs
output "posthog_project_id" {
  description = "PostHog project ID for the auth service"
  value       = var.enable_posthog ? module.auth_posthog_base.posthog_project_id : null
}

output "posthog_project_api_key" {
  description = "PostHog project API key for auth service"
  value       = var.enable_posthog ? module.auth_posthog_base.posthog_project_api_key : null
  sensitive   = true
}

output "posthog_host" {
  description = "PostHog host URL"
  value       = var.posthog_host
}

output "service_name" {
  description = "Name of the service (auth)"
  value       = "auth"
}

# Auth-Specific Feature Flags
output "auth_feature_flags" {
  description = "Auth-specific feature flags"
  value = var.enable_posthog ? merge(
    module.auth_posthog_base.feature_flags,
    {
      social_login_analytics = posthog_feature_flag.social_login_analytics[0].key
      security_monitoring   = posthog_feature_flag.security_monitoring[0].key
      onboarding_analytics  = posthog_feature_flag.onboarding_analytics[0].key
    }
  ) : {}
}

# Analytics Actions
output "auth_analytics_actions" {
  description = "Auth service analytics actions"
  value = var.enable_posthog ? merge(
    module.auth_posthog_base.analytics_actions,
    {
      google_login        = posthog_action.google_login[0].name
      apple_login         = posthog_action.apple_login[0].name
      failed_login_attempts = posthog_action.failed_login_attempts[0].name
    }
  ) : {}
}

# Auth Cohorts
output "auth_cohorts" {
  description = "Auth service user cohorts"
  value = var.enable_posthog ? merge(
    module.auth_posthog_base.cohorts,
    {
      social_login_users = posthog_cohort.social_login_users[0].name
      password_users     = posthog_cohort.password_users[0].name
      mfa_enabled_users  = posthog_cohort.mfa_enabled_users[0].name
      new_signups       = posthog_cohort.new_signups[0].name
    }
  ) : {}
}

# Dashboard
output "dashboard_url" {
  description = "URL to the auth analytics dashboard"
  value = var.enable_posthog ? "${var.posthog_host}/project/${module.auth_posthog_base.posthog_project_id}/dashboard/${posthog_dashboard.auth_analytics[0].id}" : null
}

# Environment Variables for Auth Service
output "environment_variables" {
  description = "Environment variables for auth service PostHog integration"
  value = var.enable_posthog ? merge(
    module.auth_posthog_base.environment_variables,
    local.auth_specific_config
  ) : {}
  
  sensitive = true
}

# Auth Event Schema
output "auth_event_schema" {
  description = "Event schema for auth service"
  value = {
    auth_login_attempt = {
      event_name = "auth_login_attempt"
      properties = {
        provider    = "string"  # google, apple, facebook, password, etc.
        success     = "boolean"
        user_id     = "string"  # CRA root_id
        session_id  = "string"
        ip_address  = "string"  # anonymized if privacy enabled
        user_agent  = "string"
        login_method = "string" # social, password, sso
        error_code  = "string"  # if failed
      }
    }
    
    auth_signup_started = {
      event_name = "auth_signup_started"
      properties = {
        provider      = "string"
        signup_method = "string"
        referrer      = "string"
        utm_source    = "string"
        utm_medium    = "string"
        utm_campaign  = "string"
      }
    }
    
    auth_signup_completed = {
      event_name = "auth_signup_completed"
      properties = {
        provider             = "string"
        user_id             = "string"
        verification_required = "boolean"
        profile_complete     = "boolean"
        signup_duration_ms   = "number"
        referrer            = "string"
      }
    }
    
    auth_password_reset = {
      event_name = "auth_password_reset"
      properties = {
        user_id = "string"
        method  = "string"  # email, sms
        success = "boolean"
      }
    }
    
    auth_social_login = {
      event_name = "auth_social_login"
      properties = {
        provider    = "string"  # google, apple, facebook, microsoft, github
        success     = "boolean"
        new_user    = "boolean"
        user_id     = "string"
        permissions = "array"   # requested permissions
        error_code  = "string"  # if failed
      }
    }
    
    auth_mfa_event = {
      event_name = "auth_mfa_event"
      properties = {
        mfa_type   = "string"  # totp, sms, email, backup_codes
        action     = "string"  # setup, verify, disable, backup_used
        success    = "boolean"
        user_id    = "string"
        device_id  = "string"
      }
    }
    
    auth_session_event = {
      event_name = "auth_session_event"
      properties = {
        action     = "string"  # create, refresh, terminate, expire
        session_id = "string"
        user_id    = "string"
        duration_ms = "number"
        device_type = "string"
        location   = "string"
      }
    }
    
    auth_security_event = {
      event_name = "auth_security_event"
      properties = {
        event_type = "string"  # suspicious_login, account_locked, brute_force, etc.
        severity   = "string"  # low, medium, high, critical
        user_id    = "string"
        details    = "object"
        action_taken = "string"
      }
    }
  }
}

# Integration Configuration
output "auth_integration_config" {
  description = "Configuration for integrating auth service with PostHog"
  value = var.enable_posthog ? {
    # SDK Configuration for Auth Service
    sdk_config = {
      api_key = module.auth_posthog_base.posthog_project_api_key
      host    = var.posthog_host
      
      # Auth-specific options
      capture_pageview    = false  # Not applicable for auth service
      session_recording   = false  # Privacy concern for auth
      capture_console_logs = var.enable_console_logs
      anonymize_ips       = true   # Always true for auth
      
      # Performance
      sampling_rate    = var.sampling_rate
      batch_size      = var.batch_size
      flush_interval  = var.flush_interval_ms
    }
    
    # Auth-specific Feature Flags
    feature_flags = {
      social_login_analytics = "social-login-analytics"
      security_monitoring   = "security-monitoring"
      onboarding_analytics  = "onboarding-analytics"
    }
    
    # Auth Events
    events = {
      login_attempt      = "auth_login_attempt"
      signup_started     = "auth_signup_started"
      signup_completed   = "auth_signup_completed"
      password_reset     = "auth_password_reset"
      social_login       = "auth_social_login"
      mfa_event         = "auth_mfa_event"
      session_event     = "auth_session_event"
      security_event    = "auth_security_event"
    }
    
    # User Properties for Auth Context
    user_properties = {
      user_id              = "root_id"           # From CRA
      signup_method        = "signup_method"     # google, apple, password, etc.
      mfa_enabled         = "mfa_enabled"
      login_provider      = "primary_login_provider"
      email_verified      = "email_verified"
      phone_verified      = "phone_verified"
      account_status      = "account_status"     # active, suspended, pending
      last_login_at       = "last_login_at"
      signup_date         = "signup_date"
      security_score      = "security_score"
    }
    
    # Social Login Providers
    social_providers = {
      google    = var.track_google_login
      apple     = var.track_apple_login
      facebook  = var.track_facebook_login
      microsoft = var.track_microsoft_login
      github    = var.track_github_login
    }
    
  } : null
  
  sensitive = true
}

# AWS SSM Parameters for Auth Service
output "aws_ssm_parameters" {
  description = "AWS SSM parameter names for auth PostHog configuration"
  value = var.provider == "aws" && var.enable_posthog ? {
    for key, value in local.auth_specific_config : key => "/nexpo/auth/posthog/${key}"
  } : {}
}

# Cross-Service Integration
output "cross_service_config" {
  description = "Configuration for cross-service integration"
  value = var.enable_posthog ? {
    project_id      = module.auth_posthog_base.posthog_project_id
    parent_project  = var.parent_project_id
    service_name    = "auth"
    
    # Shared user properties
    shared_user_properties = {
      root_id         = "string"  # Primary user identifier
      signup_method   = "string"
      signup_date     = "date"
      email_verified  = "boolean"
      mfa_enabled    = "boolean"
    }
    
    # Events that other services can subscribe to
    shareable_events = [
      "auth_signup_completed",
      "auth_login_attempt",
      "auth_mfa_event"
    ]
    
  } : null
}

# Webhook Status
output "webhook_configured" {
  description = "Whether webhook is configured for auth service"
  value       = var.enable_posthog && var.webhook_endpoint != "" ? true : false
}

# Terraform State Information
output "terraform_state" {
  description = "Terraform state information for auth PostHog resources"
  value = var.enable_posthog ? {
    service_name            = "auth"
    base_project_created    = true
    auth_feature_flags_count = 3
    auth_cohorts_count      = 4
    auth_actions_count      = 3
    dashboard_created       = true
    webhook_enabled         = var.webhook_endpoint != ""
    custom_events_count     = 8
    social_providers_tracked = length([
      for provider, enabled in {
        google    = var.track_google_login
        apple     = var.track_apple_login
        facebook  = var.track_facebook_login
        microsoft = var.track_microsoft_login
        github    = var.track_github_login
      } : provider if enabled
    ])
  } : {
    service_name = "auth"
    base_project_created = false
  }
}
