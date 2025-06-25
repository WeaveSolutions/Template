# PostHog Integration Example Configuration
# This file demonstrates how to integrate PostHog analytics across multiple Nexpo microservices

terraform {
  required_providers {
    posthog = {
      source  = "PostHog/posthog"
      version = "~> 0.1"
    }
  }
}

# Configure PostHog provider
provider "posthog" {
  api_key = var.posthog_api_key
  host    = var.posthog_host
}

# Common variables for all services
locals {
  common_posthog_config = {
    project_name           = "nexpo"
    environment           = "prod"
    provider              = "aws"
    posthog_host          = "https://app.posthog.com"
    posthog_organization_id = var.posthog_organization_id
    
    # Privacy settings
    anonymize_ips       = true
    data_retention_days = 365
    
    # Performance settings
    batch_size        = 50
    flush_interval_ms = 5000
    sampling_rate     = 1.0
    
    # Common tags
    common_tags = {
      Project     = "nexpo"
      Environment = "prod"
      Team       = "platform"
      Terraform  = "true"
    }
  }
}

# API Gateway PostHog Integration (Parent Project)
module "api_gateway_posthog" {
  source = "../api-gateway/modules/posthog"
  
  enable_posthog          = true
  project_name           = local.common_posthog_config.project_name
  environment            = local.common_posthog_config.environment
  provider               = local.common_posthog_config.provider
  posthog_organization_id = local.common_posthog_config.posthog_organization_id
  posthog_host           = local.common_posthog_config.posthog_host
  
  # Analytics configuration
  analytics_rollout_percentage = 100
  sampling_rate               = local.common_posthog_config.sampling_rate
  
  # Privacy settings
  anonymize_ips         = local.common_posthog_config.anonymize_ips
  data_retention_days   = local.common_posthog_config.data_retention_days
  
  # Performance settings
  batch_size        = local.common_posthog_config.batch_size
  flush_interval_ms = local.common_posthog_config.flush_interval_ms
  
  # Feature toggles
  enable_session_recording     = false
  enable_console_logs         = true
  enable_detailed_logging     = false
  enable_api_analytics        = true
  enable_error_tracking       = true
  enable_performance_monitoring = true
  
  # Social login analytics
  track_social_login_providers     = true
  social_login_conversion_tracking = true
  
  # Business metrics
  enable_business_metrics    = true
  track_subscription_events  = true
  enable_revenue_tracking    = false  # Enable when ready
  
  # Webhook configuration
  webhook_endpoint = var.posthog_webhook_endpoint
  webhook_secret   = var.posthog_webhook_secret
  
  common_tags = local.common_posthog_config.common_tags
}

# Auth Service PostHog Integration
module "auth_posthog" {
  source = "../auth/modules/posthog"
  
  enable_posthog          = true
  project_name           = local.common_posthog_config.project_name
  environment            = local.common_posthog_config.environment
  provider               = local.common_posthog_config.provider
  posthog_organization_id = local.common_posthog_config.posthog_organization_id
  parent_organization_id  = local.common_posthog_config.posthog_organization_id
  posthog_host           = local.common_posthog_config.posthog_host
  parent_project_id      = module.api_gateway_posthog.posthog_project_id
  
  # Analytics settings
  analytics_rollout_percentage = 100
  sampling_rate               = local.common_posthog_config.sampling_rate
  
  # Privacy settings (enhanced for auth)
  data_retention_days = local.common_posthog_config.data_retention_days
  
  # Performance settings
  batch_size        = local.common_posthog_config.batch_size
  flush_interval_ms = local.common_posthog_config.flush_interval_ms
  
  # Auth-specific features
  enable_social_login_analytics = true
  enable_security_monitoring   = true
  enable_onboarding_analytics  = true
  social_login_analytics_rollout = 100
  
  # Social login provider tracking
  track_google_login    = true
  track_apple_login     = true
  track_facebook_login  = true
  track_microsoft_login = true
  track_github_login    = true
  
  # Security features
  track_failed_logins   = true
  track_mfa_events     = true
  track_session_events = true
  
  # Onboarding tracking
  track_signup_funnel     = true
  track_verification_flow = true
  
  # Integration settings
  integrate_with_api_gateway = true
  enable_auth0_integration  = true
  
  # Webhook configuration
  webhook_endpoint = var.posthog_webhook_endpoint
  webhook_secret   = var.posthog_webhook_secret
  
  common_tags = local.common_posthog_config.common_tags
}

# Payments Service PostHog Integration
module "payments_posthog" {
  source = "../payments/modules/posthog"
  
  enable_posthog          = true
  project_name           = local.common_posthog_config.project_name
  environment            = local.common_posthog_config.environment
  provider               = local.common_posthog_config.provider
  posthog_organization_id = local.common_posthog_config.posthog_organization_id
  parent_organization_id  = local.common_posthog_config.posthog_organization_id
  posthog_host           = local.common_posthog_config.posthog_host
  parent_project_id      = module.api_gateway_posthog.posthog_project_id
  
  # Analytics settings
  analytics_rollout_percentage = 100
  sampling_rate               = local.common_posthog_config.sampling_rate
  
  # Privacy settings (enhanced for payments)
  data_retention_days = 2555  # 7 years for financial compliance
  
  # Performance settings
  batch_size        = local.common_posthog_config.batch_size
  flush_interval_ms = local.common_posthog_config.flush_interval_ms
  
  # Payments-specific features
  enable_revenue_tracking         = true
  enable_subscription_analytics   = true
  enable_churn_prediction        = true
  enable_payment_provider_analytics = true
  enable_fraud_detection         = true
  
  # Business intelligence
  track_subscription_lifecycle = true
  track_payment_methods       = true
  track_revenue_metrics       = true
  track_churn_indicators      = true
  
  # Compliance features
  enable_pci_compliance = true
  enable_gdpr_tracking  = true
  
  # Integration settings
  integrate_with_api_gateway = true
  enable_stripe_integration = true
  enable_paypal_integration = true
  
  # Webhook configuration
  webhook_endpoint = var.posthog_webhook_endpoint
  webhook_secret   = var.posthog_webhook_secret
  
  common_tags = merge(local.common_posthog_config.common_tags, {
    DataClass = "financial"
    Compliance = "pci-dss"
  })
}

# User Service PostHog Integration
module "user_posthog" {
  source = "../user/modules/posthog"
  
  enable_posthog          = true
  project_name           = local.common_posthog_config.project_name
  environment            = local.common_posthog_config.environment
  provider               = local.common_posthog_config.provider
  posthog_organization_id = local.common_posthog_config.posthog_organization_id
  parent_organization_id  = local.common_posthog_config.posthog_organization_id
  posthog_host           = local.common_posthog_config.posthog_host
  parent_project_id      = module.api_gateway_posthog.posthog_project_id
  
  # Analytics settings
  analytics_rollout_percentage = 100
  sampling_rate               = local.common_posthog_config.sampling_rate
  
  # Privacy settings
  anonymize_ips       = local.common_posthog_config.anonymize_ips
  data_retention_days = local.common_posthog_config.data_retention_days
  
  # Performance settings
  batch_size        = local.common_posthog_config.batch_size
  flush_interval_ms = local.common_posthog_config.flush_interval_ms
  
  # User-specific features
  enable_profile_analytics      = true
  enable_engagement_tracking    = true
  enable_subscription_analytics = true
  enable_churn_prediction      = true
  profile_analytics_rollout    = 100
  
  # Privacy and compliance
  enable_gdpr_tracking    = true
  enable_consent_tracking = true
  
  # User lifecycle tracking
  enable_lifecycle_tracking   = true
  enable_engagement_scoring   = true
  enable_churn_risk_alerts   = true
  
  # Feature adoption
  track_feature_adoption   = true
  track_onboarding_funnel = true
  
  # Integration settings
  integrate_with_api_gateway = true
  enable_business_metrics   = true
  enable_identity_merge     = true
  
  # Webhook configuration
  webhook_endpoint = var.posthog_webhook_endpoint
  webhook_secret   = var.posthog_webhook_secret
  
  common_tags = merge(local.common_posthog_config.common_tags, {
    DataClass = "user_data"
  })
}

# Additional microservices using shared module
module "kpi_engine_posthog" {
  source = "../shared/posthog"
  
  enable_posthog          = true
  service_name           = "kpi-engine"
  project_name           = local.common_posthog_config.project_name
  environment            = local.common_posthog_config.environment
  provider               = local.common_posthog_config.provider
  posthog_organization_id = local.common_posthog_config.posthog_organization_id
  parent_organization_id  = local.common_posthog_config.posthog_organization_id
  posthog_host           = local.common_posthog_config.posthog_host
  parent_project_id      = module.api_gateway_posthog.posthog_project_id
  
  # Analytics settings
  analytics_rollout_percentage = 100
  sampling_rate               = local.common_posthog_config.sampling_rate
  
  # Privacy settings
  anonymize_ips       = local.common_posthog_config.anonymize_ips
  data_retention_days = local.common_posthog_config.data_retention_days
  
  # Performance settings
  batch_size        = local.common_posthog_config.batch_size
  flush_interval_ms = local.common_posthog_config.flush_interval_ms
  
  # Feature toggles
  enable_debug_logging        = false
  enable_performance_monitoring = true
  enable_error_tracking       = true
  enable_business_metrics     = true
  
  # Custom events for KPI Engine
  custom_events = {
    kpi_calculation = {
      name                = "KPI Calculation"
      description         = "Track KPI calculation events"
      required_properties = ["kpi_type", "calculation_time", "data_points"]
      tags               = ["kpi", "calculation", "performance"]
    }
    
    kpi_alert_triggered = {
      name                = "KPI Alert Triggered"
      description         = "Track when KPI alerts are triggered"
      required_properties = ["kpi_name", "threshold", "current_value", "severity"]
      tags               = ["kpi", "alert", "monitoring"]
    }
    
    dashboard_viewed = {
      name                = "Dashboard Viewed"
      description         = "Track dashboard usage"
      required_properties = ["dashboard_id", "user_id", "view_duration"]
      tags               = ["dashboard", "usage", "analytics"]
    }
  }
  
  # Integration settings
  integrate_with_api_gateway = true
  enable_cross_service_tracking = true
  
  # Webhook configuration
  webhook_endpoint = var.posthog_webhook_endpoint
  webhook_secret   = var.posthog_webhook_secret
  additional_webhook_events = [
    "kpi_alert_triggered",
    "dashboard_error"
  ]
  
  common_tags = merge(local.common_posthog_config.common_tags, {
    Service = "kpi-engine"
    DataClass = "business_intelligence"
  })
}

# Outputs for integration
output "posthog_projects" {
  description = "PostHog project information for all services"
  value = {
    api_gateway = {
      project_id = module.api_gateway_posthog.posthog_project_id
      dashboard_url = module.api_gateway_posthog.dashboard_url
    }
    auth = {
      project_id = module.auth_posthog.posthog_project_id
      dashboard_url = module.auth_posthog.dashboard_url
    }
    payments = {
      project_id = module.payments_posthog.posthog_project_id
      dashboard_url = module.payments_posthog.dashboard_url
    }
    user = {
      project_id = module.user_posthog.posthog_project_id
      dashboard_url = module.user_posthog.dashboard_url
    }
    kpi_engine = {
      project_id = module.kpi_engine_posthog.posthog_project_id
      dashboard_url = module.kpi_engine_posthog.dashboard_url
    }
  }
}

output "posthog_environment_variables" {
  description = "Environment variables for all services (use in CI/CD)"
  value = {
    api_gateway = module.api_gateway_posthog.environment_variables
    auth       = module.auth_posthog.environment_variables
    payments   = module.payments_posthog.environment_variables
    user       = module.user_posthog.environment_variables
    kpi_engine = module.kpi_engine_posthog.environment_variables
  }
  sensitive = true
}

# Variables for the example
variable "posthog_api_key" {
  description = "PostHog API key for Terraform provider"
  type        = string
  sensitive   = true
}

variable "posthog_organization_id" {
  description = "PostHog organization ID"
  type        = string
}

variable "posthog_host" {
  description = "PostHog host URL"
  type        = string
  default     = "https://app.posthog.com"
}

variable "posthog_webhook_endpoint" {
  description = "Webhook endpoint for PostHog events"
  type        = string
  default     = ""
}

variable "posthog_webhook_secret" {
  description = "Secret for webhook authentication"
  type        = string
  default     = ""
  sensitive   = true
}
