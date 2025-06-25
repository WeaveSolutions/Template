# PostHog Analytics Integration - Nexpo Template

## Overview
PostHog is a comprehensive product analytics platform that provides event tracking, feature flags, session recordings, and A/B testing capabilities. This document outlines the generic implementation strategy for integrating PostHog into the Nexpo template, supporting both web (Next.js) and mobile (Expo) applications.

**Key Features:**
- **Event Tracking**: Custom events, user properties, and conversion funnels
- **Feature Flags**: Dynamic feature rollouts and A/B testing
- **Session Recordings**: User behavior analysis and debugging
- **Heatmaps**: Visual user interaction insights
- **Cohort Analysis**: User segmentation and retention tracking
- **Privacy-First**: GDPR compliant with data residency options

## Table of Contents

### 1. [Architecture](#architecture)
- [System Overview](#system-overview)
- [Data Flow](#data-flow)
- [Privacy & Compliance](#privacy-compliance)

### 2. [Setup & Configuration](#setup-configuration)
- [PostHog Project Setup](#posthog-project-setup)
- [Environment Variables](#environment-variables)
- [SDK Installation](#sdk-installation)

### 3. [Implementation](#implementation)
- [Next.js Integration](#nextjs-integration)
- [Expo Integration](#expo-integration)
- [Event Tracking Strategy](#event-tracking-strategy)
- [User Identification](#user-identification)

### 4. [Feature Flags](#feature-flags)
- [Implementation Guide](#feature-flags-implementation)
- [Best Practices](#feature-flags-best-practices)

### 5. [Analytics Strategy](#analytics-strategy)
- [Core Events](#core-events)
- [User Properties](#user-properties)
- [Custom Events](#custom-events)

### 6. [Terraform Infrastructure Integration](#terraform-infrastructure)
- [Microservices PostHog Modules](#microservices-posthog-modules)
- [Shared Module Architecture](#shared-module-architecture)
- [Service-Specific Configurations](#service-specific-configurations)
- [Cross-Service Analytics](#cross-service-analytics)

### 7. [Security & Privacy](#security-privacy)
- [Data Protection](#data-protection)
- [GDPR Compliance](#gdpr-compliance)
- [User Consent Management](#user-consent-management)

---

## Terraform Infrastructure Integration

The Nexpo template includes comprehensive Terraform modules for PostHog integration across microservices, providing infrastructure-as-code management for analytics, feature flags, cohorts, dashboards, and security configurations.

### Microservices PostHog Modules

#### Overview
Each microservice has a dedicated PostHog Terraform module that extends a shared base module, providing:
- Service-specific PostHog projects and configurations
- Custom event schemas tailored to service domain
- Feature flags for controlled analytics rollout
- User cohorts for service-specific segmentation
- Dashboards and actions for service monitoring
- Secure environment variable management
- Cross-service integration capabilities

#### Available Modules

1. **API Gateway PostHog Module** (`terraform/microservices/api-gateway/`)
   - Request/response analytics
   - Rate limiting and error tracking
   - Cross-service event coordination
   - Business intelligence dashboards

2. **Auth Service PostHog Module** (`terraform/microservices/auth/modules/posthog/`)
   - Authentication flow tracking
   - Social login provider analytics
   - Security monitoring and fraud detection
   - User onboarding funnel analysis

3. **User Service PostHog Module** (`terraform/microservices/user/modules/posthog/`)
   - User lifecycle management
   - Profile completion tracking
   - Engagement scoring and churn prediction
   - GDPR compliance and consent tracking

4. **Payments Service PostHog Module** (`terraform/microservices/payments/modules/posthog/`)
   - Revenue and subscription analytics
   - Payment provider performance tracking
   - Churn prediction and customer lifetime value
   - PCI-compliant financial data handling

### Shared Module Architecture

#### Base Module (`terraform/microservices/shared/posthog/`)

The shared PostHog module provides common functionality that all service modules inherit:

```hcl
# Example usage of shared module
module "auth_posthog_base" {
  source = "../../../shared/posthog"
  
  # Common configuration
  project_name              = var.project_name
  environment              = var.environment
  service_name             = "auth"
  posthog_organization_id  = var.posthog_organization_id
  
  # Analytics settings
  enable_posthog           = var.enable_posthog
  analytics_rollout_percentage = var.analytics_rollout_percentage
  sampling_rate            = var.sampling_rate
  
  # Privacy and compliance
  anonymize_ips            = true
  data_retention_days      = var.data_retention_days
  
  # Performance tuning
  batch_size              = var.batch_size
  flush_interval_ms       = var.flush_interval_ms
  
  # Integration
  parent_project_id       = var.parent_project_id
  webhook_endpoint        = var.webhook_endpoint
  webhook_secret          = var.webhook_secret
  
  common_tags = var.common_tags
}
```

#### Key Features of Shared Module:

1. **PostHog Project Management**
   - Automated project creation and configuration
   - API key generation and secure storage
   - Organization and team management

2. **Privacy-First Configuration**
   - IP anonymization enabled by default
   - Configurable data retention policies
   - GDPR and PCI compliance settings

3. **Performance Optimization**
   - Configurable sampling rates
   - Batching and flush interval tuning
   - Resource allocation management

4. **Integration Support**
   - Cross-service project linking
   - Webhook configuration for real-time events
   - Cloud provider secret management (AWS SSM, GCP Secret Manager, Azure Key Vault)

### Service-Specific Configurations

#### Auth Service Configuration Example

```hcl
# terraform/microservices/auth/main.tf
module "auth_posthog" {
  source = "./modules/posthog"
  
  # Base configuration
  enable_posthog              = var.enable_posthog_analytics
  project_name               = var.project_name
  environment                = var.environment
  posthog_organization_id    = var.posthog_organization_id
  parent_project_id          = var.api_gateway_posthog_project_id
  
  # Auth-specific features
  enable_social_login_analytics = var.enable_social_login_analytics
  enable_security_monitoring    = var.enable_security_monitoring
  enable_onboarding_analytics   = var.enable_onboarding_analytics
  
  # Social login provider tracking
  track_google_login    = var.track_google_login
  track_apple_login     = var.track_apple_login
  track_facebook_login  = var.track_facebook_login
  track_microsoft_login = var.track_microsoft_login
  track_github_login    = var.track_github_login
  
  # Security tracking
  track_failed_logins  = var.track_failed_logins
  track_mfa_events     = var.track_mfa_events
  track_session_events = var.track_session_events
  
  # Integration settings
  integrate_with_api_gateway = var.integrate_with_api_gateway
  webhook_endpoint           = var.auth_webhook_endpoint
  webhook_secret            = var.auth_webhook_secret
  
  common_tags = var.common_tags
}
```

#### Custom Events Schema

Each service defines domain-specific events:

**Auth Service Events:**
- `auth_login_attempt`: Login success/failure tracking
- `auth_signup_started/completed`: User registration funnel
- `auth_social_login`: OAuth provider analytics
- `auth_mfa_event`: Multi-factor authentication tracking
- `auth_security_event`: Security incident monitoring

**User Service Events:**
- `user_profile_created/updated`: Profile lifecycle
- `user_preferences_changed`: Settings modifications
- `user_subscription_event`: Subscription changes
- `user_engagement_milestone`: Engagement scoring
- `user_consent_event`: GDPR compliance tracking

**Payments Service Events:**
- `subscription_created/cancelled`: Subscription lifecycle
- `payment_completed/failed`: Transaction tracking
- `refund_processed`: Refund management
- `churn_prediction_event`: Customer retention
- `revenue_milestone`: Business intelligence

### Cross-Service Analytics

#### Unified User Journey Tracking

The microservices PostHog integration supports cross-service user journey tracking:

```hcl
# Cross-service configuration example
output "cross_service_config" {
  description = "Configuration for cross-service integration"
  value = {
    project_id      = module.service_posthog_base.posthog_project_id
    parent_project  = var.parent_project_id
    service_name    = "auth"
    
    # Shared user properties across services
    shared_user_properties = {
      root_id         = "string"  # Primary user identifier from CRA
      signup_method   = "string"  # google, apple, password, etc.
      signup_date     = "date"
      email_verified  = "boolean"
      subscription_plan = "string"
      engagement_score = "number"
    }
    
    # Events that other services can subscribe to
    shareable_events = [
      "auth_signup_completed",
      "user_subscription_event",
      "payment_completed"
    ]
  }
}
```

#### Integration Benefits

1. **Unified Analytics**: Single view of user journey across services
2. **Consistent Feature Flags**: Coordinated feature rollouts
3. **Cross-Service Cohorts**: User segmentation across service boundaries
4. **Shared Business Intelligence**: Comprehensive dashboard views
5. **Cost Optimization**: Efficient resource usage and data deduplication

### Deployment and Management

#### Terraform Deployment

```bash
# Deploy PostHog for specific service
cd terraform/microservices/auth
terraform init
terraform plan -var-file="environments/prod.tfvars"
terraform apply

# Deploy all microservices with PostHog
cd terraform/microservices
./scripts/deploy-all-services.sh prod
```

#### Environment Variables Output

Each module generates environment variables for application integration:

```bash
# Example output for auth service
POSTHOG_PROJECT_API_KEY=phc_auth_service_key
POSTHOG_HOST=https://app.posthog.com
POSTHOG_PROJECT_ID=auth-service-project-id
POSTHOG_FEATURE_FLAGS_ENABLED=true
POSTHOG_SOCIAL_LOGIN_ANALYTICS=true
POSTHOG_SECURITY_MONITORING=true
```

#### Monitoring and Observability

The Terraform modules include built-in monitoring:

```hcl
# Dashboard configuration
resource "posthog_dashboard" "auth_analytics" {
  name        = "Auth Service Analytics - ${var.environment}"
  description = "Authentication flow metrics and security monitoring"
  
  widgets = [
    {
      type = "trends"
      name = "Login Success Rate"
      events = ["auth_login_attempt"]
      breakdown_by = "success"
    },
    {
      type = "funnel"
      name = "Signup Conversion"
      events = ["auth_signup_started", "auth_signup_completed"]
    }
  ]
}
```

### Security and Compliance

#### Data Protection

- **IP Anonymization**: Enabled by default for all services
- **Data Retention**: Configurable per compliance requirements
- **Access Controls**: IAM-based PostHog project access
- **Audit Logging**: Complete deployment and configuration audit trail

#### Compliance Features

- **GDPR Support**: Built-in consent tracking and data deletion capabilities
- **PCI Compliance**: Special handling for payment service data
- **SOC 2**: Audit-ready infrastructure and access controls
- **HIPAA**: Configurable for healthcare data requirements

For detailed implementation examples and advanced configuration options, see:
- `/terraform/microservices/examples/posthog-integration.tf`
- `/terraform/microservices/shared/posthog/README.md`
- Individual service module documentation in respective directories

---

## Security & Privacy

### Data Protection

#### Data Minimization
```typescript
// Only collect necessary data
const sanitizeEventData = (data: Record<string, any>) => {
  const allowedFields = [
    'user_id',
    'event_name',
    'timestamp',
    'platform',
    'feature_name',
    // Add other allowed fields
  ]

  return Object.keys(data)
    .filter(key => allowedFields.includes(key))
    .reduce((obj, key) => {
      obj[key] = data[key]
      return obj
    }, {} as Record<string, any>)
}
```

#### PII Handling
```typescript
// Hash sensitive data before sending
import crypto from 'crypto'

const hashPII = (value: string): string => {
  return crypto.createHash('sha256').update(value).digest('hex')
}

// Example usage
track('user_action', {
  user_id_hash: hashPII(user.email), // Instead of plain email
  action_type: 'profile_update',
})
```

### GDPR Compliance

#### Consent Management
```typescript
export const useAnalyticsConsent = () => {
  const [hasConsent, setHasConsent] = useState(false)
  const { track } = usePostHog()

  useEffect(() => {
    // Check stored consent
    const consent = localStorage.getItem('analytics_consent')
    setHasConsent(consent === 'granted')
  }, [])

  const grantConsent = () => {
    localStorage.setItem('analytics_consent', 'granted')
    setHasConsent(true)
    track('analytics_consent_granted')
  }

  const revokeConsent = () => {
    localStorage.setItem('analytics_consent', 'revoked')
    setHasConsent(false)
    track('analytics_consent_revoked')
    
    // Optionally reset PostHog
    if (typeof window !== 'undefined') {
      posthog.reset()
    }
  }

  return { hasConsent, grantConsent, revokeConsent }
}
```

#### Data Deletion
```typescript
// Server-side data deletion
export const deleteUserAnalyticsData = async (userId: string) => {
  try {
    await serverPostHog.delete({
      person: {
        distinct_id: userId,
      },
    })
    
    console.log(`Analytics data deleted for user: ${userId}`)
  } catch (error) {
    console.error('Failed to delete analytics data:', error)
    throw error
  }
}
```

### User Consent Management

#### Consent Banner Component
```typescript
import { useAnalyticsConsent } from '../hooks/useAnalyticsConsent'

export const ConsentBanner: React.FC = () => {
  const { hasConsent, grantConsent, revokeConsent } = useAnalyticsConsent()
  const [showBanner, setShowBanner] = useState(false)

  useEffect(() => {
    const consent = localStorage.getItem('analytics_consent')
    if (!consent) {
      setShowBanner(true)
    }
  }, [])

  if (!showBanner || hasConsent) return null

  return (
    <div className="fixed bottom-0 left-0 right-0 bg-gray-900 text-white p-4 z-50">
      <div className="max-w-4xl mx-auto flex items-center justify-between">
        <p className="text-sm">
          We use analytics to improve your experience. By continuing, you agree to our use of cookies and data collection.
        </p>
        <div className="flex gap-2">
          <button
            onClick={() => {
              setShowBanner(false)
              // Implicit consent by continuing to use
            }}
            className="px-4 py-2 text-sm border border-gray-600 rounded hover:bg-gray-800"
          >
            Decline
          </button>
          <button
            onClick={() => {
              grantConsent()
              setShowBanner(false)
            }}
            className="px-4 py-2 text-sm bg-blue-600 rounded hover:bg-blue-700"
          >
            Accept
          </button>
        </div>
      </div>
    </div>
  )
}
```

---

## Best Practices

### Performance Optimization

1. **Event Batching**
   ```typescript
   // Configure batching for better performance
   posthog.init(apiKey, {
     batch_size: 10,
     flush_interval: 30000, // 30 seconds
   })
   ```

2. **Lazy Loading**
   ```typescript
   // Load PostHog only when needed
   const loadPostHog = async () => {
     if (!window.posthog) {
       const { default: posthog } = await import('posthog-js')
       // Initialize PostHog
     }
   }
   ```

3. **Error Handling**
   ```typescript
   const safeTrack = (event: string, properties?: Record<string, any>) => {
     try {
       if (posthog && posthog.__loaded) {
         posthog.capture(event, properties)
       }
     } catch (error) {
       console.warn('Analytics tracking failed:', error)
     }
   }
   ```

### Development Workflow

1. **Environment Separation**
   ```typescript
   // Use different PostHog projects for different environments
   const getPostHogConfig = () => {
     switch (process.env.NODE_ENV) {
       case 'development':
         return { key: 'dev_key', debug: true }
       case 'staging':
         return { key: 'staging_key', debug: false }
       case 'production':
         return { key: 'prod_key', debug: false }
       default:
         return null
     }
   }
   ```

2. **Testing Strategy**
   ```typescript
   // Mock PostHog in tests
   jest.mock('posthog-js', () => ({
     init: jest.fn(),
     capture: jest.fn(),
     identify: jest.fn(),
     getFeatureFlag: jest.fn(),
   }))
   ```

3. **Analytics Documentation**
   - Document all events and their properties
   - Maintain an event catalog
   - Create analytics playbooks for common scenarios

---

## Troubleshooting

### Common Issues

1. **Events Not Appearing**
   - Check API key configuration
   - Verify network connectivity
   - Check browser console for errors
   - Ensure PostHog is properly initialized

2. **Feature Flags Not Working**
   - Verify user identification
   - Check flag configuration in PostHog dashboard
   - Ensure proper async handling

3. **Performance Issues**
   - Review event volume and frequency
   - Optimize event batching
   - Consider sampling for high-volume events

### Debug Mode

```typescript
// Enable debug mode in development
if (process.env.NODE_ENV === 'development') {
  posthog.debug(true)
}

// Custom debug logging
const debugTrack = (event: string, properties?: Record<string, any>) => {
  if (process.env.NODE_ENV === 'development') {
    console.log('Analytics Event:', { event, properties })
  }
  posthog.capture(event, properties)
}
```

---

## Conclusion

This PostHog integration provides a comprehensive analytics foundation for the Nexpo template, supporting both web and mobile applications with privacy-first principles and enterprise-grade features. The implementation focuses on:

- **Developer Experience**: Easy-to-use hooks and components
- **Privacy Compliance**: GDPR-ready consent management
- **Performance**: Optimized for production use
- **Flexibility**: Extensible for custom use cases
- **Security**: Best practices for data protection

For additional support and advanced configurations, refer to the [PostHog documentation](https://posthog.com/docs) and consider the specific needs of your application domain.
