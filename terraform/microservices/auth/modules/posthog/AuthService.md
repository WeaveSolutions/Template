# Auth Service PostHog Analytics Module

This module provides comprehensive PostHog analytics integration for the Nexpo Auth service, including authentication flow tracking, social login analytics, security monitoring, and user onboarding funnel analysis.

## Features

### Analytics Capabilities
- **Authentication Flow Tracking**: Login attempts, signup funnel, password resets
- **Social Login Analytics**: Provider-specific tracking (Google, Apple, Facebook, Microsoft, GitHub)
- **Security Monitoring**: Failed login attempts, MFA events, security incidents
- **User Onboarding**: Signup conversion tracking and verification flows
- **Session Management**: Session creation, refresh, and termination tracking

### PostHog Resources Created
- Dedicated PostHog project for auth service
- Feature flags for controlled analytics rollout
- User cohorts for authentication-based segmentation
- Custom actions for key authentication events
- Analytics dashboard for auth metrics monitoring
- Webhook integration for real-time event processing

## Usage

### Basic Configuration

```hcl
module "auth_posthog" {
  source = "./modules/posthog"
  
  # Base configuration
  enable_posthog              = true
  project_name               = "nexpo"
  environment                = "prod"
  posthog_organization_id    = "your-posthog-org-id"
  
  # Auth-specific features
  enable_social_login_analytics = true
  enable_security_monitoring    = true
  enable_onboarding_analytics   = true
  
  # Social login provider tracking
  track_google_login    = true
  track_apple_login     = true
  track_facebook_login  = true
  track_microsoft_login = true
  track_github_login    = true
  
  # Security features
  track_failed_logins  = true
  track_mfa_events     = true
  track_session_events = true
  
  common_tags = {
    Environment = "prod"
    Service     = "auth"
    Team        = "platform"
  }
}
```

### Advanced Configuration

```hcl
module "auth_posthog" {
  source = "./modules/posthog"
  
  # Base configuration
  enable_posthog              = true
  project_name               = "nexpo"
  environment                = "prod"
  posthog_organization_id    = "your-posthog-org-id"
  parent_project_id          = var.api_gateway_posthog_project_id
  
  # Analytics rollout
  analytics_rollout_percentage = 100
  sampling_rate               = 1.0
  social_login_analytics_rollout = 100
  
  # Privacy and compliance
  data_retention_days = 365
  
  # Performance tuning
  batch_size         = 50
  flush_interval_ms  = 5000
  
  # Feature toggles
  enable_console_logs       = false
  enable_debug_logging      = false
  enable_performance_monitoring = true
  enable_error_tracking     = true
  
  # Integration settings
  integrate_with_api_gateway = true
  enable_auth0_integration   = false
  
  # Webhook configuration
  webhook_endpoint = "https://api.nexpo.com/webhooks/posthog/auth"
  webhook_secret   = var.auth_webhook_secret
  
  common_tags = var.common_tags
}
```

## Outputs

### Key Outputs

- `posthog_project_id`: PostHog project ID for auth service
- `posthog_project_api_key`: API key for auth service (sensitive)
- `auth_feature_flags`: Auth-specific feature flags
- `auth_analytics_actions`: Analytics actions for auth events
- `auth_cohorts`: User cohorts for auth segmentation
- `dashboard_url`: URL to auth analytics dashboard
- `environment_variables`: Environment variables for auth service integration

### Example Output Usage

```hcl
# Use outputs in auth service deployment
resource "kubernetes_secret" "auth_posthog_config" {
  metadata {
    name = "auth-posthog-config"
  }
  
  data = module.auth_posthog.environment_variables
}

# Reference dashboard URL in documentation
output "auth_dashboard_url" {
  description = "Auth analytics dashboard"
  value       = module.auth_posthog.dashboard_url
}
```

## Event Schema

### Authentication Events

#### Login Attempt
```json
{
  "event_name": "auth_login_attempt",
  "properties": {
    "provider": "google|apple|facebook|password|etc",
    "success": true,
    "user_id": "user-root-id",
    "session_id": "session-id",
    "ip_address": "anonymized-ip",
    "user_agent": "user-agent-string",
    "login_method": "social|password|sso",
    "error_code": "error-code-if-failed"
  }
}
```

#### Signup Flow
```json
{
  "event_name": "auth_signup_started",
  "properties": {
    "provider": "google|apple|email",
    "signup_method": "social|email",
    "referrer": "referrer-url",
    "utm_source": "utm-source",
    "utm_medium": "utm-medium",
    "utm_campaign": "utm-campaign"
  }
}
```

#### Social Login
```json
{
  "event_name": "auth_social_login",
  "properties": {
    "provider": "google|apple|facebook|microsoft|github",
    "success": true,
    "new_user": false,
    "user_id": "user-root-id",
    "permissions": ["email", "profile"],
    "error_code": "error-code-if-failed"
  }
}
```

### Security Events

#### MFA Event
```json
{
  "event_name": "auth_mfa_event",
  "properties": {
    "mfa_type": "totp|sms|email|backup_codes",
    "action": "setup|verify|disable|backup_used",
    "success": true,
    "user_id": "user-root-id",
    "device_id": "device-id"
  }
}
```

#### Security Event
```json
{
  "event_name": "auth_security_event",
  "properties": {
    "event_type": "suspicious_login|account_locked|brute_force",
    "severity": "low|medium|high|critical",
    "user_id": "user-root-id",
    "details": {},
    "action_taken": "blocked|flagged|logged"
  }
}
```

## Feature Flags

### Available Feature Flags

1. **social-login-analytics** (`social_login_analytics`)
   - Controls social login provider analytics
   - Default: Enabled for all users
   - Rollout: Configurable percentage

2. **security-monitoring** (`security_monitoring`)
   - Controls enhanced security monitoring
   - Default: Enabled for all users
   - Includes failed login tracking and threat detection

3. **onboarding-analytics** (`onboarding_analytics`)
   - Controls user onboarding funnel tracking
   - Default: Enabled for all users
   - Tracks signup conversion and verification flows

### Feature Flag Usage in Application

```javascript
// Check feature flag in auth service
const isSecurityMonitoringEnabled = await posthog.isFeatureEnabled('security-monitoring');

if (isSecurityMonitoringEnabled) {
  // Track security events
  posthog.capture('auth_security_event', {
    event_type: 'suspicious_login',
    severity: 'medium',
    user_id: user.root_id
  });
}
```

## User Cohorts

### Auth-Specific Cohorts

1. **social_login_users**: Users who use OAuth providers
2. **password_users**: Users who use password authentication
3. **mfa_enabled_users**: Users with multi-factor authentication enabled
4. **new_signups**: Users who signed up in the last 30 days

### Cohort Criteria

```hcl
# Social login users cohort
resource "posthog_cohort" "social_login_users" {
  name = "Social Login Users - ${var.environment}"
  description = "Users who primarily use social login providers"
  
  filters = {
    events = [
      {
        event = "auth_social_login"
        properties = [
          {
            key = "success"
            value = true
            operator = "exact"
          }
        ]
      }
    ]
  }
}
```

## Integration with Applications

### Environment Variables

The module generates environment variables for easy integration:

```bash
# PostHog Configuration
POSTHOG_PROJECT_API_KEY=phc_auth_service_key
POSTHOG_HOST=https://app.posthog.com
POSTHOG_PROJECT_ID=auth-service-project-id
POSTHOG_SERVICE_NAME=auth

# Feature Flags
POSTHOG_FEATURE_FLAGS_ENABLED=true
POSTHOG_SOCIAL_LOGIN_ANALYTICS=true
POSTHOG_SECURITY_MONITORING=true
POSTHOG_ONBOARDING_ANALYTICS=true

# Privacy Settings
POSTHOG_ANONYMIZE_IPS=true
POSTHOG_DATA_RETENTION_DAYS=365

# Performance Settings
POSTHOG_SAMPLING_RATE=1.0
POSTHOG_BATCH_SIZE=50
POSTHOG_FLUSH_INTERVAL=5000

# Integration
POSTHOG_PARENT_PROJECT_ID=api-gateway-project-id
```

### Node.js Integration Example

```javascript
const PostHog = require('posthog-node');

const posthog = new PostHog(process.env.POSTHOG_PROJECT_API_KEY, {
  host: process.env.POSTHOG_HOST,
  flushAt: parseInt(process.env.POSTHOG_BATCH_SIZE),
  flushInterval: parseInt(process.env.POSTHOG_FLUSH_INTERVAL)
});

// Track login attempt
function trackLoginAttempt(userId, provider, success, errorCode) {
  const isEnabled = process.env.POSTHOG_SOCIAL_LOGIN_ANALYTICS === 'true';
  
  if (isEnabled) {
    posthog.capture({
      distinctId: userId,
      event: 'auth_login_attempt',
      properties: {
        provider,
        success,
        error_code: errorCode,
        login_method: provider === 'password' ? 'password' : 'social'
      }
    });
  }
}
```

## Security and Privacy

### Data Protection

- **IP Anonymization**: Always enabled for auth service
- **Sensitive Data**: No passwords or tokens tracked
- **User Consent**: Respects user privacy preferences
- **Data Retention**: Configurable retention periods

### Compliance Features

- **GDPR**: Built-in data deletion and export capabilities
- **SOC 2**: Audit-ready configuration and access controls
- **Security**: Encrypted data transmission and storage

## Monitoring and Alerting

### Dashboard Widgets

The auth analytics dashboard includes:

1. **Login Success Rate**: Trending login success/failure rates
2. **Social Login Distribution**: Breakdown by OAuth provider
3. **Signup Conversion Funnel**: Registration flow analytics
4. **Security Events**: Failed login attempts and security incidents
5. **MFA Adoption**: Multi-factor authentication usage
6. **Session Analytics**: Session duration and activity patterns

### Webhook Integration

Configure webhooks for real-time alerting:

```hcl
# Webhook for security events
webhook_endpoint = "https://api.nexpo.com/webhooks/posthog/auth"
webhook_secret   = var.auth_webhook_secret
```

## Troubleshooting

### Common Issues

1. **Events Not Appearing**
   - Verify `enable_posthog = true`
   - Check API key configuration
   - Ensure feature flags are enabled

2. **Feature Flags Not Working**
   - Verify user identification
   - Check flag configuration in PostHog dashboard
   - Ensure proper async handling

3. **Dashboard Not Accessible**
   - Check PostHog project permissions
   - Verify organization access
   - Confirm dashboard creation in Terraform state

### Debug Mode

Enable debug logging for troubleshooting:

```hcl
module "auth_posthog" {
  # ... other configuration
  enable_debug_logging = true
  enable_console_logs  = true
}
```

## Cost Optimization

### Event Volume Management

- Configure appropriate sampling rates
- Use batch processing for high-volume events
- Set up data retention policies
- Monitor PostHog usage and costs

### Performance Tuning

```hcl
# Optimize for high-volume auth service
module "auth_posthog" {
  # ... other configuration
  
  # Increase batch size for better performance
  batch_size = 100
  
  # Reduce flush frequency for cost savings
  flush_interval_ms = 10000
  
  # Sample high-volume events if needed
  sampling_rate = 0.1  # 10% sampling
}
```

## Version Compatibility

- **Terraform**: >= 0.14
- **PostHog Provider**: >= 0.1.0
- **AWS Provider**: >= 3.0 (for SSM parameters)
- **PostHog**: Cloud or self-hosted >= 1.30.0

## Contributing

To extend this module:

1. Add new custom events to the event schema
2. Create additional feature flags for new analytics features
3. Define new cohorts for user segmentation
4. Update dashboard widgets for new metrics
5. Extend webhook configuration for additional integrations

## Support

For questions and support:
- Review the shared PostHog module documentation
- Check the example configurations in `/terraform/microservices/examples/`
- Refer to PostHog official documentation
- Contact the platform team for Nexpo-specific configurations
