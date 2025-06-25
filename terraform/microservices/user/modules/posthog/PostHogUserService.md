# User Service PostHog Analytics Module

This module provides comprehensive PostHog analytics integration for the Nexpo User service, including user lifecycle tracking, profile analytics, engagement monitoring, and GDPR-compliant user data management.

## Features

### Analytics Capabilities
- **User Lifecycle Tracking**: Profile creation, updates, deactivation, and deletion
- **Profile Analytics**: Profile completion tracking and demographic insights
- **Engagement Monitoring**: User activity patterns and engagement scoring
- **Subscription Analytics**: Plan changes, upgrade/downgrade tracking
- **Churn Prediction**: Early warning signals and retention analytics
- **Consent Management**: GDPR-compliant consent tracking and data handling

### PostHog Resources Created
- Dedicated PostHog project for user service
- Feature flags for user experience optimization
- User cohorts for behavioral segmentation
- Custom actions for user lifecycle events
- Analytics dashboard for user metrics monitoring
- Webhook integration for real-time user event processing

## Usage

### Basic Configuration

```hcl
module "user_posthog" {
  source = "./modules/posthog"
  
  # Base configuration
  enable_posthog              = true
  project_name               = "nexpo"
  environment                = "prod"
  posthog_organization_id    = "your-posthog-org-id"
  
  # User-specific features
  enable_lifecycle_analytics = true
  enable_profile_analytics   = true
  enable_engagement_tracking = true
  enable_gdpr_compliance     = true
  
  # Lifecycle tracking
  track_profile_creation   = true
  track_profile_updates    = true
  track_profile_completion = true
  track_user_preferences   = true
  
  # Privacy and compliance
  enable_consent_tracking = true
  data_retention_days     = 365
  
  common_tags = {
    Environment = "prod"
    Service     = "user"
    Team        = "platform"
  }
}
```

### Advanced Configuration

```hcl
module "user_posthog" {
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
  lifecycle_analytics_rollout = 100
  profile_analytics_rollout   = 90
  
  # Privacy and compliance
  enable_gdpr_compliance = true
  consent_required      = true
  data_retention_days   = 730
  
  # Feature flags
  enable_churn_prediction      = true
  enable_subscription_analytics = true
  enable_engagement_scoring    = true
  
  # Performance tuning
  batch_size         = 75
  flush_interval_ms  = 4000
  
  # Integration settings
  integrate_with_api_gateway = true
  integrate_with_auth        = true
  integrate_with_payments    = true
  
  # Webhook configuration
  webhook_endpoint = "https://api.nexpo.com/webhooks/posthog/user"
  webhook_secret   = var.user_webhook_secret
  
  common_tags = var.common_tags
}
```

## Outputs

### Key Outputs

- `posthog_project_id`: PostHog project ID for user service
- `posthog_project_api_key`: API key for user service (sensitive)
- `user_feature_flags`: User-specific feature flags
- `user_analytics_actions`: Analytics actions for user events
- `user_cohorts`: User segmentation cohorts
- `dashboard_url`: URL to user analytics dashboard
- `environment_variables`: Environment variables for user service integration
- `user_segmentation_rules`: Rules for user cohort segmentation

### Example Output Usage

```hcl
# Use outputs in user service deployment
resource "kubernetes_secret" "user_posthog_config" {
  metadata {
    name = "user-posthog-config"
  }
  
  data = module.user_posthog.environment_variables
}

# Reference segmentation rules
output "user_cohorts_config" {
  description = "User segmentation configuration"
  value       = module.user_posthog.user_segmentation_rules
}
```

## Event Schema

### User Lifecycle Events

#### Profile Creation
```json
{
  "event_name": "user_profile_created",
  "properties": {
    "user_id": "user-root-id",
    "signup_method": "social|email|sso",
    "onboarding_source": "web|mobile|api",
    "initial_plan": "free|basic|premium",
    "referrer": "referrer-url",
    "utm_source": "utm-source",
    "utm_medium": "utm-medium",
    "utm_campaign": "utm-campaign",
    "profile_completion": 0.25
  }
}
```

#### Profile Update
```json
{
  "event_name": "user_profile_updated",
  "properties": {
    "user_id": "user-root-id",
    "fields_updated": ["name", "email", "preferences"],
    "profile_completion_before": 0.5,
    "profile_completion_after": 0.75,
    "update_trigger": "user_initiated|system|admin",
    "section": "personal_info|preferences|privacy"
  }
}
```

#### Profile Completion
```json
{
  "event_name": "user_profile_completed",
  "properties": {
    "user_id": "user-root-id",
    "completion_percentage": 100,
    "time_to_complete": 1800,
    "completed_sections": ["personal_info", "preferences", "avatar"],
    "completion_trigger": "guided|organic|reminder"
  }
}
```

### Engagement Events

#### User Activity
```json
{
  "event_name": "user_activity",
  "properties": {
    "user_id": "user-root-id",
    "activity_type": "login|feature_use|content_view|interaction",
    "session_id": "session-id",
    "activity_duration": 1200,
    "features_used": ["dashboard", "settings", "profile"],
    "engagement_score": 8.5
  }
}
```

#### Subscription Change
```json
{
  "event_name": "user_subscription_changed",
  "properties": {
    "user_id": "user-root-id",
    "from_plan": "free",
    "to_plan": "premium",
    "change_type": "upgrade|downgrade|cancellation|reactivation",
    "change_reason": "user_initiated|admin|system|payment_failure",
    "effective_date": "2024-01-15",
    "billing_cycle": "monthly|yearly"
  }
}
```

### Privacy and Consent Events

#### Consent Update
```json
{
  "event_name": "user_consent_updated",
  "properties": {
    "user_id": "user-root-id",
    "consent_type": "analytics|marketing|functional|necessary",
    "consent_given": true,
    "consent_source": "cookie_banner|privacy_settings|onboarding",
    "previous_consent": false,
    "gdpr_applicable": true
  }
}
```

#### Data Request
```json
{
  "event_name": "user_data_request",
  "properties": {
    "user_id": "user-root-id",
    "request_type": "export|deletion|rectification|portability",
    "request_status": "pending|processing|completed|failed",
    "request_source": "user_portal|admin|api",
    "completion_time": 3600,
    "gdpr_request": true
  }
}
```

## Feature Flags

### Available Feature Flags

1. **lifecycle-analytics** (`lifecycle_analytics`)
   - Controls user lifecycle event tracking
   - Default: Enabled for all users
   - Tracks profile creation, updates, and completion

2. **profile-analytics** (`profile_analytics`)
   - Controls profile completion and demographic analytics
   - Default: Enabled for 90% of users
   - Includes profile scoring and completion tracking

3. **engagement-tracking** (`engagement_tracking`)
   - Controls user engagement and activity monitoring
   - Default: Enabled for all users
   - Tracks session activity and engagement scoring

4. **churn-prediction** (`churn_prediction`)
   - Controls churn prediction algorithms
   - Default: Enabled for premium users
   - Includes early warning signals and retention scoring

5. **subscription-analytics** (`subscription_analytics`)
   - Controls subscription change tracking
   - Default: Enabled for all users
   - Tracks plan changes and billing events

### Feature Flag Usage in Application

```javascript
// Check feature flag in user service
const isChurnPredictionEnabled = await posthog.isFeatureEnabled('churn-prediction');

if (isChurnPredictionEnabled) {
  // Calculate and track churn risk
  const churnRisk = calculateChurnRisk(user);
  posthog.capture('user_churn_risk_calculated', {
    user_id: user.root_id,
    churn_risk_score: churnRisk,
    risk_factors: ['low_engagement', 'payment_issues']
  });
}
```

## User Cohorts

### User Segmentation Cohorts

1. **high_value_users**: Users with high engagement and spending
2. **at_risk_users**: Users showing churn indicators
3. **new_users**: Users registered in the last 30 days
4. **premium_subscribers**: Users with paid subscriptions
5. **complete_profiles**: Users with 100% profile completion
6. **gdpr_users**: Users subject to GDPR regulations

### Cohort Criteria Examples

```hcl
# High value users cohort
resource "posthog_cohort" "high_value_users" {
  name = "High Value Users - ${var.environment}"
  description = "Users with high engagement and subscription value"
  
  filters = {
    events = [
      {
        event = "user_subscription_changed"
        properties = [
          {
            key = "to_plan"
            value = ["premium", "enterprise"]
            operator = "in"
          }
        ]
      }
    ]
    properties = [
      {
        key = "engagement_score"
        value = 7.0
        operator = "gte"
      }
    ]
  }
}

# At-risk users cohort
resource "posthog_cohort" "at_risk_users" {
  name = "At Risk Users - ${var.environment}"
  description = "Users showing signs of potential churn"
  
  filters = {
    events = [
      {
        event = "user_activity"
        properties = [
          {
            key = "last_seen"
            value = "14d"
            operator = "gt"
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
POSTHOG_PROJECT_API_KEY=phc_user_service_key
POSTHOG_HOST=https://app.posthog.com
POSTHOG_PROJECT_ID=user-service-project-id
POSTHOG_SERVICE_NAME=user

# Feature Flags
POSTHOG_FEATURE_FLAGS_ENABLED=true
POSTHOG_LIFECYCLE_ANALYTICS=true
POSTHOG_PROFILE_ANALYTICS=true
POSTHOG_ENGAGEMENT_TRACKING=true
POSTHOG_CHURN_PREDICTION=true
POSTHOG_SUBSCRIPTION_ANALYTICS=true

# Privacy and Compliance
POSTHOG_GDPR_COMPLIANCE=true
POSTHOG_CONSENT_REQUIRED=true
POSTHOG_ANONYMIZE_IPS=true
POSTHOG_DATA_RETENTION_DAYS=365

# Performance Settings
POSTHOG_SAMPLING_RATE=1.0
POSTHOG_BATCH_SIZE=75
POSTHOG_FLUSH_INTERVAL=4000

# Cross-service Integration
POSTHOG_PARENT_PROJECT_ID=api-gateway-project-id
POSTHOG_AUTH_PROJECT_ID=auth-service-project-id
POSTHOG_PAYMENTS_PROJECT_ID=payments-service-project-id
```

### Node.js Integration Example

```javascript
const PostHog = require('posthog-node');

const posthog = new PostHog(process.env.POSTHOG_PROJECT_API_KEY, {
  host: process.env.POSTHOG_HOST,
  flushAt: parseInt(process.env.POSTHOG_BATCH_SIZE),
  flushInterval: parseInt(process.env.POSTHOG_FLUSH_INTERVAL)
});

// Track profile completion
function trackProfileCompletion(userId, completionData) {
  const isEnabled = process.env.POSTHOG_PROFILE_ANALYTICS === 'true';
  
  if (isEnabled) {
    posthog.capture({
      distinctId: userId,
      event: 'user_profile_completed',
      properties: {
        completion_percentage: completionData.percentage,
        time_to_complete: completionData.timeToComplete,
        completed_sections: completionData.sections,
        completion_trigger: completionData.trigger
      }
    });
  }
}

// Update user properties for segmentation
function updateUserProperties(userId, userProperties) {
  posthog.identify({
    distinctId: userId,
    properties: {
      subscription_plan: userProperties.plan,
      profile_completion: userProperties.profileCompletion,
      engagement_score: userProperties.engagementScore,
      is_gdpr_user: userProperties.isGdprUser,
      last_active: userProperties.lastActive
    }
  });
}
```

## Privacy and GDPR Compliance

### Data Protection Features

- **Consent Management**: Tracks and respects user consent preferences
- **Data Retention**: Configurable data retention periods
- **Right to Deletion**: Automated data deletion workflows
- **Data Export**: User data portability features
- **IP Anonymization**: Automatic IP address anonymization

### GDPR Request Handling

```javascript
// Handle GDPR deletion request
async function handleGdprDeletion(userId) {
  // Track the deletion request
  posthog.capture({
    distinctId: userId,
    event: 'user_data_request',
    properties: {
      request_type: 'deletion',
      request_status: 'processing',
      gdpr_request: true
    }
  });
  
  // Delete user data from PostHog
  await posthog.deleteUser(userId);
  
  // Track completion
  posthog.capture({
    distinctId: 'system',
    event: 'user_data_request',
    properties: {
      request_type: 'deletion',
      request_status: 'completed',
      user_id: userId,
      gdpr_request: true
    }
  });
}
```

## Churn Prediction and Retention

### Churn Risk Indicators

The module tracks various signals for churn prediction:

1. **Engagement Decline**: Reduced activity over time
2. **Feature Usage**: Decreased feature adoption
3. **Support Interactions**: Increased support tickets
4. **Billing Issues**: Payment failures or downgrades
5. **Profile Abandonment**: Incomplete profile setup

### Retention Analytics

```javascript
// Calculate and track engagement score
function calculateEngagementScore(userActivity) {
  const score = {
    login_frequency: userActivity.loginFrequency * 0.3,
    feature_usage: userActivity.featureUsage * 0.4,
    session_duration: userActivity.avgSessionDuration * 0.2,
    content_interaction: userActivity.contentInteraction * 0.1
  };
  
  const totalScore = Object.values(score).reduce((a, b) => a + b, 0);
  
  // Track engagement score
  posthog.capture({
    distinctId: userActivity.userId,
    event: 'user_engagement_scored',
    properties: {
      engagement_score: totalScore,
      score_breakdown: score,
      scoring_period: '30d'
    }
  });
  
  return totalScore;
}
```

## Dashboard and Monitoring

### User Analytics Dashboard

The user analytics dashboard includes:

1. **User Lifecycle Funnel**: Registration to activation flow
2. **Profile Completion Rates**: Profile completion analytics
3. **Engagement Trends**: User activity and engagement patterns
4. **Churn Risk Analysis**: At-risk user identification
5. **Subscription Analytics**: Plan changes and revenue impact
6. **GDPR Compliance**: Consent tracking and data requests

### Real-time Monitoring

```hcl
# Webhook for user events
webhook_endpoint = "https://api.nexpo.com/webhooks/posthog/user"
webhook_secret   = var.user_webhook_secret
```

### Alerting Configuration

Set up alerts for:
- High churn risk scores
- GDPR compliance violations
- Unusual user activity patterns
- Profile completion rate drops

## Cost Optimization

### Efficient Event Tracking

```javascript
// Batch user property updates
class UserPropertyBatcher {
  constructor() {
    this.batch = new Map();
    this.flushInterval = setInterval(() => this.flush(), 5000);
  }
  
  addUpdate(userId, properties) {
    const existing = this.batch.get(userId) || {};
    this.batch.set(userId, { ...existing, ...properties });
  }
  
  flush() {
    for (const [userId, properties] of this.batch) {
      posthog.identify({
        distinctId: userId,
        properties
      });
    }
    this.batch.clear();
  }
}
```

### Sampling Strategies

```hcl
# Different sampling rates for different event types
module "user_posthog" {
  # High-value events: 100% sampling
  lifecycle_analytics_sampling = 1.0
  
  # Medium-value events: 50% sampling  
  engagement_tracking_sampling = 0.5
  
  # High-volume events: 10% sampling
  activity_tracking_sampling = 0.1
}
```

## Version Compatibility

- **Terraform**: >= 0.14
- **PostHog Provider**: >= 0.1.0
- **AWS Provider**: >= 3.0 (for SSM parameters)
- **PostHog**: Cloud or self-hosted >= 1.30.0

## Troubleshooting

### Common Issues

1. **User Properties Not Updating**
   - Verify user identification is consistent
   - Check batch processing configuration
   - Ensure proper async handling

2. **Cohorts Not Populating**
   - Verify event properties match cohort criteria
   - Check user property updates
   - Confirm cohort calculation frequency

3. **GDPR Compliance Issues**
   - Verify consent tracking implementation
   - Check data retention configuration
   - Ensure proper deletion workflows

### Debug Mode

```hcl
module "user_posthog" {
  # ... other configuration
  enable_debug_logging = true
  enable_console_logs  = true
  enable_performance_monitoring = true
}
```

## Contributing

To extend this module:

1. Add new user lifecycle events
2. Create additional behavioral cohorts
3. Implement new churn prediction models
4. Extend GDPR compliance features
5. Add new dashboard widgets for user insights

## Support

For questions and support:
- Review the shared PostHog module documentation
- Check the example configurations in `/terraform/microservices/examples/`
- Refer to PostHog official documentation for GDPR compliance
- Contact the platform team for user analytics configurations
