# Shared PostHog Analytics Module

This module provides a standardized PostHog analytics integration for Nexpo microservices. It creates service-specific PostHog projects with common features, actions, cohorts, and dashboards while allowing for customization per service.

## Features

- **Service-Specific Projects**: Creates dedicated PostHog projects for each microservice
- **Cross-Service Integration**: Links to parent projects for unified analytics
- **Standard Feature Flags**: Common feature flags for analytics, debugging, and performance monitoring
- **Common Actions**: Predefined actions for requests, errors, and performance tracking
- **User Cohorts**: Standard user segments (active users)
- **Analytics Dashboard**: Service-specific analytics dashboard
- **Cloud Provider Integration**: Secure storage of API keys and configuration
- **Privacy Compliance**: GDPR-ready with IP anonymization and data retention controls
- **Customizable Events**: Support for service-specific custom events

## Usage

```hcl
module "service_posthog" {
  source = "../shared/posthog"
  
  enable_posthog = true
  service_name   = "auth"  # Name of your service
  project_name   = "nexpo"
  environment    = "prod"
  provider       = "aws"
  
  # PostHog configuration
  posthog_organization_id = "org_123456"
  parent_project_id      = "project_parent_123"
  posthog_host          = "https://app.posthog.com"
  
  # Analytics settings
  analytics_rollout_percentage = 100
  sampling_rate               = 1.0
  
  # Privacy settings
  anonymize_ips       = true
  data_retention_days = 365
  
  # Performance settings
  batch_size        = 50
  flush_interval_ms = 5000
  
  # Feature toggles
  enable_debug_logging        = true
  enable_performance_monitoring = true
  enable_error_tracking       = true
  
  # Custom events for your service
  custom_events = {
    service_specific_event = {
      name                = "Service Specific Event"
      description         = "Track service-specific business logic"
      required_properties = ["user_id", "action_type"]
      tags               = ["service", "business"]
    }
  }
  
  # Webhook configuration
  webhook_endpoint = "https://webhook.example.com/posthog"
  webhook_secret   = "secure_webhook_secret"
  
  common_tags = {
    Environment = "prod"
    Team       = "platform"
  }
}
```

## Outputs

| Name | Description | Type |
|------|-------------|------|
| `posthog_project_id` | PostHog project ID for the service | `string` |
| `posthog_project_api_key` | PostHog project API key (sensitive) | `string` |
| `posthog_host` | PostHog host URL | `string` |
| `service_name` | Name of the service | `string` |
| `feature_flags` | Map of feature flag keys | `map(string)` |
| `analytics_actions` | Configured analytics actions | `map(string)` |
| `custom_actions` | Custom analytics actions | `map(string)` |
| `cohorts` | User cohorts for the service | `map(string)` |
| `dashboard_url` | URL to the service analytics dashboard | `string` |
| `environment_variables` | Environment variables for app integration (sensitive) | `map(string)` |
| `shared_config` | Shared configuration for other services (sensitive) | `object` |
| `event_schema` | Standard event schema documentation | `object` |
| `integration_config` | Configuration for application integration (sensitive) | `object` |

## Standard Event Schema

### Service Request Event
```json
{
  "event": "{service_name}_request",
  "properties": {
    "endpoint": "string",
    "method": "string",
    "status_code": "number",
    "response_time": "number",
    "user_id": "string",
    "session_id": "string"
  }
}
```

### Service Error Event
```json
{
  "event": "{service_name}_error",
  "properties": {
    "error_type": "string",
    "error_message": "string",
    "endpoint": "string",
    "user_id": "string",
    "session_id": "string",
    "stack_trace": "string"
  }
}
```

### Service Performance Event
```json
{
  "event": "{service_name}_performance",
  "properties": {
    "operation": "string",
    "duration_ms": "number",
    "memory_usage": "number",
    "cpu_usage": "number",
    "user_id": "string"
  }
}
```

## Feature Flags

| Flag Key | Description | Default Rollout |
|----------|-------------|-----------------|
| `{service}-analytics-enabled` | Enable analytics for the service | 100% |
| `{service}-debug-logging` | Enable debug logging | Based on environment |
| `{service}-performance-monitoring` | Enable performance monitoring | 100% |

## User Cohorts

| Cohort Name | Description | Criteria |
|-------------|-------------|----------|
| `Active Users` | Users active in the service | Last activity within 7 days |

## Environment Variables

The module outputs environment variables that can be used in your application:

```bash
# Core PostHog configuration
POSTHOG_PROJECT_API_KEY=phc_xxx
POSTHOG_HOST=https://app.posthog.com
POSTHOG_PROJECT_ID=12345
POSTHOG_SERVICE_NAME=auth

# Feature flags
POSTHOG_FEATURE_FLAGS_ENABLED=true

# Privacy and performance
POSTHOG_ANONYMIZE_IPS=true
POSTHOG_SAMPLING_RATE=1.0
POSTHOG_BATCH_SIZE=50
POSTHOG_FLUSH_INTERVAL=5000

# Service features
POSTHOG_DEBUG_LOGGING=false
POSTHOG_PERFORMANCE_MONITORING=true
POSTHOG_ERROR_TRACKING=true
POSTHOG_USER_IDENTIFICATION=true

# Cross-service integration
POSTHOG_PARENT_PROJECT_ID=parent_123
POSTHOG_CROSS_SERVICE_TRACKING=true
```

## Variables

### Required Variables

| Name | Description | Type |
|------|-------------|------|
| `service_name` | Name of the microservice | `string` |

### Optional Variables

| Name | Description | Type | Default |
|------|-------------|------|---------|
| `enable_posthog` | Enable PostHog analytics integration | `bool` | `false` |
| `project_name` | Name of the main project | `string` | `"nexpo"` |
| `environment` | Environment (dev, staging, prod) | `string` | - |
| `provider` | Cloud provider (aws, gcp, azure) | `string` | `"aws"` |
| `posthog_organization_id` | PostHog organization ID | `string` | `""` |
| `posthog_host` | PostHog host URL | `string` | `"https://app.posthog.com"` |
| `parent_project_id` | Parent PostHog project ID | `string` | `""` |
| `analytics_rollout_percentage` | Percentage of users to enable analytics for | `number` | `100` |
| `sampling_rate` | Sampling rate for analytics events (0.0 to 1.0) | `number` | `1.0` |
| `anonymize_ips` | Anonymize IP addresses for privacy | `bool` | `true` |
| `data_retention_days` | Number of days to retain analytics data | `number` | `365` |
| `batch_size` | Number of events to batch before sending | `number` | `50` |
| `flush_interval_ms` | Interval in milliseconds to flush events | `number` | `5000` |
| `enable_session_recording` | Enable session recording | `bool` | `false` |
| `enable_console_logs` | Enable console log capture | `bool` | `false` |
| `enable_debug_logging` | Enable debug logging | `bool` | `false` |
| `enable_performance_monitoring` | Enable performance monitoring | `bool` | `true` |
| `enable_error_tracking` | Enable error tracking | `bool` | `true` |
| `enable_user_identification` | Enable user identification | `bool` | `true` |
| `enable_cross_service_tracking` | Enable cross-service tracking | `bool` | `true` |
| `enable_business_metrics` | Enable business metric tracking | `bool` | `false` |
| `webhook_endpoint` | Webhook endpoint for real-time events | `string` | `""` |
| `webhook_secret` | Secret token for webhook security | `string` | `""` |
| `custom_events` | Custom events to track for this service | `map(object)` | `{}` |

## Cloud Provider Support

### AWS
- Stores configuration in SSM Parameter Store
- Parameters: `/nexpo/{service_name}/posthog/{config_key}`

### Google Cloud Platform
- Stores configuration in Secret Manager
- Secrets: `nexpo-{service_name}-posthog-{config_key}`

### Azure
- Stores configuration in Key Vault
- Secrets: `nexpo-{service_name}-posthog-{config_key}`

## Security Considerations

1. **API Keys**: PostHog API keys are stored securely in cloud provider secret management services
2. **IP Anonymization**: Enabled by default for GDPR compliance
3. **Webhook Security**: Webhooks use secret tokens for authentication
4. **Data Retention**: Configurable data retention periods
5. **Access Control**: Feature flags control access to sensitive analytics features

## Integration Examples

### Node.js/Express Integration

```javascript
const { PostHog } = require('posthog-node');

const posthog = new PostHog(process.env.POSTHOG_PROJECT_API_KEY, {
  host: process.env.POSTHOG_HOST,
  flushAt: parseInt(process.env.POSTHOG_BATCH_SIZE),
  flushInterval: parseInt(process.env.POSTHOG_FLUSH_INTERVAL),
});

// Track service request
app.use((req, res, next) => {
  const startTime = Date.now();
  
  res.on('finish', () => {
    posthog.capture({
      distinctId: req.user?.id || 'anonymous',
      event: `${process.env.POSTHOG_SERVICE_NAME}_request`,
      properties: {
        endpoint: req.path,
        method: req.method,
        status_code: res.statusCode,
        response_time: Date.now() - startTime,
        user_id: req.user?.root_id,
        session_id: req.sessionID,
      },
    });
  });
  
  next();
});

// Track service errors
app.use((err, req, res, next) => {
  posthog.capture({
    distinctId: req.user?.id || 'anonymous',
    event: `${process.env.POSTHOG_SERVICE_NAME}_error`,
    properties: {
      error_type: err.name,
      error_message: err.message,
      endpoint: req.path,
      user_id: req.user?.root_id,
      session_id: req.sessionID,
      stack_trace: process.env.POSTHOG_DEBUG_LOGGING === 'true' ? err.stack : undefined,
    },
  });
  
  next(err);
});
```

### Feature Flag Usage

```javascript
// Check if analytics is enabled for user
const analyticsEnabled = await posthog.isFeatureEnabled(
  'service-analytics-enabled',
  req.user?.root_id
);

if (analyticsEnabled) {
  // Track detailed analytics
}

// Check if debug logging is enabled
const debugEnabled = await posthog.isFeatureEnabled(
  'service-debug-logging',
  req.user?.root_id
);
```

## Monitoring and Alerting

The module creates webhooks for critical events that can be used with monitoring systems:

- Service errors
- Performance degradation
- High error rates
- Service-specific critical events

## Cost Optimization

1. **Sampling Rate**: Adjust `sampling_rate` to reduce event volume in high-traffic services
2. **Batch Size**: Increase `batch_size` to reduce API calls
3. **Data Retention**: Set appropriate `data_retention_days` to manage storage costs
4. **Feature Flags**: Use feature flags to control rollout and reduce initial costs

## Extending the Module

To add service-specific features:

1. **Custom Events**: Add events via the `custom_events` variable
2. **Feature Flags**: Create additional feature flags in your service module
3. **Cohorts**: Define service-specific user segments
4. **Actions**: Create actions for service-specific business events

## Support

For issues or questions:
1. Check the [PostHog documentation](https://posthog.com/docs)
2. Review the Terraform PostHog provider documentation
3. Contact the platform team for Nexpo-specific questions
