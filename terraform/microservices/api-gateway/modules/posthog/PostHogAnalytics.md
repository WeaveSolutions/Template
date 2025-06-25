# PostHog Analytics Module for API Gateway

This Terraform module integrates PostHog analytics with the Nexpo API Gateway, providing comprehensive tracking of API usage, authentication events, user behavior, and business metrics.

## Features

- **🔍 API Request Tracking**: Monitor all API calls with detailed metadata
- **🔐 Authentication Analytics**: Track social login providers and success rates
- **⚡ Performance Monitoring**: Response times, error rates, and throughput
- **👥 User Segmentation**: Cohorts for power users and premium subscribers
- **🚩 Feature Flags**: Dynamic control over analytics features
- **📊 Real-time Dashboards**: Pre-configured analytics dashboards
- **🔒 Privacy Compliant**: GDPR-ready with IP anonymization and consent management
- **🎯 Business Metrics**: Subscription tracking and revenue analytics
- **⚠️ Error Monitoring**: Comprehensive error tracking and alerting

## Usage

### Basic Configuration

```hcl
module "posthog_analytics" {
  source = "./modules/posthog"
  
  enable_posthog          = true
  project_name           = "nexpo"
  environment            = "prod"
  posthog_organization_id = "your-org-id"
  
  # Privacy settings
  anonymize_ips       = true
  data_retention_days = 365
  
  # Performance settings
  sampling_rate    = 1.0
  batch_size      = 50
  flush_interval_ms = 5000
  
  common_tags = {
    Project     = "nexpo"
    Environment = "production"
    Team        = "platform"
  }
}
```

### Advanced Configuration

```hcl
module "posthog_analytics" {
  source = "./modules/posthog"
  
  enable_posthog = true
  project_name   = "nexpo"
  environment    = "prod"
  
  # Analytics features
  enable_api_analytics          = true
  enable_detailed_logging       = false  # Only for debugging
  enable_error_tracking         = true
  enable_performance_monitoring = true
  enable_business_metrics       = true
  
  # Social login tracking
  track_social_login_providers     = true
  social_login_conversion_tracking = true
  
  # Privacy and compliance
  anonymize_ips         = true
  track_geolocation     = false
  enable_session_recording = false
  
  # Performance optimization
  analytics_rollout_percentage = 100
  sampling_rate               = 0.1  # 10% sampling for high-volume APIs
  
  # Webhook for real-time alerts
  webhook_endpoint = "https://api.nexpo.com/webhooks/posthog"
  webhook_secret   = var.webhook_secret
  
  common_tags = {
    Project     = "nexpo"
    Environment = "production"
    Team        = "platform"
    CostCenter  = "engineering"
  }
}
```

## Outputs

The module provides several outputs for integration with other services:

```hcl
# Use in other modules
module "auth_service" {
  source = "../auth"
  
  posthog_config = module.posthog_analytics.shared_posthog_config
}

# Environment variables for application
resource "aws_ssm_parameter" "posthog_env" {
  for_each = module.posthog_analytics.environment_variables
  
  name  = "/nexpo/api-gateway/${each.key}"
  type  = each.key == "POSTHOG_PROJECT_API_KEY" ? "SecureString" : "String"
  value = each.value
}
```

## Analytics Events Schema

### API Request Event
```json
{
  "event": "api_request",
  "properties": {
    "endpoint": "/api/v1/users",
    "method": "GET",
    "status_code": 200,
    "response_time": 45,
    "user_agent": "Mozilla/5.0...",
    "ip_address": "192.168.1.xxx", // anonymized if enabled
    "user_id": "cra_user_123",
    "session_id": "sess_abc123",
    "request_size": 1024,
    "response_size": 2048
  }
}
```

### Authentication Event
```json
{
  "event": "auth_attempt",
  "properties": {
    "provider": "google",
    "success": true,
    "user_id": "cra_user_123",
    "session_id": "sess_abc123",
    "ip_address": "192.168.1.xxx",
    "user_agent": "Mozilla/5.0..."
  }
}
```

### Error Event
```json
{
  "event": "api_error",
  "properties": {
    "error_code": "VALIDATION_ERROR",
    "error_message": "Invalid email format",
    "endpoint": "/api/v1/auth/login",
    "method": "POST",
    "user_id": "cra_user_123",
    "session_id": "sess_abc123"
  }
}
```

## Feature Flags

The module creates several feature flags for dynamic control:

| Flag Key | Description | Default |
|----------|-------------|---------|
| `api-analytics-enabled` | Enable/disable API analytics | Enabled |
| `detailed-api-logging` | Detailed request/response logging | Admin/Developer only |
| `rate-limit-analytics` | Track rate limiting events | Premium/Enterprise only |

## User Cohorts

Pre-configured cohorts for user segmentation:

- **API Power Users**: Users with >100 API calls
- **Premium Subscribers**: Users with premium/enterprise plans

## Environment Variables

The module generates environment variables for application integration:

```bash
# Required
POSTHOG_PROJECT_API_KEY=phc_xxxxxxxxxxxxxxxxxxxxx
POSTHOG_HOST=https://app.posthog.com
POSTHOG_PROJECT_ID=12345

# Feature toggles
POSTHOG_FEATURE_FLAGS_ENABLED=true
POSTHOG_API_ANALYTICS=true
POSTHOG_ERROR_TRACKING=true

# Privacy settings
POSTHOG_ANONYMIZE_IPS=true
POSTHOG_SESSION_RECORDING=false

# Performance settings
POSTHOG_SAMPLING_RATE=1.0
POSTHOG_BATCH_SIZE=50
POSTHOG_FLUSH_INTERVAL=5000
```

## Security Considerations

1. **API Key Management**: Store PostHog API keys in secure parameter stores
2. **IP Anonymization**: Enabled by default for GDPR compliance
3. **Data Retention**: Configurable retention periods
4. **Webhook Security**: Use secret tokens for webhook validation
5. **Sampling**: Use sampling for high-volume APIs to control costs

## Integration with Other Microservices

### Auth Service Integration
```hcl
module "auth_posthog" {
  source = "../shared/posthog"
  
  service_name = "auth"
  parent_project_id = module.api_gateway_posthog.posthog_project_id
  
  # Inherit settings from API Gateway
  posthog_config = module.api_gateway_posthog.shared_posthog_config
}
```

### Payments Service Integration
```hcl
module "payments_posthog" {
  source = "../shared/posthog"
  
  service_name = "payments"
  parent_project_id = module.api_gateway_posthog.posthog_project_id
  
  # Enable revenue tracking for payments
  enable_revenue_tracking = true
  track_subscription_events = true
}
```

## Monitoring and Alerting

The module includes webhook configuration for real-time alerts:

```hcl
# Configure alerts for critical events
webhook_endpoint = "https://api.nexpo.com/webhooks/posthog"
webhook_secret   = var.webhook_secret

# Events sent to webhook:
# - api_error (for immediate error response)
# - rate_limit_exceeded (for scaling alerts)
# - auth_attempt (for security monitoring)
```

## Cost Optimization

For high-volume APIs, consider:

1. **Sampling**: Reduce `sampling_rate` to control event volume
2. **Selective Tracking**: Disable detailed logging in production
3. **Batch Configuration**: Optimize `batch_size` and `flush_interval_ms`
4. **Data Retention**: Set appropriate `data_retention_days`

## Requirements

| Name | Version |
|------|---------|
| terraform | >= 1.0 |
| posthog | ~> 0.1 |

## Providers

| Name | Version |
|------|---------|
| posthog | ~> 0.1 |
| aws | ~> 5.0 (if using AWS) |

## Variables

See [variables.tf](./variables.tf) for all available configuration options.

## Outputs

See [outputs.tf](./outputs.tf) for all available outputs.

## Contributing

When extending this module:

1. Follow the existing naming conventions
2. Add comprehensive variable validation
3. Include appropriate tags for all resources
4. Update documentation and examples
5. Test with different cloud providers

## License

This module is part of the Nexpo template and follows the same licensing terms.
