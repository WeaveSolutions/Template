# Notifications Service Infrastructure

This directory contains the Terraform configuration for deploying the Notifications Service, which handles multi-channel communication including email, SMS, push notifications, and in-app messaging for the Nexpo application.

## 📬 Service Overview

The Notifications Service provides:
- **Email Notifications**: Brevo (Sendinblue) integration for transactional emails
- **SMS Messaging**: Twilio integration for SMS delivery
- **Push Notifications**: Firebase Cloud Messaging (FCM) and Apple Push Notification Service (APNS)
- **In-App Notifications**: Real-time notifications via WebSocket
- **Multi-Channel Campaigns**: Coordinated messaging across channels
- **Template Management**: Dynamic email and message templates
- **Delivery Tracking**: Comprehensive delivery and engagement analytics

## 🏗️ Architecture

```
┌─────────────────┐     ┌──────────────┐     ┌─────────────────┐
│  Mobile/Web App │────▶│  API Gateway │────▶│ Notifications   │
└─────────────────┘     └──────────────┘     │    Service      │
                                              └─────────────────┘
                                                       │
                        ┌──────────────────────────────┼──────────────────────────────┐
                        │                              │                              │
                        ▼                              ▼                              ▼
                ┌──────────────┐              ┌──────────────┐              ┌──────────────┐
                │    Brevo     │              │    Twilio    │              │ FCM / APNS   │
                │   (Email)    │              │    (SMS)     │              │   (Push)     │
                └──────────────┘              └──────────────┘              └──────────────┘
                        │                              │                              │
                        └──────────────────────────────┼──────────────────────────────┘
                                                       │
                                                       ▼
                                               ┌──────────────┐
                                               │ Message Queue│
                                               │ & Database   │
                                               └──────────────┘
```

## 📁 Directory Structure

```
notifications/
├── main.tf                    # Main service configuration
├── variables.tf               # Service variables
├── outputs.tf                # Service outputs
├── provider-specific/        # Cloud-specific configs
│   ├── aws.tf               # ECS, SQS, SNS, SES
│   ├── gcp.tf               # Cloud Run, Pub/Sub, Cloud Tasks
│   ├── azure.tf             # Container Apps, Service Bus
│   ├── oci.tf               # Container Instances, Streaming
│   ├── ibm.tf               # Code Engine, Event Streams
│   └── cloudflare.tf        # Workers, Queues, Email Routing
├── channels/                 # Communication channel configs
│   ├── email.tf             # Brevo email configuration
│   ├── sms.tf               # Twilio SMS setup
│   ├── push.tf              # FCM/APNS push notifications
│   ├── webhook.tf           # Webhook delivery
│   └── in-app.tf            # WebSocket in-app notifications
├── templates/                # Message template configurations
│   ├── email-templates.tf   # Email template management
│   ├── sms-templates.tf     # SMS template management
│   ├── push-templates.tf    # Push notification templates
│   └── template-engine.tf   # Dynamic template rendering
├── queues/                   # Message queue configurations
│   ├── delivery-queue.tf    # Message delivery queue
│   ├── retry-queue.tf       # Failed message retry queue
│   ├── priority-queue.tf    # High-priority message queue
│   └── dead-letter.tf       # Dead letter queue for failures
├── analytics/                # Delivery analytics
│   ├── tracking.tf          # Delivery tracking setup
│   ├── engagement.tf        # Engagement metrics
│   └── reporting.tf         # Analytics reporting
├── monitoring/               # Observability configurations
│   ├── dashboards.tf        # Service dashboards
│   ├── alerts.tf            # Delivery failure alerts
│   └── logs.tf              # Message delivery logging
└── README.md                # This file
```

## 🚀 Deployment

### Prerequisites
- Terraform >= 1.0
- Cloud provider CLI configured
- Brevo API key for email
- Twilio credentials for SMS
- Firebase project for push notifications
- Apple Developer account for APNS

### Quick Start
```bash
# Navigate to notifications service
cd terraform/microservices/notifications

# Initialize Terraform
terraform init

# Plan deployment
terraform plan -var-file="../../terraform.tfvars"

# Deploy service
terraform apply -var-file="../../terraform.tfvars"
```

### Environment-Specific Deployment
```bash
# Development (limited channels)
terraform apply -var="environment=dev" -var="enable_sms=false"

# Production (all channels)
terraform apply -var="environment=prod" -var="enable_all_channels=true"
```

## 🔧 Configuration

### Required Variables
```hcl
# Service Configuration
service_name     = "notifications-service"
environment      = "dev"
instance_count   = 2
cpu_units        = 512
memory_mb        = 1024

# Notification Service Provider Feature Flags
ENABLE_BREVO_EMAIL=true
ENABLE_TWILIO_SMS=true
ENABLE_FIREBASE_PUSH=true
ENABLE_APNS_PUSH=true
ENABLE_WEBHOOK_NOTIFICATIONS=true
ENABLE_IN_APP_NOTIFICATIONS=true

# Channel Configuration
enable_email     = true
enable_sms       = true
enable_push      = true
enable_webhook   = true
enable_in_app    = true

# Email Configuration (Brevo)
BREVO_API_KEY=your-brevo-key
brevo_sender_email = "noreply@example.com"
brevo_sender_name  = "Your App"

# SMS Configuration (Twilio)
TWILIO_ACCOUNT_SID=your-twilio-sid
TWILIO_AUTH_TOKEN=your-twilio-token
twilio_phone_number = "+1234567890"

# Push Notification Configuration
FIREBASE_SERVER_KEY=your-firebase-key
firebase_project_id = "your-firebase-project"
apns_key_id        = var.apns_key_id
apns_team_id       = var.apns_team_id
```

### Message Queue Configuration
```hcl
# Queue settings
message_queue_config = {
  delivery_queue = {
    visibility_timeout = 300
    message_retention  = 1209600  # 14 days
    max_receive_count  = 3
  }
  
  retry_queue = {
    visibility_timeout = 600
    message_retention  = 1209600
    max_receive_count  = 5
  }
  
  dead_letter_queue = {
    message_retention = 1209600
  }
}

# Rate limiting
rate_limits = {
  email_per_minute = 100
  sms_per_minute   = 50
  push_per_minute  = 1000
}
```

## 📧 Channel Configurations

### Email Configuration (Brevo)
```python
# Brevo email service configuration
brevo_config = {
    "api_key": "${var.BREVO_API_KEY}",
    "api_url": "https://api.brevo.com/v3",
    "sender": {
        "email": "noreply@example.com",
        "name": "Your App"
    },
    "templates": {
        "welcome": 1,
        "password_reset": 2,
        "order_confirmation": 3,
        "weekly_digest": 4
    },
    "lists": {
        "newsletter": 1,
        "product_updates": 2,
        "marketing": 3
    }
}
```

### SMS Configuration (Twilio)
```python
# Twilio SMS service configuration
twilio_config = {
    "account_sid": "${var.TWILIO_ACCOUNT_SID}",
    "auth_token": "${var.TWILIO_AUTH_TOKEN}",
    "phone_number": "+1234567890",
    "messaging_service_sid": "${var.TWILIO_MESSAGING_SERVICE_SID}",
    "webhook_url": "https://api.example.com/webhooks/twilio",
    "templates": {
        "verification": "Your verification code is: {{code}}",
        "alert": "Alert: {{message}}",
        "reminder": "Reminder: {{event}} at {{time}}"
    }
}
```

### Push Notification Configuration
```python
# Firebase Cloud Messaging configuration
fcm_config = {
    "project_id": "${var.firebase_project_id}",
    "server_key": "${var.FIREBASE_SERVER_KEY}",
    "sender_id": "${var.firebase_sender_id}",
    "templates": {
        "default": {
            "title": "{{title}}",
            "body": "{{message}}",
            "icon": "notification_icon",
            "sound": "default"
        },
        "urgent": {
            "title": "🚨 {{title}}",
            "body": "{{message}}",
            "priority": "high",
            "sound": "urgent_sound"
        }
    }
}

# Apple Push Notification Service configuration
apns_config = {
    "key_id": "${var.apns_key_id}",
    "team_id": "${var.apns_team_id}",
    "bundle_id": "com.example.app",
    "environment": "production",  # or "sandbox"
    "templates": {
        "default": {
            "aps": {
                "alert": {
                    "title": "{{title}}",
                    "body": "{{message}}"
                },
                "sound": "default",
                "badge": 1
            }
        }
    }
}
```

## 🔌 API Endpoints

### Send Notification APIs
```
POST   /api/notifications/send           # Send single notification
POST   /api/notifications/send/bulk      # Send bulk notifications
POST   /api/notifications/send/scheduled # Schedule notification
POST   /api/notifications/send/campaign  # Send multi-channel campaign
```

### Template Management APIs
```
GET    /api/notifications/templates      # List all templates
POST   /api/notifications/templates      # Create new template
PUT    /api/notifications/templates/:id  # Update template
DELETE /api/notifications/templates/:id  # Delete template
POST   /api/notifications/templates/test # Test template rendering
```

### Delivery Tracking APIs
```
GET    /api/notifications/status/:id     # Get delivery status
GET    /api/notifications/history        # Get delivery history
GET    /api/notifications/analytics      # Get delivery analytics
GET    /api/notifications/engagement     # Get engagement metrics
```

### Subscription Management APIs
```
POST   /api/notifications/subscribe      # Subscribe to notifications
DELETE /api/notifications/unsubscribe    # Unsubscribe from notifications
GET    /api/notifications/preferences    # Get notification preferences
PUT    /api/notifications/preferences    # Update preferences
```

## 📨 Message Types and Templates

### Email Templates
```html
<!-- Welcome Email Template -->
<!DOCTYPE html>
<html>
<head>
    <title>Welcome to {{app_name}}</title>
</head>
<body>
    <h1>Welcome {{user_name}}!</h1>
    <p>Thank you for joining {{app_name}}. We're excited to have you on board.</p>
    <a href="{{activation_link}}" style="background: #007bff; color: white; padding: 10px 20px; text-decoration: none;">
        Activate Your Account
    </a>
</body>
</html>

<!-- Password Reset Template -->
<h2>Password Reset Request</h2>
<p>Hi {{user_name}},</p>
<p>You requested a password reset. Click the link below to reset your password:</p>
<a href="{{reset_link}}">Reset Password</a>
<p>This link expires in 24 hours.</p>
```

### SMS Templates
```python
sms_templates = {
    "verification": {
        "message": "Your {{app_name}} verification code is: {{code}}",
        "variables": ["app_name", "code"]
    },
    "alert": {
        "message": "🚨 Alert: {{message}}",
        "variables": ["message"]
    },
    "reminder": {
        "message": "Reminder: {{event}} starts in {{time_until}}",
        "variables": ["event", "time_until"]
    },
    "order_update": {
        "message": "Your order #{{order_id}} is now {{status}}. Track: {{tracking_url}}",
        "variables": ["order_id", "status", "tracking_url"]
    }
}
```

### Push Notification Templates
```json
{
  "welcome": {
    "title": "Welcome to {{app_name}}!",
    "body": "Thanks for joining us, {{user_name}}",
    "data": {
      "screen": "welcome",
      "user_id": "{{user_id}}"
    }
  },
  "new_message": {
    "title": "New message from {{sender_name}}",
    "body": "{{message_preview}}",
    "data": {
      "screen": "chat",
      "chat_id": "{{chat_id}}"
    }
  },
  "achievement": {
    "title": "🎉 Achievement Unlocked!",
    "body": "You've earned: {{achievement_name}}",
    "data": {
      "screen": "achievements",
      "achievement_id": "{{achievement_id}}"
    }
  }
}
```

## 🔄 Message Processing Workflow

### Single Notification Flow
```python
def send_notification_workflow(notification_request):
    """
    Process single notification request
    """
    # 1. Validate request
    validated_request = validate_notification_request(notification_request)
    
    # 2. Apply user preferences
    filtered_channels = apply_user_preferences(
        user_id=validated_request.user_id,
        channels=validated_request.channels
    )
    
    # 3. Render templates
    rendered_messages = {}
    for channel in filtered_channels:
        template = get_template(channel, validated_request.template_id)
        rendered_messages[channel] = render_template(
            template, 
            validated_request.variables
        )
    
    # 4. Queue for delivery
    for channel, message in rendered_messages.items():
        queue_message_for_delivery(
            channel=channel,
            message=message,
            user_id=validated_request.user_id,
            priority=validated_request.priority
        )
    
    # 5. Return tracking ID
    return generate_tracking_id(validated_request)
```

### Bulk Notification Flow
```python
def send_bulk_notifications_workflow(bulk_request):
    """
    Process bulk notification request
    """
    # 1. Validate and segment users
    user_segments = segment_users(bulk_request.user_criteria)
    
    # 2. Apply rate limiting
    batches = create_delivery_batches(
        user_segments, 
        batch_size=100,
        rate_limit=bulk_request.rate_limit
    )
    
    # 3. Process batches
    for batch in batches:
        for user in batch:
            # Apply user-specific preferences and variables
            personalized_request = personalize_notification(
                bulk_request.template,
                user
            )
            
            # Queue individual notification
            queue_message_for_delivery(personalized_request)
    
    # 4. Return campaign tracking ID
    return generate_campaign_tracking_id(bulk_request)
```

## 📊 Analytics and Tracking

### Delivery Metrics
```python
delivery_metrics = {
    "sent": "Total messages sent",
    "delivered": "Successfully delivered messages",
    "failed": "Failed delivery attempts",
    "bounced": "Bounced emails",
    "opened": "Email opens",
    "clicked": "Link clicks",
    "unsubscribed": "Unsubscribe events",
    "spam_reports": "Spam complaints"
}

# Channel-specific metrics
channel_metrics = {
    "email": ["sent", "delivered", "opened", "clicked", "bounced", "spam_reports"],
    "sms": ["sent", "delivered", "failed"],
    "push": ["sent", "delivered", "opened", "clicked"],
    "webhook": ["sent", "delivered", "failed", "retried"]
}
```

### Engagement Analytics
```sql
-- Email engagement analysis
SELECT 
    DATE_TRUNC('day', sent_at) as date,
    template_name,
    COUNT(*) as sent_count,
    COUNT(CASE WHEN delivered_at IS NOT NULL THEN 1 END) as delivered_count,
    COUNT(CASE WHEN opened_at IS NOT NULL THEN 1 END) as opened_count,
    COUNT(CASE WHEN clicked_at IS NOT NULL THEN 1 END) as clicked_count,
    ROUND(
        COUNT(CASE WHEN opened_at IS NOT NULL THEN 1 END) * 100.0 / 
        COUNT(CASE WHEN delivered_at IS NOT NULL THEN 1 END), 2
    ) as open_rate,
    ROUND(
        COUNT(CASE WHEN clicked_at IS NOT NULL THEN 1 END) * 100.0 / 
        COUNT(CASE WHEN opened_at IS NOT NULL THEN 1 END), 2
    ) as click_rate
FROM email_notifications 
WHERE sent_at >= CURRENT_DATE - INTERVAL '30 days'
GROUP BY DATE_TRUNC('day', sent_at), template_name
ORDER BY date DESC, sent_count DESC;
```

## 🔒 Security Features

### Data Protection
- PII encryption in message queues
- Secure API key storage in secret managers
- Message content encryption in transit
- Audit logging for all notification activities

### Compliance Features
- GDPR compliance with unsubscribe mechanisms
- CAN-SPAM Act compliance for emails
- TCPA compliance for SMS
- Data retention policies

### Rate Limiting and Abuse Prevention
```python
rate_limiting_config = {
    "per_user": {
        "email": "10/minute",
        "sms": "5/minute", 
        "push": "20/minute"
    },
    "per_ip": {
        "api_calls": "100/minute"
    },
    "global": {
        "email": "10000/hour",
        "sms": "5000/hour",
        "push": "50000/hour"
    }
}
```

## 📈 Monitoring & Observability

### Health Checks
```
GET /health              # Basic health check
GET /health/channels     # Channel connectivity status
GET /health/queues       # Message queue status
GET /metrics            # Prometheus metrics
```

### Key Metrics
- **Delivery Performance**: Success rate, delivery time, retry attempts
- **Channel Health**: API response times, error rates, quota usage
- **Business Metrics**: Engagement rates, conversion tracking
- **System Health**: Queue depth, processing latency, error rates

### Alerting Rules
```yaml
# High delivery failure rate
- alert: HighDeliveryFailureRate
  expr: (failed_deliveries / total_deliveries) > 0.05
  for: 5m
  labels:
    severity: warning
  annotations:
    summary: "High notification delivery failure rate"

# Queue depth too high
- alert: HighQueueDepth
  expr: notification_queue_depth > 1000
  for: 2m
  labels:
    severity: critical
  annotations:
    summary: "Notification queue depth is too high"
```

## 🔄 Integration Points

### Internal Services
- **User Service**: User preferences and contact information
- **Auth Service**: User authentication for notification preferences
- **AI Advisor Service**: AI-generated notification content
- **KPI Engine**: Performance-based notification triggers

### External Integrations
- **CRM Systems**: Customer communication history
- **Analytics Platforms**: Engagement tracking
- **Marketing Automation**: Campaign management
- **Customer Support**: Support ticket notifications

## 🚦 Deployment Strategies

### Blue-Green Deployment
```bash
# Deploy to green environment
terraform apply -var="deployment_slot=green"

# Test notification delivery
curl -X POST https://notifications-green.example.com/api/notifications/test

# Switch traffic
terraform apply -var="active_slot=green"
```

### Canary Deployment for New Channels
```bash
# Deploy new channel with limited traffic
terraform apply -var="new_channel_percentage=10"

# Monitor delivery success and gradually increase
terraform apply -var="new_channel_percentage=50"
terraform apply -var="new_channel_percentage=100"
```

## 🛠️ Development

### Local Development
```bash
# Start message queue and database
docker-compose up -d redis postgresql

# Set environment variables
export BREVO_API_KEY="your-brevo-key"
export TWILIO_ACCOUNT_SID="your-twilio-sid"
export TWILIO_AUTH_TOKEN="your-twilio-token"
export FIREBASE_SERVER_KEY="your-firebase-key"

# Run notifications service
npm run dev
```

### Testing
```bash
# Unit tests
npm run test

# Integration tests with external services
npm run test:integration

# Load tests for high-volume scenarios
npm run test:load

# Template rendering tests
npm run test:templates
```

## 🆘 Troubleshooting

### Common Issues

**Email Delivery Failures**
```bash
# Check Brevo API status
curl -H "api-key: $BREVO_API_KEY" https://api.brevo.com/v3/account

# Review bounce and spam reports
curl /api/notifications/analytics/email/issues
```

**SMS Delivery Issues**
```bash
# Check Twilio account status
curl -u $TWILIO_ACCOUNT_SID:$TWILIO_AUTH_TOKEN \
     https://api.twilio.com/2010-04-01/Accounts/$TWILIO_ACCOUNT_SID.json

# Review SMS delivery logs
kubectl logs -f deployment/notifications-service | grep "sms"
```

**Push Notification Problems**
```bash
# Verify Firebase configuration
curl -H "Authorization: key=$FIREBASE_SERVER_KEY" \
     https://fcm.googleapis.com/fcm/send

# Check device token validity
curl /api/notifications/push/validate-tokens
```

## 📚 Related Documentation

- [User Service Integration](../user/README.md)
- [Email Template Design Guide](../../templates/email/README.md)
- [Push Notification Setup](../../mobile/push-notifications.md)
- [Analytics and Reporting](../../analytics/README.md)
- [Compliance and Privacy](../../compliance/README.md)
