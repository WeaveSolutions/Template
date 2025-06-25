# Notification Microservice Implementation Guide

## Overview

The Notification microservice provides a unified multi-channel communication system for the Nexpo application, handling email, SMS, push notifications, and in-app messaging. It integrates with various third-party providers while maintaining a consistent API for the rest of the application.

## Architecture

### Core Technologies
- **Language**: TypeScript/Node.js
- **Framework**: Express.js or Fastify
- **Message Queue**: Redis, RabbitMQ, or AWS SQS
- **Database**: PostgreSQL for templates and delivery history
- **Cache**: Redis for rate limiting and temporary data

### External Integrations
- **Email**: Brevo (SendinBlue) for transactional emails
- **SMS**: Twilio for SMS delivery
- **Push**: Firebase Cloud Messaging (FCM) and Apple Push Notification Service (APNS)
- **In-App**: WebSocket for real-time notifications

### Service Structure
```
notification-service/
├── src/
│   ├── api/
│   │   ├── routes/
│   │   │   ├── notifications.ts
│   │   │   ├── templates.ts
│   │   │   ├── preferences.ts
│   │   │   └── analytics.ts
│   │   └── middleware/
│   │       ├── auth.ts
│   │       └── validation.ts
│   ├── channels/
│   │   ├── email/
│   │   │   ├── brevo.ts
│   │   │   └── types.ts
│   │   ├── sms/
│   │   │   ├── twilio.ts
│   │   │   └── types.ts
│   │   ├── push/
│   │   │   ├── fcm.ts
│   │   │   ├── apns.ts
│   │   │   └── types.ts
│   │   └── inapp/
│   │       ├── websocket.ts
│   │       └── types.ts
│   ├── templates/
│   │   ├── engine.ts
│   │   ├── storage.ts
│   │   └── types.ts
│   ├── queue/
│   │   ├── processor.ts
│   │   ├── retry.ts
│   │   └── priority.ts
│   ├── database/
│   │   ├── models/
│   │   ├── migrations/
│   │   └── connection.ts
│   └── utils/
│       ├── logger.ts
│       └── metrics.ts
├── tests/
├── docs/
└── package.json
```

## Channel Implementations

### Email Channel (Brevo)
```typescript
// src/channels/email/brevo.ts
import * as Brevo from '@getbrevo/brevo';
import { EmailChannel, EmailMessage, EmailResult } from './types';

export class BrevoEmailChannel implements EmailChannel {
  private client: Brevo.TransactionalEmailsApi;
  private defaultSender: { email: string; name: string };

  constructor(apiKey: string, defaultSender: { email: string; name: string }) {
    const apiInstance = new Brevo.TransactionalEmailsApi();
    apiInstance.setApiKey(Brevo.TransactionalEmailsApiApiKeys.apiKey, apiKey);
    this.client = apiInstance;
    this.defaultSender = defaultSender;
  }

  async send(message: EmailMessage): Promise<EmailResult> {
    try {
      const sendSmtpEmail = new Brevo.SendSmtpEmail();
      sendSmtpEmail.subject = message.subject;
      sendSmtpEmail.htmlContent = message.html;
      sendSmtpEmail.sender = message.from || this.defaultSender;
      sendSmtpEmail.to = [{ email: message.to, name: message.toName }];
      
      if (message.templateId) {
        sendSmtpEmail.templateId = Number(message.templateId);
        sendSmtpEmail.params = message.templateData;
      }

      const result = await this.client.sendTransacEmail(sendSmtpEmail);
      
      return {
        success: true,
        messageId: result.body.messageId,
        provider: 'brevo'
      };
    } catch (error) {
      return {
        success: false,
        error: error.message,
        provider: 'brevo'
      };
    }
  }
}
```

### SMS Channel (Twilio)
```typescript
// src/channels/sms/twilio.ts
import twilio from 'twilio';
import { SmsChannel, SmsMessage, SmsResult } from './types';

export class TwilioSmsChannel implements SmsChannel {
  private client: twilio.Twilio;
  private fromNumber: string;

  constructor(accountSid: string, authToken: string, fromNumber: string) {
    this.client = twilio(accountSid, authToken);
    this.fromNumber = fromNumber;
  }

  async send(message: SmsMessage): Promise<SmsResult> {
    try {
      const result = await this.client.messages.create({
        body: message.body,
        from: this.fromNumber,
        to: message.to
      });

      return {
        success: true,
        messageId: result.sid,
        provider: 'twilio',
        status: result.status
      };
    } catch (error) {
      return {
        success: false,
        error: error.message,
        provider: 'twilio'
      };
    }
  }
}
```

### Push Notification Channel
```typescript
// src/channels/push/fcm.ts
import admin from 'firebase-admin';
import { PushChannel, PushMessage, PushResult } from './types';

export class FcmPushChannel implements PushChannel {
  private messaging: admin.messaging.Messaging;

  constructor(serviceAccount: any) {
    admin.initializeApp({
      credential: admin.credential.cert(serviceAccount)
    });
    this.messaging = admin.messaging();
  }

  async send(message: PushMessage): Promise<PushResult> {
    try {
      const fcmMessage: admin.messaging.Message = {
        notification: {
          title: message.title,
          body: message.body
        },
        data: message.data || {},
        token: message.deviceToken,
        android: message.android,
        apns: message.apns
      };

      const messageId = await this.messaging.send(fcmMessage);

      return {
        success: true,
        messageId,
        provider: 'fcm'
      };
    } catch (error) {
      return {
        success: false,
        error: error.message,
        provider: 'fcm'
      };
    }
  }

  async sendMulticast(message: PushMessage, tokens: string[]): Promise<PushResult[]> {
    const multicastMessage: admin.messaging.MulticastMessage = {
      notification: {
        title: message.title,
        body: message.body
      },
      data: message.data || {},
      tokens
    };

    const response = await this.messaging.sendMulticast(multicastMessage);
    
    return response.responses.map((resp, idx) => ({
      success: resp.success,
      messageId: resp.messageId,
      error: resp.error?.message,
      provider: 'fcm',
      token: tokens[idx]
    }));
  }
}
```

## API Endpoints

### Send Notification
```typescript
// POST /api/notifications/send
interface SendNotificationRequest {
  userId: string;
  channels: ('email' | 'sms' | 'push' | 'inapp')[];
  template?: string;
  data?: Record<string, any>;
  priority?: 'low' | 'normal' | 'high';
  scheduledAt?: Date;
  email?: {
    to?: string;
    subject?: string;
    body?: string;
  };
  sms?: {
    to?: string;
    body?: string;
  };
  push?: {
    title?: string;
    body?: string;
    data?: Record<string, any>;
  };
}

interface SendNotificationResponse {
  notificationId: string;
  status: 'queued' | 'processing' | 'sent' | 'failed';
  channels: {
    channel: string;
    status: string;
    messageId?: string;
    error?: string;
  }[];
}
```

### Manage Templates
```typescript
// GET /api/templates
// POST /api/templates
// PUT /api/templates/:id
// DELETE /api/templates/:id

interface Template {
  id: string;
  name: string;
  channel: 'email' | 'sms' | 'push' | 'inapp';
  subject?: string; // For email
  content: string;
  variables: string[];
  metadata?: Record<string, any>;
  createdAt: Date;
  updatedAt: Date;
}
```

### User Preferences
```typescript
// GET /api/preferences/:userId
// PUT /api/preferences/:userId

interface UserPreferences {
  userId: string;
  channels: {
    email: {
      enabled: boolean;
      address?: string;
      categories?: string[];
    };
    sms: {
      enabled: boolean;
      number?: string;
      categories?: string[];
    };
    push: {
      enabled: boolean;
      tokens?: string[];
      categories?: string[];
    };
    inapp: {
      enabled: boolean;
      categories?: string[];
    };
  };
  quiet_hours?: {
    enabled: boolean;
    start: string; // "22:00"
    end: string;   // "08:00"
    timezone: string;
  };
}
```

### Analytics
```typescript
// GET /api/analytics/delivery
// GET /api/analytics/engagement

interface DeliveryMetrics {
  period: string;
  metrics: {
    sent: number;
    delivered: number;
    failed: number;
    bounced: number;
    opened?: number;
    clicked?: number;
  };
  byChannel: Record<string, {
    sent: number;
    delivered: number;
    failed: number;
  }>;
}
```

## Template Engine

### Dynamic Template Rendering
```typescript
// src/templates/engine.ts
import Handlebars from 'handlebars';
import { Template, TemplateData } from './types';

export class TemplateEngine {
  private handlebars: typeof Handlebars;

  constructor() {
    this.handlebars = Handlebars.create();
    this.registerHelpers();
  }

  private registerHelpers() {
    // Format date helper
    this.handlebars.registerHelper('formatDate', (date: Date, format: string) => {
      return new Intl.DateTimeFormat('en-US', {
        dateStyle: format as any
      }).format(date);
    });

    // Conditional helper
    this.handlebars.registerHelper('ifEqual', (a: any, b: any, options: any) => {
      return a === b ? options.fn(this) : options.inverse(this);
    });

    // Currency formatter
    this.handlebars.registerHelper('currency', (amount: number) => {
      return new Intl.NumberFormat('en-US', {
        style: 'currency',
        currency: 'USD'
      }).format(amount);
    });
  }

  async render(template: Template, data: TemplateData): Promise<string> {
    const compiled = this.handlebars.compile(template.content);
    return compiled(data);
  }

  async renderSubject(template: Template, data: TemplateData): Promise<string> {
    if (!template.subject) return '';
    const compiled = this.handlebars.compile(template.subject);
    return compiled(data);
  }
}
```

## Queue Processing

### Message Queue Processor
```typescript
// src/queue/processor.ts
import { Queue, Worker } from 'bullmq';
import { NotificationJob, NotificationResult } from './types';
import { ChannelManager } from '../channels/manager';

export class QueueProcessor {
  private queue: Queue;
  private worker: Worker;
  private channelManager: ChannelManager;

  constructor(redisUrl: string, channelManager: ChannelManager) {
    this.queue = new Queue('notifications', {
      connection: { url: redisUrl }
    });
    
    this.channelManager = channelManager;
    this.initializeWorker(redisUrl);
  }

  private initializeWorker(redisUrl: string) {
    this.worker = new Worker(
      'notifications',
      async (job) => {
        return await this.processNotification(job.data);
      },
      {
        connection: { url: redisUrl },
        concurrency: 10,
        limiter: {
          max: 100,
          duration: 60000 // 100 jobs per minute
        }
      }
    );

    this.worker.on('completed', (job, result) => {
      console.log(`Notification ${job.id} completed`, result);
    });

    this.worker.on('failed', (job, err) => {
      console.error(`Notification ${job?.id} failed`, err);
    });
  }

  async processNotification(job: NotificationJob): Promise<NotificationResult> {
    const results: NotificationResult = {
      notificationId: job.notificationId,
      channels: []
    };

    // Process each channel
    for (const channel of job.channels) {
      try {
        const result = await this.channelManager.send(channel, job);
        results.channels.push({
          channel,
          success: result.success,
          messageId: result.messageId,
          error: result.error
        });
      } catch (error) {
        results.channels.push({
          channel,
          success: false,
          error: error.message
        });
      }
    }

    return results;
  }

  async addJob(job: NotificationJob) {
    const options = {
      priority: job.priority === 'high' ? 1 : job.priority === 'low' ? 10 : 5,
      delay: job.scheduledAt ? new Date(job.scheduledAt).getTime() - Date.now() : 0,
      attempts: 3,
      backoff: {
        type: 'exponential',
        delay: 2000
      }
    };

    return await this.queue.add(job.notificationId, job, options);
  }
}
```

## Environment Variables

```bash
# Service Configuration
NODE_ENV=production
PORT=3003
SERVICE_NAME=notification-service

# Auth0 M2M
AUTH0_DOMAIN=your-tenant.auth0.com
AUTH0_CLIENT_ID=your_m2m_client_id
AUTH0_CLIENT_SECRET=your_m2m_client_secret
AUTH0_AUDIENCE=https://api.nexpo.app

# Database
DATABASE_URL=postgresql://user:password@localhost:5432/notifications

# Redis
REDIS_URL=redis://localhost:6379

# Email Provider (Brevo)
BREVO_API_KEY=your_brevo_api_key
DEFAULT_FROM_EMAIL=noreply@nexpo.app
DEFAULT_FROM_NAME=Nexpo

# SMS Provider (Twilio)
TWILIO_ACCOUNT_SID=your_account_sid
TWILIO_AUTH_TOKEN=your_auth_token
TWILIO_FROM_NUMBER=+1234567890

# Push Notifications
FCM_SERVICE_ACCOUNT={"type":"service_account",...}
APNS_KEY_ID=your_key_id
APNS_TEAM_ID=your_team_id
APNS_BUNDLE_ID=com.nexpo.app

# WebSocket
WEBSOCKET_PORT=3004
WEBSOCKET_PATH=/notifications

# Analytics
POSTHOG_API_KEY=your_posthog_key
POSTHOG_HOST=https://app.posthog.com

# Rate Limiting
RATE_LIMIT_EMAIL_PER_HOUR=10
RATE_LIMIT_SMS_PER_HOUR=5
RATE_LIMIT_PUSH_PER_HOUR=20
```

## Security Considerations

### Authentication
- Service-to-service authentication using Auth0 M2M tokens
- JWT validation on all API endpoints
- User context validation for notification sending

### Data Protection
- Encrypt sensitive data at rest (phone numbers, email addresses)
- Use TLS for all external API communications
- Implement PII data retention policies

### Rate Limiting
- Per-user rate limits by channel
- Global rate limits to prevent abuse
- Gradual backoff for failed deliveries

## Deployment

### Docker Configuration
```dockerfile
FROM node:18-alpine

WORKDIR /app

COPY package*.json ./
RUN npm ci --only=production

COPY . .
RUN npm run build

EXPOSE 3003
CMD ["node", "dist/index.js"]
```

### Health Checks
```typescript
// GET /health
app.get('/health', async (req, res) => {
  const checks = {
    service: 'ok',
    database: await checkDatabase(),
    redis: await checkRedis(),
    providers: {
      email: await checkBrevo(),
      sms: await checkTwilio(),
      push: await checkFcm()
    }
  };

  const healthy = Object.values(checks).every(v => 
    typeof v === 'string' ? v === 'ok' : 
    Object.values(v).every(s => s === 'ok')
  );

  res.status(healthy ? 200 : 503).json(checks);
});
```

## Testing Strategy

### Unit Tests
- Channel implementations
- Template rendering
- Queue processing logic
- API endpoint validation

### Integration Tests
- Provider API integration
- Database operations
- Queue processing
- End-to-end notification flow

### Load Testing
- Queue throughput testing
- API endpoint performance
- Provider rate limit testing

## Monitoring

### Metrics to Track
- Delivery rates by channel
- Failed delivery reasons
- Queue depth and processing time
- API response times
- Provider API errors

### Alerts
- High failure rates
- Queue backup
- Provider API outages
- Rate limit approaching

## Next Steps

1. **Implement Core Service**
   - Set up Express/Fastify server
   - Implement Auth0 M2M authentication
   - Create database schema

2. **Integrate Providers**
   - Configure Brevo for email
   - Set up Twilio for SMS
   - Implement FCM/APNS for push

3. **Build Queue System**
   - Set up BullMQ with Redis
   - Implement retry logic
   - Add priority queuing

4. **Create Template System**
   - Design template schema
   - Build template CRUD API
   - Implement Handlebars engine

5. **Add Analytics**
   - Track delivery metrics
   - Integrate PostHog events
   - Build analytics API

6. **Deploy Service**
   - Create Docker image
   - Set up health checks
   - Configure monitoring