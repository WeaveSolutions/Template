# Payments Microservice Implementation Guide

## Overview
The Payments Microservice is a comprehensive billing and subscription management system for the Nexpo platform. Based on the Terraform infrastructure, it supports multiple payment providers (Polar, Stripe, PayPal, Apple Pay, Google Pay), handles subscription lifecycles, processes mobile in-app purchases, and provides revenue analytics integration with PostHog.

## Architecture Components

### 1. Core Technologies
- **Runtime**: Node.js with Express.js/Fastify for high performance
- **Language**: TypeScript with strict typing
- **API Style**: RESTful with webhook endpoints
- **Authentication**: Auth0 M2M for service-to-service, JWT for client requests
- **Databases**: 
  - PostgreSQL for transactional data
  - Redis for caching and idempotency
  - TimescaleDB for analytics
- **Message Queue**: RabbitMQ/AWS SQS/Google Pub-Sub for async processing

### 2. Service Structure
```
payments/
├── src/
│   ├── api/
│   │   ├── routes/           # REST endpoints
│   │   ├── webhooks/         # Provider webhook handlers
│   │   └── middleware/       # Auth, validation, idempotency
│   ├── providers/
│   │   ├── polar/           # Polar integration
│   │   ├── stripe/          # Stripe integration
│   │   ├── paypal/          # PayPal integration
│   │   ├── applepay/        # Apple Pay integration
│   │   └── googlepay/       # Google Pay integration
│   ├── billing/
│   │   ├── subscriptions/   # Subscription management
│   │   ├── invoicing/       # Invoice generation
│   │   ├── dunning/         # Failed payment recovery
│   │   └── taxes/           # Tax calculations
│   ├── mobile/
│   │   ├── app-store/       # iOS IAP handling
│   │   ├── play-store/      # Android IAP handling
│   │   └── receipt-validation/ # Receipt verification
│   ├── analytics/
│   │   ├── revenue/         # MRR, ARR calculations
│   │   ├── metrics/         # Business metrics
│   │   └── posthog/         # PostHog integration
│   ├── database/
│   │   ├── entities/        # Database models
│   │   ├── migrations/      # Database migrations
│   │   └── repositories/    # Data access layer
│   ├── queues/             # Message queue handlers
│   ├── utils/              # Common utilities
│   └── index.ts            # Application entry
├── tests/                  # Unit and integration tests
├── docs/                   # API documentation
├── migrations/             # Database migrations
├── scripts/               # Deployment and utility scripts
└── Dockerfile             # Container configuration
```

## Implementation Details

### 1. Multi-Provider Architecture

#### Provider Interface
```typescript
// src/providers/provider.interface.ts
export interface PaymentProvider {
  name: string;
  
  // Customer Management
  createCustomer(data: CustomerData): Promise<Customer>;
  updateCustomer(customerId: string, data: Partial<CustomerData>): Promise<Customer>;
  deleteCustomer(customerId: string): Promise<void>;
  
  // Payment Methods
  attachPaymentMethod(customerId: string, paymentMethod: PaymentMethodData): Promise<PaymentMethod>;
  detachPaymentMethod(paymentMethodId: string): Promise<void>;
  listPaymentMethods(customerId: string): Promise<PaymentMethod[]>;
  
  // Charges
  createCharge(data: ChargeData): Promise<Charge>;
  captureCharge(chargeId: string, amount?: number): Promise<Charge>;
  refundCharge(chargeId: string, amount?: number): Promise<Refund>;
  
  // Subscriptions
  createSubscription(data: SubscriptionData): Promise<Subscription>;
  updateSubscription(subscriptionId: string, data: Partial<SubscriptionData>): Promise<Subscription>;
  cancelSubscription(subscriptionId: string, immediately?: boolean): Promise<Subscription>;
  pauseSubscription(subscriptionId: string): Promise<Subscription>;
  resumeSubscription(subscriptionId: string): Promise<Subscription>;
  
  // Webhooks
  constructWebhookEvent(payload: string, signature: string): WebhookEvent;
  handleWebhook(event: WebhookEvent): Promise<void>;
}
```

#### Polar Implementation
```typescript
// src/providers/polar/polar.provider.ts
import { Polar } from '@polar-sh/sdk';
import { PaymentProvider, CustomerData, Customer } from '../provider.interface';

export class PolarProvider implements PaymentProvider {
  private client: Polar;
  
  constructor() {
    this.client = new Polar({
      accessToken: process.env.POLAR_ACCESS_TOKEN,
      server: process.env.POLAR_SERVER || 'production'
    });
  }

  async createCustomer(data: CustomerData): Promise<Customer> {
    const polarCustomer = await this.client.customers.create({
      email: data.email,
      name: data.name,
      metadata: {
        userId: data.userId,
        ...data.metadata
      }
    });

    return this.mapPolarCustomer(polarCustomer);
  }

  async createSubscription(data: SubscriptionData): Promise<Subscription> {
    const subscription = await this.client.subscriptions.create({
      customer_id: data.customerId,
      price_id: data.priceId,
      metadata: {
        userId: data.userId,
        planName: data.planName
      }
    });

    // Emit event for analytics
    await this.emitSubscriptionEvent('created', subscription);
    
    return this.mapPolarSubscription(subscription);
  }

  // Additional implementation...
}
```

### 2. Subscription Management

#### Subscription Service
```typescript
// src/billing/subscriptions/subscription.service.ts
import { Injectable } from '@nestjs/common';
import { InjectRepository } from '@nestjs/typeorm';
import { Repository } from 'typeorm';
import { Subscription } from '../../database/entities/subscription.entity';
import { PaymentProviderFactory } from '../../providers/provider.factory';
import { EventEmitter } from 'events';

@Injectable()
export class SubscriptionService {
  constructor(
    @InjectRepository(Subscription)
    private subscriptionRepo: Repository<Subscription>,
    private providerFactory: PaymentProviderFactory,
    private eventEmitter: EventEmitter
  ) {}

  async createSubscription(userId: string, planId: string, providerId: string) {
    // Check for existing active subscription
    const existing = await this.subscriptionRepo.findOne({
      where: { userId, status: 'active' }
    });

    if (existing) {
      throw new Error('User already has an active subscription');
    }

    // Get provider and create subscription
    const provider = this.providerFactory.getProvider(providerId);
    const customer = await this.getOrCreateCustomer(userId, provider);
    
    const providerSubscription = await provider.createSubscription({
      customerId: customer.providerId,
      priceId: planId,
      userId,
      planName: await this.getPlanName(planId)
    });

    // Save to database
    const subscription = await this.subscriptionRepo.save({
      userId,
      planId,
      providerId,
      providerSubscriptionId: providerSubscription.id,
      status: 'active',
      currentPeriodStart: providerSubscription.currentPeriodStart,
      currentPeriodEnd: providerSubscription.currentPeriodEnd,
      cancelAtPeriodEnd: false
    });

    // Emit events
    this.eventEmitter.emit('subscription.created', {
      subscription,
      provider: providerId
    });

    // Update user entitlements
    await this.updateUserEntitlements(userId, planId);

    return subscription;
  }

  async upgradeSubscription(subscriptionId: string, newPlanId: string) {
    const subscription = await this.subscriptionRepo.findOne(subscriptionId);
    if (!subscription) throw new Error('Subscription not found');

    const provider = this.providerFactory.getProvider(subscription.providerId);
    
    // Calculate proration
    const proration = await this.calculateProration(subscription, newPlanId);
    
    // Update with provider
    const updated = await provider.updateSubscription(
      subscription.providerSubscriptionId,
      { priceId: newPlanId }
    );

    // Update local record
    subscription.planId = newPlanId;
    await this.subscriptionRepo.save(subscription);

    // Emit event
    this.eventEmitter.emit('subscription.upgraded', {
      subscription,
      previousPlan: subscription.planId,
      newPlan: newPlanId,
      proration
    });

    return subscription;
  }

  private async calculateProration(subscription: Subscription, newPlanId: string) {
    // Complex proration logic based on time remaining and price difference
    const remainingDays = this.calculateRemainingDays(subscription);
    const currentPrice = await this.getPlanPrice(subscription.planId);
    const newPrice = await this.getPlanPrice(newPlanId);
    
    return {
      amount: (newPrice - currentPrice) * (remainingDays / 30),
      description: `Proration for plan change`
    };
  }
}
```

### 3. Mobile In-App Purchase Integration

#### Receipt Validation Service
```typescript
// src/mobile/receipt-validation/receipt-validator.ts
import { Injectable } from '@nestjs/common';
import appleReceiptVerify from 'node-apple-receipt-verify';
import { google } from 'googleapis';

@Injectable()
export class ReceiptValidationService {
  private androidPublisher;

  constructor() {
    // Configure Apple receipt verification
    appleReceiptVerify.config({
      secret: process.env.APPLE_SHARED_SECRET,
      environment: process.env.NODE_ENV === 'production' ? 
        ['production'] : ['sandbox', 'production']
    });

    // Configure Google Play verification
    this.androidPublisher = google.androidpublisher({
      version: 'v3',
      auth: new google.auth.JWT({
        email: process.env.GOOGLE_SERVICE_ACCOUNT_EMAIL,
        key: process.env.GOOGLE_SERVICE_ACCOUNT_KEY,
        scopes: ['https://www.googleapis.com/auth/androidpublisher']
      })
    });
  }

  async validateAppleReceipt(receipt: string): Promise<AppleValidationResult> {
    try {
      const products = await appleReceiptVerify.validate({
        receipt: receipt
      });

      if (!products || products.length === 0) {
        throw new Error('No valid products in receipt');
      }

      // Find active subscription
      const activeSubscription = products.find(p => 
        p.expirationDate && new Date(p.expirationDate) > new Date()
      );

      return {
        valid: true,
        products,
        activeSubscription,
        originalTransactionId: products[0].originalTransactionId
      };
    } catch (error) {
      console.error('Apple receipt validation failed:', error);
      return { valid: false, error: error.message };
    }
  }

  async validateGoogleReceipt(
    packageName: string,
    subscriptionId: string,
    purchaseToken: string
  ): Promise<GoogleValidationResult> {
    try {
      const response = await this.androidPublisher.purchases.subscriptions.get({
        packageName,
        subscriptionId,
        token: purchaseToken
      });

      const subscription = response.data;
      const isActive = this.isGoogleSubscriptionActive(subscription);

      return {
        valid: true,
        subscription,
        isActive,
        expiryTimeMillis: subscription.expiryTimeMillis,
        orderId: subscription.orderId
      };
    } catch (error) {
      console.error('Google receipt validation failed:', error);
      return { valid: false, error: error.message };
    }
  }

  private isGoogleSubscriptionActive(subscription: any): boolean {
    const now = Date.now();
    const expiryTime = parseInt(subscription.expiryTimeMillis);
    
    return subscription.paymentState === 1 && // Payment received
           expiryTime > now &&
           !subscription.cancelReason;
  }
}
```

### 4. Webhook Processing

#### Webhook Handler
```typescript
// src/api/webhooks/webhook.handler.ts
import { Injectable } from '@nestjs/common';
import { InjectQueue } from '@nestjs/bull';
import { Queue } from 'bull';
import crypto from 'crypto';

@Injectable()
export class WebhookHandler {
  constructor(
    @InjectQueue('webhooks') private webhookQueue: Queue,
    private providerFactory: PaymentProviderFactory
  ) {}

  async handleWebhook(
    providerId: string,
    payload: string,
    signature: string,
    headers: Record<string, string>
  ) {
    // Verify webhook signature
    const isValid = await this.verifyWebhookSignature(
      providerId,
      payload,
      signature,
      headers
    );

    if (!isValid) {
      throw new Error('Invalid webhook signature');
    }

    // Parse event
    const provider = this.providerFactory.getProvider(providerId);
    const event = provider.constructWebhookEvent(payload, signature);

    // Check idempotency
    const isDuplicate = await this.checkIdempotency(event.id);
    if (isDuplicate) {
      return { status: 'already_processed' };
    }

    // Queue for processing
    await this.webhookQueue.add('process-webhook', {
      providerId,
      event,
      receivedAt: new Date()
    }, {
      attempts: 3,
      backoff: {
        type: 'exponential',
        delay: 2000
      }
    });

    return { status: 'queued' };
  }

  private async verifyWebhookSignature(
    providerId: string,
    payload: string,
    signature: string,
    headers: Record<string, string>
  ): Promise<boolean> {
    switch (providerId) {
      case 'stripe':
        return this.verifyStripeSignature(payload, signature, headers);
      case 'polar':
        return this.verifyPolarSignature(payload, signature);
      case 'paypal':
        return this.verifyPayPalSignature(payload, headers);
      default:
        throw new Error(`Unknown provider: ${providerId}`);
    }
  }

  private verifyStripeSignature(
    payload: string,
    signature: string,
    headers: Record<string, string>
  ): boolean {
    const secret = process.env.STRIPE_WEBHOOK_SECRET;
    const sig = headers['stripe-signature'];
    
    const elements = sig.split(',').reduce((acc, element) => {
      const [key, value] = element.split('=');
      acc[key] = value;
      return acc;
    }, {});

    const signedPayload = `${elements.t}.${payload}`;
    const expectedSignature = crypto
      .createHmac('sha256', secret)
      .update(signedPayload)
      .digest('hex');

    return crypto.timingSafeEqual(
      Buffer.from(elements.v1),
      Buffer.from(expectedSignature)
    );
  }
}
```

### 5. Revenue Analytics Integration

#### PostHog Analytics Service
```typescript
// src/analytics/posthog/posthog.service.ts
import { PostHog } from 'posthog-node';
import { Injectable } from '@nestjs/common';

@Injectable()
export class PostHogAnalyticsService {
  private client: PostHog;

  constructor() {
    this.client = new PostHog(process.env.POSTHOG_API_KEY, {
      host: process.env.POSTHOG_HOST
    });
  }

  async trackRevenue(event: RevenueEvent) {
    await this.client.capture({
      distinctId: event.userId,
      event: event.type,
      properties: {
        revenue: event.amount,
        currency: event.currency,
        subscription_id: event.subscriptionId,
        plan_name: event.planName,
        provider: event.provider,
        mrr_impact: event.mrrImpact,
        ...event.metadata
      }
    });
  }

  async updateUserProperties(userId: string, properties: UserRevenueProperties) {
    await this.client.capture({
      distinctId: userId,
      event: '$set',
      properties: {
        $set: {
          subscription_status: properties.subscriptionStatus,
          current_mrr: properties.currentMrr,
          lifetime_value: properties.lifetimeValue,
          first_purchase_date: properties.firstPurchaseDate,
          last_purchase_date: properties.lastPurchaseDate,
          total_spent: properties.totalSpent,
          active_subscriptions: properties.activeSubscriptions
        }
      }
    });
  }

  async trackChurn(userId: string, reason: string, feedback?: string) {
    await this.client.capture({
      distinctId: userId,
      event: 'subscription_churned',
      properties: {
        churn_reason: reason,
        feedback,
        final_mrr: await this.getUserMrr(userId),
        lifetime_value: await this.calculateLTV(userId)
      }
    });
  }
}
```

### 6. Tax Calculation Service

#### Tax Service Implementation
```typescript
// src/billing/taxes/tax.service.ts
import { Injectable } from '@nestjs/common';
import TaxJar from 'taxjar';

@Injectable()
export class TaxService {
  private taxjar: TaxJar;

  constructor() {
    this.taxjar = new TaxJar({
      apiKey: process.env.TAXJAR_API_KEY
    });
  }

  async calculateTax(order: TaxCalculationRequest): Promise<TaxCalculationResult> {
    try {
      const response = await this.taxjar.taxForOrder({
        from_country: 'US',
        from_zip: process.env.BUSINESS_ZIP,
        from_state: process.env.BUSINESS_STATE,
        to_country: order.country,
        to_zip: order.zipCode,
        to_state: order.state,
        amount: order.amount,
        shipping: order.shipping || 0,
        line_items: order.items.map(item => ({
          id: item.id,
          quantity: item.quantity,
          unit_price: item.unitPrice,
          product_tax_code: item.taxCode
        }))
      });

      return {
        taxAmount: response.tax.amount_to_collect,
        taxRate: response.tax.rate,
        hasNexus: response.tax.has_nexus,
        breakdown: response.tax.breakdown
      };
    } catch (error) {
      console.error('Tax calculation failed:', error);
      // Fallback to simple calculation
      return this.fallbackTaxCalculation(order);
    }
  }

  private fallbackTaxCalculation(order: TaxCalculationRequest): TaxCalculationResult {
    // Simple fallback logic for when TaxJar is unavailable
    const defaultRate = 0.08; // 8% default
    const taxAmount = order.amount * defaultRate;
    
    return {
      taxAmount,
      taxRate: defaultRate,
      hasNexus: true,
      breakdown: {
        state_tax_collectable: taxAmount
      }
    };
  }
}
```

### 7. Environment Configuration

#### Required Environment Variables
```env
# Service Configuration
NODE_ENV=production
PORT=3002
SERVICE_NAME=payments-service

# Auth0 M2M
AUTH0_DOMAIN=your-tenant.auth0.com
AUTH0_AUDIENCE=https://api.nexpo.app
AUTH0_CLIENT_ID=payments-service-client
AUTH0_CLIENT_SECRET=secret

# Database
DATABASE_URL=postgresql://user:pass@localhost:5432/payments
REDIS_URL=redis://localhost:6379

# Payment Providers
POLAR_ACCESS_TOKEN=polar_test_xxx
POLAR_WEBHOOK_SECRET=whsec_xxx

STRIPE_SECRET_KEY=sk_test_xxx
STRIPE_PUBLISHABLE_KEY=pk_test_xxx
STRIPE_WEBHOOK_SECRET=whsec_xxx

PAYPAL_CLIENT_ID=xxx
PAYPAL_CLIENT_SECRET=xxx
PAYPAL_WEBHOOK_ID=xxx

# Mobile IAP
APPLE_SHARED_SECRET=xxx
GOOGLE_SERVICE_ACCOUNT_EMAIL=xxx@project.iam.gserviceaccount.com
GOOGLE_SERVICE_ACCOUNT_KEY=xxx

# Analytics
POSTHOG_API_KEY=xxx
POSTHOG_HOST=https://app.posthog.com

# Tax Service
TAXJAR_API_KEY=xxx
BUSINESS_ZIP=94105
BUSINESS_STATE=CA

# Encryption
PAYMENT_ENCRYPTION_KEY=64-char-hex-key
```

### 8. Security Implementation

#### PCI Compliance
```typescript
// src/security/pci-compliance.ts
export class PCIComplianceService {
  // Never store raw card data
  async tokenizeCardData(cardData: any): Promise<string> {
    // Use payment provider's tokenization
    throw new Error('Use provider tokenization APIs');
  }

  // Audit logging for all payment operations
  async logPaymentOperation(operation: PaymentOperation) {
    await this.auditLogger.log({
      timestamp: new Date(),
      operation: operation.type,
      userId: operation.userId,
      amount: operation.amount,
      provider: operation.provider,
      ipAddress: operation.ipAddress,
      userAgent: operation.userAgent,
      result: operation.result
    });
  }

  // Data masking for logs
  maskSensitiveData(data: any): any {
    const masked = { ...data };
    
    // Mask card numbers
    if (masked.cardNumber) {
      masked.cardNumber = `****${masked.cardNumber.slice(-4)}`;
    }
    
    // Remove CVV completely
    delete masked.cvv;
    
    // Mask account numbers
    if (masked.accountNumber) {
      masked.accountNumber = `****${masked.accountNumber.slice(-4)}`;
    }
    
    return masked;
  }
}
```

## Testing Strategy

### Integration Tests
```typescript
// tests/integration/subscription.test.ts
describe('Subscription Service Integration', () => {
  let app: INestApplication;
  let subscriptionService: SubscriptionService;

  beforeAll(async () => {
    const moduleRef = await Test.createTestingModule({
      imports: [AppModule],
    }).compile();

    app = moduleRef.createNestApplication();
    await app.init();
    
    subscriptionService = app.get(SubscriptionService);
  });

  describe('Subscription Creation', () => {
    it('should create subscription with Polar', async () => {
      const userId = 'test-user-123';
      const planId = 'pro-monthly';
      
      const subscription = await subscriptionService.createSubscription(
        userId,
        planId,
        'polar'
      );

      expect(subscription).toBeDefined();
      expect(subscription.status).toBe('active');
      expect(subscription.userId).toBe(userId);
    });

    it('should prevent duplicate active subscriptions', async () => {
      const userId = 'test-user-123';
      
      await expect(
        subscriptionService.createSubscription(userId, 'pro-monthly', 'polar')
      ).rejects.toThrow('User already has an active subscription');
    });
  });
});
```

## Next Steps

1. **Set up payment provider accounts** and obtain API credentials
2. **Configure Auth0 M2M application** for service authentication
3. **Set up databases** (PostgreSQL, Redis, TimescaleDB)
4. **Implement core payment flows** starting with one provider
5. **Add webhook endpoints** and test with provider tools
6. **Implement mobile IAP** validation for both platforms
7. **Configure tax service** based on business requirements
8. **Set up monitoring** and PCI compliance logging
9. **Create comprehensive test suite** including webhook testing
10. **Document API endpoints** with OpenAPI specification