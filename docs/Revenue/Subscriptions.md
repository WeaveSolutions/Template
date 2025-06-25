# Subscription Management - FTC Compliant

## Table of Contents

- [Overview](#overview)
- [FTC Compliance Requirements](#ftc-compliance-requirements)
  - [Clear and Prominent Disclosure](#clear-and-prominent-disclosure)
  - [Express Informed Consent](#express-informed-consent)
  - [Easy Cancellation](#easy-cancellation)
  - [Automatic Renewal Notifications](#automatic-renewal-notifications)
- [Account Center Integration](#account-center-integration)
  - [Authentication Requirements](#authentication-requirements)
  - [OAuth Provider Billing](#oauth-provider-billing)
  - [Multi-Account Management](#multi-account-management)
- [Subscription Architecture](#subscription-architecture)
  - [Database Schema](#database-schema)
  - [Payment Processing](#payment-processing)
  - [Subscription Lifecycle](#subscription-lifecycle)
- [User Experience Design](#user-experience-design)
  - [Subscription Dashboard](#subscription-dashboard)
  - [Billing Management](#billing-management)
  - [Cancellation Flow](#cancellation-flow)
- [Implementation Details](#implementation-details)
  - [Frontend Components](#frontend-components)
  - [Backend Services](#backend-services)
  - [API Endpoints](#api-endpoints)
- [Revenue Analytics](#revenue-analytics)
- [Security & Privacy](#security--privacy)
- [Testing & Compliance](#testing--compliance)

## Overview

The Nexpo Subscription Management system provides FTC-compliant subscription services integrated with the Account Center authentication system. It supports multiple payment processors, transparent billing practices, and user-friendly subscription controls across all platforms.

## FTC Compliance Requirements

### Clear and Prominent Disclosure

**Material Terms Display Requirements:**
- **Subscription Price**: Display full price including taxes and fees
- **Billing Frequency**: Clearly state weekly, monthly, or annual billing
- **Auto-Renewal**: Prominent disclosure of automatic renewal
- **Cancellation Policy**: Clear instructions for cancellation
- **Free Trial Terms**: Duration and post-trial charges
- **Material Changes**: 30-day advance notice for price increases

**Implementation:**
```typescript
interface SubscriptionDisclosure {
  price: {
    amount: number;
    currency: string;
    frequency: 'weekly' | 'monthly' | 'quarterly' | 'annually';
    taxes_included: boolean;
    additional_fees: Array<{
      name: string;
      amount: number;
      description: string;
    }>;
  };
  auto_renewal: {
    enabled: boolean;
    next_billing_date: string;
    cancellation_deadline: string;
  };
  free_trial: {
    duration_days: number;
    requires_payment_method: boolean;
    charges_after_trial: number;
  };
  cancellation: {
    method: 'account_center' | 'email' | 'phone';
    deadline: string;
    refund_policy: string;
  };
}
```

### Express Informed Consent

**Pre-Purchase Consent Flow:**
1. **Clear Subscription Summary**: Full terms in plain language
2. **Affirmative Action**: Explicit "Subscribe Now" button
3. **Consent Verification**: Checkbox confirmation for auto-renewal
4. **Email Confirmation**: Send subscription confirmation immediately

**Consent Tracking:**
```typescript
interface ConsentRecord {
  user_id: string;
  timestamp: string;
  consent_type: 'subscription_signup' | 'auto_renewal' | 'terms_update';
  ip_address: string;
  user_agent: string;
  consent_text: string;
  checkbox_checked: boolean;
  button_clicked: string;
}
```

### Easy Cancellation

**One-Click Cancellation Requirements:**
- **Same Medium**: Cancel online if subscribed online
- **No Obstacles**: No phone calls or emails required for standard cancellation
- **Immediate Confirmation**: Instant cancellation confirmation
- **Service Continuation**: Continue service until end of billing period

**Cancellation Implementation:**
```typescript
class SubscriptionCancellation {
  async cancelSubscription(userId: string, subscriptionId: string): Promise<CancellationResult> {
    // Validate user owns subscription
    const subscription = await this.validateOwnership(userId, subscriptionId);
    
    // Process immediate cancellation
    const cancellation = await this.processCancellation(subscription);
    
    // Send confirmation email
    await this.sendCancellationConfirmation(userId, cancellation);
    
    // Continue service until end of period
    await this.scheduleServiceTermination(subscription.next_billing_date);
    
    return {
      cancelled: true,
      effective_date: subscription.next_billing_date,
      refund_amount: cancellation.refund_amount,
      confirmation_number: cancellation.confirmation_id
    };
  }
}
```

### Automatic Renewal Notifications

**Notification Schedule:**
- **7 Days Before**: Email reminder with cancellation link
- **24 Hours Before**: Final renewal notice
- **Post-Renewal**: Confirmation with receipt

**Notification Content Requirements:**
```typescript
interface RenewalNotification {
  subject: string; // "Your [Service] subscription renews in 7 days"
  renewal_date: string;
  amount_to_be_charged: number;
  payment_method: string; // Last 4 digits of card
  cancellation_link: string; // Direct link to cancel
  service_period: {
    start_date: string;
    end_date: string;
  };
  customer_service: {
    email: string;
    phone?: string;
    hours: string;
  };
}
```

## Account Center Integration

### Authentication Requirements

**Subscription Access Control:**
- **Primary Account**: Only authenticated users can manage subscriptions
- **OAuth Linking**: Subscriptions linked to primary account, not OAuth providers
- **Multi-Factor Authentication**: Required for billing changes and cancellations
- **Session Management**: Secure session handling for payment operations

**Integration with Account Center:**
```typescript
interface SubscriptionAccountIntegration {
  // Link subscription to primary account regardless of login method
  linkSubscriptionToAccount(userId: string, subscriptionId: string): Promise<void>;
  
  // Verify user can access subscription
  verifySubscriptionAccess(userId: string, subscriptionId: string): Promise<boolean>;
  
  // Handle account linking scenarios
  handleAccountLinking(primaryUserId: string, linkedProviderId: string): Promise<void>;
  
  // Transfer subscriptions during account merge
  transferSubscriptions(fromUserId: string, toUserId: string): Promise<void>;
}
```

### OAuth Provider Billing

**Provider-Specific Considerations:**
- **Google OAuth**: Integrated via Qonversion for Android in-app purchases
- **Apple ID**: Integrated via Qonversion for iOS App Store billing
- **GitHub**: Developer-focused subscription tiers
- **Discord**: Community-based subscription features

**Implementation:**
```typescript
interface ProviderBillingIntegration {
  qonversion: {
    enabled: boolean;
    project_key: string;
    apple_app_store: {
      enabled: boolean;
      product_ids: string[];
      receipt_validation: boolean;
    };
    google_play: {
      enabled: boolean;
      product_ids: string[];
      purchase_validation: boolean;
    };
  };
```

### Multi-Account Management

**Account Consolidation:**
- **Primary Billing Account**: Single billing account for all linked OAuth providers
- **Usage Attribution**: Track usage across linked accounts
- **Consolidated Billing**: Single invoice for all services
- **Shared Subscriptions**: Family or team plans across linked accounts

## Subscription Architecture

### Database Schema

**Core Subscription Tables:**
```sql
-- Subscription Plans
CREATE TABLE subscription_plans (
    id UUID PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    description TEXT,
    price_cents INTEGER NOT NULL,
    currency VARCHAR(3) NOT NULL,
    billing_frequency VARCHAR(20) NOT NULL, -- weekly, monthly, quarterly, annually
    features JSONB,
    max_users INTEGER,
    created_at TIMESTAMP DEFAULT NOW(),
    updated_at TIMESTAMP DEFAULT NOW()
);

-- User Subscriptions
CREATE TABLE user_subscriptions (
    id UUID PRIMARY KEY,
    user_id UUID NOT NULL REFERENCES users(id),
    plan_id UUID NOT NULL REFERENCES subscription_plans(id),
    status VARCHAR(20) NOT NULL, -- active, cancelled, expired, trial
    current_period_start TIMESTAMP NOT NULL,
    current_period_end TIMESTAMP NOT NULL,
    cancel_at_period_end BOOLEAN DEFAULT FALSE,
    trial_start TIMESTAMP,
    trial_end TIMESTAMP,
    created_at TIMESTAMP DEFAULT NOW(),
    updated_at TIMESTAMP DEFAULT NOW()
);

-- Payment Methods
CREATE TABLE payment_methods (
    id UUID PRIMARY KEY,
    user_id UUID NOT NULL REFERENCES users(id),
    stripe_payment_method_id VARCHAR(255),
    type VARCHAR(20) NOT NULL, -- card, bank_account, digital_wallet
    last4 VARCHAR(4),
    brand VARCHAR(20),
    exp_month INTEGER,
    exp_year INTEGER,
    is_default BOOLEAN DEFAULT FALSE,
    created_at TIMESTAMP DEFAULT NOW()
);

-- Billing History
CREATE TABLE billing_history (
    id UUID PRIMARY KEY,
    subscription_id UUID NOT NULL REFERENCES user_subscriptions(id),
    amount_cents INTEGER NOT NULL,
    currency VARCHAR(3) NOT NULL,
    status VARCHAR(20) NOT NULL, -- succeeded, failed, pending
    invoice_url VARCHAR(500),
    payment_method_id UUID REFERENCES payment_methods(id),
    billing_date TIMESTAMP NOT NULL,
    created_at TIMESTAMP DEFAULT NOW()
);

-- Consent Records
CREATE TABLE subscription_consent (
    id UUID PRIMARY KEY,
    user_id UUID NOT NULL REFERENCES users(id),
    subscription_id UUID REFERENCES user_subscriptions(id),
    consent_type VARCHAR(50) NOT NULL,
    consent_text TEXT NOT NULL,
    ip_address INET,
    user_agent TEXT,
    granted_at TIMESTAMP DEFAULT NOW()
);
```

### Payment Processing

**Multi-Processor Support:**
- **Polar**: Modern payment platform with subscription management and revenue analytics
- **Stripe**: Enterprise payment processor with comprehensive features
- **PayPal**: Global payment method with buyer protection
- **Qonversion**: Mobile in-app purchase management for iOS and Android

**Payment Service Implementation:**
```typescript
class PaymentProcessor {
  constructor(
    private polar: PolarService,
    private stripe: StripeService,
    private paypal: PayPalService,
    private qonversion: QonversionService
  ) {}

  async processSubscriptionPayment(
    subscriptionId: string,
    paymentMethodId: string,
    amount: number
  ): Promise<PaymentResult> {
    const paymentMethod = await this.getPaymentMethod(paymentMethodId);
    
    switch (paymentMethod.processor) {
      case 'polar':
        return await this.polar.processPayment(subscriptionId, amount);
      case 'stripe':
        return await this.stripe.processPayment(subscriptionId, amount);
      case 'paypal':
        return await this.paypal.processPayment(subscriptionId, amount);
      case 'qonversion':
        return await this.qonversion.processIAPPayment(subscriptionId, amount);
      default:
        throw new Error('Unsupported payment processor');
    }
  }

  async handleMobileIAP(platform: 'ios' | 'android', receipt: string): Promise<SubscriptionResult> {
    return await this.qonversion.validateAndProcessIAP(platform, receipt);
  }
}
```

### Subscription Lifecycle

**Lifecycle States:**
1. **Trial**: Free trial period with optional payment method
2. **Active**: Paid subscription with regular billing
3. **Past Due**: Payment failed, retry attempts in progress
4. **Cancelled**: User cancelled, service continues until period end
5. **Expired**: Subscription ended, service terminated

**Lifecycle Management:**
```typescript
class SubscriptionLifecycle {
  async handleSubscriptionEvent(event: SubscriptionEvent): Promise<void> {
    switch (event.type) {
      case 'trial_started':
        await this.startTrialPeriod(event.subscription_id);
        break;
      case 'trial_ending':
        await this.sendTrialEndingNotification(event.subscription_id);
        break;
      case 'payment_succeeded':
        await this.renewSubscription(event.subscription_id);
        break;
      case 'payment_failed':
        await this.handlePaymentFailure(event.subscription_id);
        break;
      case 'subscription_cancelled':
        await this.processCancellation(event.subscription_id);
        break;
      case 'iap_purchase_validated':
        await this.processQonversionIAP(event.subscription_id, event.receipt_data);
        break;
      case 'iap_subscription_renewed':
        await this.syncQonversionRenewal(event.subscription_id);
        break;
    }
  }

  async processQonversionIAP(subscriptionId: string, receiptData: any): Promise<void> {
    // Validate IAP through Qonversion
    const validation = await this.qonversion.validatePurchase(receiptData);
    
    if (validation.isValid) {
      await this.activateSubscription(subscriptionId, {
        processor: 'qonversion',
        store: validation.store, // 'app_store' or 'play_store'
        originalTransactionId: validation.originalTransactionId,
        expirationDate: validation.expirationDate
      });
      
      // Sync with PostHog analytics
      await this.analytics.trackIAPConversion(subscriptionId, validation);
    }
  }

  async syncQonversionRenewal(subscriptionId: string): Promise<void> {
    // Handle automatic renewal from mobile stores
    const qonversionData = await this.qonversion.getSubscriptionStatus(subscriptionId);
    await this.updateSubscriptionStatus(subscriptionId, qonversionData);
  }
}
```

### Qonversion IAP Integration

**Mobile Store Integration:**
```typescript
class QonversionService {
  async validatePurchase(receiptData: MobileReceipt): Promise<ValidationResult> {
    const response = await this.qonversion.api.validateReceipt({
      receipt: receiptData.receipt,
      platform: receiptData.platform, // 'ios' or 'android'
      userId: receiptData.userId
    });

    return {
      isValid: response.status === 'valid',
      store: receiptData.platform === 'ios' ? 'app_store' : 'play_store',
      originalTransactionId: response.originalTransactionId,
      expirationDate: response.expirationDate,
      productId: response.productId,
      entitlements: response.entitlements
    };
  }

  async getSubscriptionStatus(userId: string): Promise<QonversionStatus> {
    return await this.qonversion.api.getUser(userId);
  }

  async syncToWebSubscription(qonversionUserId: string, webUserId: string): Promise<void> {
    // Sync mobile IAP subscription to web platform subscription
    const mobileStatus = await this.getSubscriptionStatus(qonversionUserId);
    await this.subscriptionService.syncPlatformSubscription(webUserId, mobileStatus);
  }
}

interface MobileReceipt {
  receipt: string;
  platform: 'ios' | 'android';
  userId: string;
  productId: string;
}

interface ValidationResult {
  isValid: boolean;
  store: 'app_store' | 'play_store';
  originalTransactionId: string;
  expirationDate: string;
  productId: string;
  entitlements: string[];
}
```

## User Experience Design

### Subscription Dashboard

**Dashboard Components:**
- **Current Plan**: Plan name, price, next billing date
- **Usage Metrics**: Current usage vs. plan limits
- **Payment Method**: Default payment method with update option
- **Billing History**: Recent charges and invoices
- **Plan Comparison**: Easy upgrade/downgrade options

**React Component Example:**
```tsx
import React from 'react';
import { useSubscription } from '@shared-hooks/useSubscription';

interface SubscriptionDashboardProps {
  userId: string;
}

export const SubscriptionDashboard: React.FC<SubscriptionDashboardProps> = ({ userId }) => {
  const { subscription, loading, error } = useSubscription(userId);

  if (loading) return <SubscriptionSkeleton />;
  if (error) return <ErrorMessage error={error} />;

  return (
    <div className="subscription-dashboard">
      <div className="current-plan">
        <h2>{subscription.plan.name}</h2>
        <p className="price">${subscription.plan.price}/month</p>
        <p className="next-billing">Next billing: {subscription.next_billing_date}</p>
      </div>

      <div className="usage-metrics">
        <UsageProgress 
          usage={subscription.usage} 
          limits={subscription.plan.limits} 
        />
      </div>

      <div className="payment-method">
        <PaymentMethodCard 
          paymentMethod={subscription.payment_method}
          onUpdate={handlePaymentMethodUpdate}
        />
      </div>

      <div className="billing-history">
        <BillingHistory 
          subscriptionId={subscription.id}
          limit={5}
        />
      </div>

      <div className="plan-actions">
        <PlanComparison 
          currentPlan={subscription.plan}
          onUpgrade={handleUpgrade}
          onDowngrade={handleDowngrade}
        />
        <CancelSubscriptionButton 
          subscriptionId={subscription.id}
          onCancel={handleCancellation}
        />
      </div>
    </div>
  );
};
```

### Billing Management

**Billing Features:**
- **Invoice Download**: PDF invoices for all charges
- **Payment Method Management**: Add, update, remove payment methods
- **Billing Address**: Manage billing information for tax compliance
- **Tax Information**: Display applicable taxes and VAT

### Cancellation Flow

**FTC-Compliant Cancellation Process:**
1. **Easy Access**: Prominent "Cancel Subscription" button in Account Center
2. **Confirmation Page**: Review cancellation terms and effective date
3. **Reason Collection**: Optional feedback collection (cannot be required)
4. **Final Confirmation**: Clear "Confirm Cancellation" button
5. **Email Confirmation**: Immediate confirmation email

**Cancellation Component:**
```tsx
export const CancellationFlow: React.FC<CancellationFlowProps> = ({ 
  subscriptionId, 
  onComplete 
}) => {
  const [step, setStep] = useState<'review' | 'confirm' | 'complete'>('review');
  const [reason, setReason] = useState('');

  const handleCancellation = async () => {
    try {
      const result = await cancelSubscription(subscriptionId, reason);
      setStep('complete');
      onComplete(result);
    } catch (error) {
      // Handle cancellation error
    }
  };

  return (
    <div className="cancellation-flow">
      {step === 'review' && (
        <CancellationReview 
          subscription={subscription}
          onContinue={() => setStep('confirm')}
          onCancel={() => onComplete(null)}
        />
      )}
      
      {step === 'confirm' && (
        <CancellationConfirmation 
          subscription={subscription}
          reason={reason}
          onReasonChange={setReason}
          onConfirm={handleCancellation}
          onBack={() => setStep('review')}
        />
      )}
      
      {step === 'complete' && (
        <CancellationComplete 
          cancellationResult={cancellationResult}
        />
      )}
    </div>
  );
};
```

## Implementation Details

### Frontend Components

**Shared Components (Next.js + Expo):**
```typescript
// packages/shared-components/src/subscription/
export { SubscriptionDashboard } from './SubscriptionDashboard';
export { PlanSelector } from './PlanSelector';
export { PaymentMethodManager } from './PaymentMethodManager';
export { BillingHistory } from './BillingHistory';
export { CancellationFlow } from './CancellationFlow';
export { UsageMetrics } from './UsageMetrics';
```

**Platform-Specific Implementations:**
```typescript
// Next.js specific
export const NextSubscriptionPage = () => {
  return (
    <AccountLayout>
      <SubscriptionDashboard userId={user.id} />
    </AccountLayout>
  );
};

// Expo specific
export const ExpoSubscriptionScreen = () => {
  return (
    <ScrollView>
      <SubscriptionDashboard userId={user.id} />
    </ScrollView>
  );
};
```

### Backend Services

**Subscription Service Architecture:**
```typescript
// microservices/subscription/src/services/
export class SubscriptionService {
  constructor(
    private db: DatabaseService,
    private payment: PaymentProcessor,
    private notifications: NotificationService,
    private analytics: AnalyticsService
  ) {}

  async createSubscription(userId: string, planId: string): Promise<Subscription> {
    // Create subscription with trial period
    // Set up payment method
    // Send confirmation email
    // Track analytics event
  }

  async cancelSubscription(userId: string, subscriptionId: string): Promise<CancellationResult> {
    // Validate ownership
    // Process cancellation
    // Send confirmation
    // Schedule service termination
  }

  async updateSubscription(subscriptionId: string, updates: SubscriptionUpdate): Promise<Subscription> {
    // Validate changes
    // Process prorated billing
    // Update subscription
    // Notify user of changes
  }
}
```

### API Endpoints

**RESTful API Design:**
```typescript
// GET /api/subscriptions - Get user's subscriptions
// POST /api/subscriptions - Create new subscription
// GET /api/subscriptions/:id - Get specific subscription
// PUT /api/subscriptions/:id - Update subscription
// DELETE /api/subscriptions/:id - Cancel subscription
// GET /api/subscriptions/:id/invoices - Get billing history
// POST /api/subscriptions/:id/payment-method - Update payment method
```

## Revenue Analytics

**PostHog-Powered Revenue Insights:**
- **Monthly Recurring Revenue (MRR)**: Real-time subscription revenue tracking
- **Customer Lifetime Value (CLV)**: Predictive revenue modeling per cohort
- **Conversion Funnels**: Visual funnel analysis from trial to paid
- **Churn Analytics**: Identify patterns and reduce cancellations
- **Payment Method Performance**: Track success rates by processor

**PostHog Integration for Revenue Tracking:**
```typescript
class SubscriptionAnalytics {
  constructor(
    private posthog: PostHogClient,
    private polarAnalytics: PolarAnalyticsService
  ) {}

  async trackSubscriptionEvent(event: SubscriptionAnalyticsEvent): Promise<void> {
    // Core PostHog event tracking with revenue data
    await this.posthog.capture(event.user_id, event.event_name, {
      ...event.properties,
      revenue: event.amount,
      currency: event.currency,
      payment_processor: event.processor,
      subscription_tier: event.plan_name,
      billing_frequency: event.frequency
    });

    // Set user properties for cohort analysis
    await this.posthog.people.set(event.user_id, {
      subscription_status: event.status,
      subscription_plan: event.plan_name,
      total_revenue: event.lifetime_value,
      payment_method: event.payment_method
    });

    // Track funnel progression
    await this.trackFunnelStep(event);

    // Sync with Polar for additional revenue insights
    if (event.processor === 'polar') {
      await this.polarAnalytics.syncRevenue(event);
    }
  }

  async trackFunnelStep(event: SubscriptionAnalyticsEvent): Promise<void> {
    const funnelSteps = {
      trial_started: 'Trial Started',
      payment_method_added: 'Payment Method Added',
      trial_converted: 'Trial Converted',
      subscription_upgraded: 'Plan Upgraded',
      subscription_renewed: 'Subscription Renewed'
    };

    if (funnelSteps[event.event_name]) {
      await this.posthog.capture(event.user_id, 'Subscription Funnel', {
        step: funnelSteps[event.event_name],
        step_number: Object.keys(funnelSteps).indexOf(event.event_name) + 1,
        revenue_impact: event.amount
      });
    }
  }

  async generateRevenueReport(): Promise<RevenueReport> {
    // Use PostHog insights API for revenue metrics
    const insights = await this.posthog.insights.get({
      events: [
        { id: 'subscription_created', math: 'sum', math_property: 'revenue' },
        { id: 'subscription_renewed', math: 'sum', math_property: 'revenue' },
        { id: 'subscription_cancelled', math: 'count' }
      ],
      date_from: '-30d',
      breakdown: 'payment_processor'
    });

    return this.formatRevenueReport(insights);
  }
}

// Revenue Dashboard Integration
export const RevenueMetricsDashboard: React.FC = () => {
  const { data: metrics } = usePostHogInsight('revenue_metrics');

  return (
    <div className="revenue-dashboard">
      <MetricCard title="MRR" value={metrics.mrr} trend={metrics.mrr_trend} />
      <MetricCard title="LTV" value={metrics.ltv} trend={metrics.ltv_trend} />
      <FunnelVisualization data={metrics.funnel} />
      <ProcessorBreakdown data={metrics.by_processor} />
    </div>
  );
};
```

## Security & Privacy

**Data Protection:**
- **PCI DSS Compliance**: Secure payment data handling
- **Data Encryption**: Encrypt sensitive billing information
- **Access Controls**: Role-based access to billing data
- **Audit Logs**: Track all billing-related actions

**Privacy Compliance:**
- **GDPR**: Right to data portability and deletion
- **CCPA**: California consumer privacy rights
- **Data Retention**: Automatic deletion of old billing data
- **Consent Management**: Track and manage user consent

## Testing & Compliance

**Automated Testing:**
```typescript
describe('FTC Compliance Tests', () => {
  test('should display clear pricing information', async () => {
    const planPage = await renderPlanPage();
    expect(planPage.getByText(/\$\d+\.\d{2}\/month/)).toBeVisible();
    expect(planPage.getByText(/automatically renews/i)).toBeVisible();
    expect(planPage.getByText(/cancel anytime/i)).toBeVisible();
  });

  test('should allow easy cancellation', async () => {
    const dashboard = await renderSubscriptionDashboard();
    const cancelButton = dashboard.getByText(/cancel subscription/i);
    expect(cancelButton).toBeVisible();
    expect(cancelButton).not.toBeDisabled();
  });

  test('should send renewal notifications', async () => {
    await scheduleRenewalNotification(subscription);
    await advanceTime(6 * 24 * 60 * 60 * 1000); // 6 days
    expect(mockEmailService.send).toHaveBeenCalledWith(
      expect.objectContaining({
        subject: expect.stringContaining('renews in'),
        body: expect.stringContaining('cancel')
      })
    );
  });
});
```

**Compliance Checklist:**
- [ ] Clear and prominent pricing disclosure
- [ ] Express informed consent for auto-renewal
- [ ] Easy cancellation process
- [ ] Automatic renewal notifications
- [ ] Material change notifications
- [ ] Refund policy disclosure
- [ ] Terms of service accessibility
- [ ] Privacy policy compliance
- [ ] Payment security (PCI DSS)
- [ ] International compliance (GDPR, CCPA)

---

**Integration Note**: This subscription system is fully integrated with the Account Center authentication system, ensuring that users can manage their subscriptions regardless of which OAuth provider they use to sign in, while maintaining FTC compliance and providing a seamless user experience across all platforms.