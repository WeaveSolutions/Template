# Payments Service PostHog Analytics Module

This module provides comprehensive PostHog analytics integration for the Nexpo Payments service, including revenue tracking, subscription analytics, fraud detection, churn prediction, and PCI-compliant payment event monitoring.

## Features

### Analytics Capabilities
- **Revenue Tracking**: Transaction monitoring, revenue attribution, and payment success rates
- **Subscription Analytics**: Plan changes, billing cycles, and subscription lifecycle management
- **Fraud Detection**: Payment anomaly detection and security monitoring
- **Churn Prediction**: Payment-based churn indicators and retention analytics
- **Payment Provider Analytics**: Multi-provider performance comparison and optimization
- **Business Intelligence**: Revenue forecasting, cohort analysis, and financial reporting

### PostHog Resources Created
- Dedicated PostHog project for payments service
- Feature flags for payment flow optimization
- Revenue-based user cohorts for segmentation
- Custom actions for payment lifecycle events
- Financial analytics dashboard with revenue insights
- Webhook integration for real-time payment event processing

## Usage

### Basic Configuration

```hcl
module "payments_posthog" {
  source = "./modules/posthog"
  
  # Base configuration
  enable_posthog              = true
  project_name               = "nexpo"
  environment                = "prod"
  posthog_organization_id    = "your-posthog-org-id"
  
  # Payments-specific features
  enable_revenue_tracking      = true
  enable_subscription_analytics = true
  enable_fraud_detection      = true
  enable_churn_prediction     = true
  
  # Payment provider tracking
  track_stripe_events    = true
  track_paypal_events    = true
  track_apple_pay_events = true
  track_google_pay_events = true
  
  # Business intelligence
  enable_revenue_forecasting = true
  enable_cohort_analysis     = true
  
  common_tags = {
    Environment = "prod"
    Service     = "payments"
    Team        = "billing"
  }
}
```

### Advanced Configuration

```hcl
module "payments_posthog" {
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
  revenue_tracking_rollout    = 100
  subscription_analytics_rollout = 100
  
  # Privacy and compliance
  enable_pci_compliance = true
  data_retention_days   = 2555  # 7 years for financial data
  
  # Performance tuning
  batch_size         = 25  # Smaller batches for financial data
  flush_interval_ms  = 2000  # More frequent flushing
  
  # Feature toggles
  enable_revenue_milestones    = true
  enable_payment_provider_analytics = true
  enable_refund_analytics      = true
  enable_tax_analytics         = true
  
  # Integration settings
  integrate_with_api_gateway = true
  integrate_with_user        = true
  integrate_with_auth        = true
  
  # Business intelligence
  enable_ltv_calculation     = true
  enable_mrr_tracking        = true
  enable_churn_analysis      = true
  
  # Webhook configuration
  webhook_endpoint = "https://api.nexpo.com/webhooks/posthog/payments"
  webhook_secret   = var.payments_webhook_secret
  
  common_tags = var.common_tags
}
```

## Outputs

### Key Outputs

- `posthog_project_id`: PostHog project ID for payments service
- `posthog_project_api_key`: API key for payments service (sensitive)
- `payments_feature_flags`: Payment-specific feature flags
- `revenue_analytics_actions`: Analytics actions for revenue events
- `revenue_cohorts`: Revenue-based user segmentation
- `dashboard_url`: URL to payments analytics dashboard
- `environment_variables`: Environment variables for payments service integration
- `business_intelligence_config`: BI configuration for revenue analytics

### Example Output Usage

```hcl
# Use outputs in payments service deployment
resource "kubernetes_secret" "payments_posthog_config" {
  metadata {
    name = "payments-posthog-config"
  }
  
  data = module.payments_posthog.environment_variables
}

# Reference BI configuration
output "revenue_analytics_config" {
  description = "Revenue analytics configuration"
  value       = module.payments_posthog.business_intelligence_config
}
```

## Event Schema

### Payment Transaction Events

#### Payment Attempt
```json
{
  "event_name": "payment_attempted",
  "properties": {
    "user_id": "user-root-id",
    "transaction_id": "txn-12345",
    "payment_provider": "stripe|paypal|apple_pay|google_pay",
    "payment_method": "card|bank_transfer|digital_wallet",
    "amount": 29.99,
    "currency": "USD",
    "subscription_id": "sub-12345",
    "plan_id": "premium-monthly",
    "success": true,
    "error_code": "card_declined",
    "processing_time_ms": 1200,
    "country": "US",
    "ip_country": "US"
  }
}
```

#### Subscription Event
```json
{
  "event_name": "subscription_changed",
  "properties": {
    "user_id": "user-root-id",
    "subscription_id": "sub-12345",
    "change_type": "created|upgraded|downgraded|cancelled|renewed|paused|resumed",
    "from_plan": "basic-monthly",
    "to_plan": "premium-monthly",
    "from_amount": 9.99,
    "to_amount": 29.99,
    "currency": "USD",
    "billing_cycle": "monthly|yearly",
    "proration_amount": 20.00,
    "effective_date": "2024-01-15",
    "payment_provider": "stripe",
    "change_reason": "user_initiated|admin|failed_payment|trial_ended"
  }
}
```

#### Revenue Milestone
```json
{
  "event_name": "revenue_milestone_reached",
  "properties": {
    "user_id": "user-root-id",
    "milestone_type": "first_payment|ltv_threshold|annual_spending|premium_upgrade",
    "milestone_value": 1000.00,
    "currency": "USD",
    "time_to_milestone": 2592000,
    "total_transactions": 12,
    "average_transaction": 83.33,
    "subscription_plan": "premium-yearly"
  }
}
```

### Fraud Detection Events

#### Fraud Alert
```json
{
  "event_name": "fraud_alert_triggered",
  "properties": {
    "user_id": "user-root-id",
    "transaction_id": "txn-12345",
    "fraud_score": 85.5,
    "fraud_indicators": ["unusual_location", "high_velocity", "new_payment_method"],
    "risk_level": "low|medium|high|critical",
    "action_taken": "blocked|flagged|manual_review|allowed",
    "payment_provider": "stripe",
    "amount": 299.99,
    "currency": "USD"
  }
}
```

### Refund Events

#### Refund Processed
```json
{
  "event_name": "refund_processed",
  "properties": {
    "user_id": "user-root-id",
    "original_transaction_id": "txn-12345",
    "refund_id": "ref-67890",
    "refund_amount": 29.99,
    "refund_type": "full|partial|chargeback",
    "refund_reason": "customer_request|fraud|technical_issue|chargeback",
    "processing_time_hours": 24,
    "payment_provider": "stripe",
    "currency": "USD"
  }
}
```

## Feature Flags

### Available Feature Flags

1. **revenue-tracking** (`revenue_tracking`)
   - Controls revenue event tracking and analytics
   - Default: Enabled for all users
   - Includes transaction monitoring and revenue attribution

2. **subscription-analytics** (`subscription_analytics`)
   - Controls subscription lifecycle tracking
   - Default: Enabled for all users
   - Tracks plan changes, billing cycles, and renewals

3. **fraud-detection** (`fraud_detection`)
   - Controls fraud monitoring and alerting
   - Default: Enabled for all transactions
   - Includes risk scoring and anomaly detection

4. **churn-prediction** (`churn_prediction`)
   - Controls payment-based churn prediction
   - Default: Enabled for premium users
   - Analyzes payment patterns for churn indicators

5. **payment-provider-analytics** (`payment_provider_analytics`)
   - Controls multi-provider performance tracking
   - Default: Enabled for all providers
   - Compares success rates and costs across providers

### Feature Flag Usage in Application

```javascript
// Check feature flag in payments service
const isFraudDetectionEnabled = await posthog.isFeatureEnabled('fraud-detection');

if (isFraudDetectionEnabled) {
  // Calculate fraud score and track
  const fraudScore = calculateFraudScore(transaction);
  
  if (fraudScore > 75) {
    posthog.capture('fraud_alert_triggered', {
      user_id: transaction.userId,
      transaction_id: transaction.id,
      fraud_score: fraudScore,
      risk_level: 'high',
      action_taken: 'manual_review'
    });
  }
}
```

## Revenue Cohorts

### Revenue-Based Segmentation

1. **high_ltv_customers**: Customers with high lifetime value (>$1000)
2. **premium_subscribers**: Users with premium subscription plans
3. **at_risk_subscribers**: Users with failed payments or downgrades
4. **new_customers**: Users with first payment in last 30 days
5. **annual_subscribers**: Users on yearly billing cycles
6. **high_frequency_buyers**: Users with multiple transactions per month

### Cohort Criteria Examples

```hcl
# High LTV customers cohort
resource "posthog_cohort" "high_ltv_customers" {
  name = "High LTV Customers - ${var.environment}"
  description = "Customers with lifetime value over $1000"
  
  filters = {
    events = [
      {
        event = "revenue_milestone_reached"
        properties = [
          {
            key = "milestone_value"
            value = 1000
            operator = "gte"
          }
        ]
      }
    ]
  }
}

# At-risk subscribers cohort
resource "posthog_cohort" "at_risk_subscribers" {
  name = "At Risk Subscribers - ${var.environment}"
  description = "Subscribers with payment issues or downgrades"
  
  filters = {
    events = [
      {
        event = "payment_attempted"
        properties = [
          {
            key = "success"
            value = false
            operator = "exact"
          }
        ]
      },
      {
        event = "subscription_changed"
        properties = [
          {
            key = "change_type"
            value = "downgraded"
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

The module generates environment variables for payments service integration:

```bash
# PostHog Configuration
POSTHOG_PROJECT_API_KEY=phc_payments_service_key
POSTHOG_HOST=https://app.posthog.com
POSTHOG_PROJECT_ID=payments-service-project-id
POSTHOG_SERVICE_NAME=payments

# Feature Flags
POSTHOG_FEATURE_FLAGS_ENABLED=true
POSTHOG_REVENUE_TRACKING=true
POSTHOG_SUBSCRIPTION_ANALYTICS=true
POSTHOG_FRAUD_DETECTION=true
POSTHOG_CHURN_PREDICTION=true
POSTHOG_PAYMENT_PROVIDER_ANALYTICS=true

# Compliance Settings
POSTHOG_PCI_COMPLIANCE=true
POSTHOG_ANONYMIZE_IPS=true
POSTHOG_DATA_RETENTION_DAYS=2555

# Performance Settings
POSTHOG_SAMPLING_RATE=1.0
POSTHOG_BATCH_SIZE=25
POSTHOG_FLUSH_INTERVAL=2000

# Business Intelligence
POSTHOG_LTV_CALCULATION=true
POSTHOG_MRR_TRACKING=true
POSTHOG_REVENUE_FORECASTING=true

# Cross-service Integration
POSTHOG_PARENT_PROJECT_ID=api-gateway-project-id
POSTHOG_USER_PROJECT_ID=user-service-project-id
POSTHOG_AUTH_PROJECT_ID=auth-service-project-id
```

### Node.js Integration Example

```javascript
const PostHog = require('posthog-node');

const posthog = new PostHog(process.env.POSTHOG_PROJECT_API_KEY, {
  host: process.env.POSTHOG_HOST,
  flushAt: parseInt(process.env.POSTHOG_BATCH_SIZE),
  flushInterval: parseInt(process.env.POSTHOG_FLUSH_INTERVAL)
});

// Track payment attempt
function trackPaymentAttempt(transaction) {
  const isEnabled = process.env.POSTHOG_REVENUE_TRACKING === 'true';
  
  if (isEnabled) {
    posthog.capture({
      distinctId: transaction.userId,
      event: 'payment_attempted',
      properties: {
        transaction_id: transaction.id,
        payment_provider: transaction.provider,
        amount: transaction.amount,
        currency: transaction.currency,
        success: transaction.success,
        error_code: transaction.errorCode,
        processing_time_ms: transaction.processingTime
      }
    });
  }
}

// Calculate and track LTV
function trackLTVMilestone(userId, totalSpent, transactionCount) {
  if (totalSpent >= 1000) {
    posthog.capture({
      distinctId: userId,
      event: 'revenue_milestone_reached',
      properties: {
        milestone_type: 'ltv_threshold',
        milestone_value: totalSpent,
        total_transactions: transactionCount,
        average_transaction: totalSpent / transactionCount
      }
    });
  }
}
```

## PCI Compliance and Security

### Data Protection Features

- **PCI DSS Compliance**: No sensitive payment data tracked
- **Tokenization**: Only payment tokens and references tracked
- **Encryption**: All data encrypted in transit and at rest
- **Access Controls**: Role-based access to financial analytics
- **Audit Logging**: Complete audit trail for all payment events

### Secure Event Tracking

```javascript
// PCI-compliant payment tracking
function trackPaymentSecurely(transaction) {
  // Only track non-sensitive data
  const sanitizedProperties = {
    transaction_id: transaction.id,
    payment_provider: transaction.provider,
    amount: transaction.amount,
    currency: transaction.currency,
    success: transaction.success,
    // Never track: card numbers, CVV, PII
    card_last_four: transaction.cardLastFour, // Only last 4 digits
    card_type: transaction.cardType,          // visa/mastercard/etc
    country: transaction.country
  };
  
  posthog.capture({
    distinctId: transaction.userId,
    event: 'payment_attempted',
    properties: sanitizedProperties
  });
}
```

## Revenue Analytics and Business Intelligence

### Key Metrics Tracked

1. **Monthly Recurring Revenue (MRR)**: Predictable subscription revenue
2. **Annual Recurring Revenue (ARR)**: Yearly subscription revenue
3. **Customer Lifetime Value (LTV)**: Total customer value prediction
4. **Churn Rate**: Subscription cancellation rates
5. **Average Revenue Per User (ARPU)**: Revenue per customer metrics

### Revenue Forecasting

```javascript
// Track MRR changes
function trackMRRChange(subscription, previousMRR, newMRR) {
  posthog.capture({
    distinctId: subscription.userId,
    event: 'mrr_changed',
    properties: {
      subscription_id: subscription.id,
      previous_mrr: previousMRR,
      new_mrr: newMRR,
      mrr_delta: newMRR - previousMRR,
      change_type: subscription.changeType,
      plan: subscription.plan
    }
  });
}

// Calculate and track LTV
function calculateLTV(customer) {
  const monthlyRevenue = customer.subscription.amount;
  const averageLifespanMonths = 24; // Based on historical data
  const ltv = monthlyRevenue * averageLifespanMonths;
  
  posthog.identify({
    distinctId: customer.id,
    properties: {
      ltv_estimate: ltv,
      monthly_revenue: monthlyRevenue,
      subscription_start: customer.subscription.startDate
    }
  });
  
  return ltv;
}
```

## Fraud Detection and Risk Management

### Fraud Scoring Algorithm

```javascript
class FraudDetector {
  calculateFraudScore(transaction) {
    let score = 0;
    
    // Geographic risk
    if (transaction.country !== transaction.user.country) {
      score += 20;
    }
    
    // Velocity check
    if (transaction.user.transactionsLast24h > 5) {
      score += 30;
    }
    
    // Amount anomaly
    if (transaction.amount > transaction.user.averageTransaction * 5) {
      score += 25;
    }
    
    // New payment method
    if (transaction.isNewPaymentMethod) {
      score += 15;
    }
    
    return Math.min(score, 100);
  }
  
  trackFraudEvent(transaction, fraudScore) {
    posthog.capture({
      distinctId: transaction.userId,
      event: 'fraud_alert_triggered',
      properties: {
        transaction_id: transaction.id,
        fraud_score: fraudScore,
        risk_level: this.getRiskLevel(fraudScore),
        action_taken: this.getActionTaken(fraudScore)
      }
    });
  }
  
  getRiskLevel(score) {
    if (score >= 80) return 'critical';
    if (score >= 60) return 'high';
    if (score >= 40) return 'medium';
    return 'low';
  }
}
```

## Dashboard and Monitoring

### Payments Analytics Dashboard

The payments analytics dashboard includes:

1. **Revenue Overview**: MRR, ARR, and revenue trends
2. **Subscription Analytics**: Plan distribution and churn rates
3. **Payment Success Rates**: Provider performance comparison
4. **Fraud Detection**: Risk scores and blocked transactions
5. **Cohort Analysis**: Revenue cohort performance
6. **Churn Prediction**: At-risk customer identification

### Real-time Monitoring

```hcl
# Webhook for payment events
webhook_endpoint = "https://api.nexpo.com/webhooks/posthog/payments"
webhook_secret   = var.payments_webhook_secret
```

### Critical Alerts

Set up alerts for:
- High fraud score transactions
- Payment gateway failures
- Subscription churn spikes
- Revenue milestone achievements
- Failed payment increases

## Cost Optimization

### Efficient Revenue Tracking

```javascript
// Batch revenue events for cost efficiency
class RevenueEventBatcher {
  constructor() {
    this.events = [];
    this.flushInterval = setInterval(() => this.flush(), 30000); // 30 seconds
  }
  
  addRevenueEvent(event) {
    this.events.push(event);
    
    // Flush immediately for high-value transactions
    if (event.properties.amount > 1000) {
      this.flush();
    }
  }
  
  flush() {
    if (this.events.length === 0) return;
    
    // Send events in batch
    this.events.forEach(event => {
      posthog.capture(event);
    });
    
    this.events = [];
  }
}
```

### Sampling Strategy for High Volume

```hcl
# Different sampling for different transaction values
module "payments_posthog" {
  # High-value transactions: 100% tracking
  high_value_threshold = 100.00
  high_value_sampling = 1.0
  
  # Medium-value transactions: 50% sampling
  medium_value_sampling = 0.5
  
  # Low-value transactions: 10% sampling
  low_value_sampling = 0.1
}
```

## Version Compatibility

- **Terraform**: >= 0.14
- **PostHog Provider**: >= 0.1.0
- **AWS Provider**: >= 3.0 (for SSM parameters)
- **PostHog**: Cloud or self-hosted >= 1.30.0
- **PCI DSS**: Compliant configuration

## Troubleshooting

### Common Issues

1. **Revenue Metrics Discrepancies**
   - Verify transaction tracking accuracy
   - Check currency conversion handling
   - Ensure refund events are properly tracked

2. **Fraud Detection False Positives**
   - Adjust fraud scoring algorithm
   - Review geographic risk rules
   - Tune velocity thresholds

3. **PCI Compliance Violations**
   - Verify no sensitive data is tracked
   - Check data masking implementation
   - Ensure proper access controls

### Debug Mode

```hcl
module "payments_posthog" {
  # ... other configuration
  enable_debug_logging = true
  enable_transaction_logging = true
  enable_fraud_debug = true
}
```

## Contributing

To extend this module:

1. Add new payment provider integrations
2. Implement advanced fraud detection models
3. Create additional revenue analytics cohorts
4. Extend business intelligence calculations
5. Add new dashboard widgets for financial insights

## Support

For questions and support:
- Review the shared PostHog module documentation
- Check PCI compliance guidelines
- Refer to payment provider documentation
- Contact the billing team for revenue analytics questions
- Reach out to security team for fraud detection configurations
