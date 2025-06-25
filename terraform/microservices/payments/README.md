# Payments Service Infrastructure

This directory contains the Terraform configuration for deploying the Payments Service, which handles subscription management, billing, and payment processing using multiple providers for the Nexpo application.

## 💳 Service Overview

The Payments Service provides:
- **Multi-Provider Support**: Polar, Stripe, PayPal, Apple Pay, Google Pay integration
- **Subscription Management**: Plans, billing cycles, upgrades/downgrades
- **Payment Processing**: One-time and recurring payments
- **Revenue Analytics**: MRR, churn, LTV calculations with PostHog
- **Billing Automation**: Invoicing, dunning, tax calculations
- **Mobile IAP**: iOS App Store and Google Play Store integration
- **Webhook Processing**: Real-time payment event handling

## 🏗️ Architecture

```
┌─────────────────┐     ┌──────────────┐     ┌─────────────────┐
│   Mobile Apps   │────▶│  API Gateway │────▶│   Payments     │
│ (iOS/Android)   │     └──────────────┘     │    Service      │
└─────────────────┘                          └─────────────────┘
                                                       │
                        ┌──────────────────────────────┼──────────────────────────────┐
                        │                              │                              │
                        ▼                              ▼                              ▼
                ┌──────────────┐              ┌──────────────┐              ┌──────────────┐
                │   Payment    │              │   Billing    │              │   Database   │
                │  Providers   │              │   Engine     │              │  (Multi-DB)  │
                └──────────────┘              └──────────────┘              └──────────────┘
                        │                              │
          ┌─────────────┼─────────────┐                │
          │      │      │      │      │                │
          ▼      ▼      ▼      ▼      ▼                ▼
      ┌──────┐┌──────┐┌──────┐┌──────┐┌──────┐  ┌──────────────┐
      │Polar ││Stripe││PayPal││Apple ││Google│  │ Notification │
      │ API  ││ API  ││ API  ││ Pay  ││ Pay  │  │   Service    │
      └──────┘└──────┘└──────┘└──────┘└──────┘  └──────────────┘
```

## 📁 Directory Structure

```
payments/
├── main.tf                    # Main service configuration
├── variables.tf               # Service variables
├── outputs.tf                # Service outputs
├── provider-specific/        # Cloud-specific configs
│   ├── aws.tf               # ECS, SQS, Lambda, API Gateway
│   ├── gcp.tf               # Cloud Run, Pub/Sub, Cloud Functions
│   ├── azure.tf             # Container Apps, Service Bus, Functions
│   ├── oci.tf               # Container Instances, Streaming
│   ├── ibm.tf               # Code Engine, Event Streams
│   └── cloudflare.tf        # Workers, Queues, Durable Objects
├── payment-providers/        # Payment provider integrations
│   ├── polar.tf             # Polar subscription platform
│   ├── stripe.tf            # Stripe payment processing
│   ├── paypal.tf            # PayPal payment processing
│   ├── apple-pay.tf         # Apple Pay integration
│   └── google-pay.tf        # Google Pay integration
├── billing/                  # Billing and subscription logic
│   ├── plans.tf             # Subscription plans configuration
│   ├── pricing.tf           # Pricing tiers and models
│   ├── invoicing.tf         # Invoice generation and delivery
│   ├── dunning.tf           # Failed payment handling
│   └── taxes.tf             # Tax calculation and compliance
├── mobile-iap/               # Mobile in-app purchases
│   ├── app-store.tf         # iOS App Store integration
│   ├── play-store.tf        # Google Play Store integration
│   ├── receipt-validation.tf # Receipt verification
│   └── subscription-sync.tf # IAP subscription synchronization
├── analytics/                # Revenue and payment analytics
│   ├── revenue-tracking.tf  # MRR, ARR, churn calculations
│   ├── customer-ltv.tf      # Customer lifetime value
│   ├── cohort-analysis.tf   # Cohort revenue analysis
│   └── reporting.tf         # Financial reporting
├── compliance/               # Regulatory compliance
│   ├── pci-dss.tf          # PCI DSS compliance
│   ├── gdpr.tf             # GDPR data handling
│   ├── tax-compliance.tf   # Tax regulations (VAT, GST)
│   └── audit-logs.tf       # Payment audit logging
├── monitoring/               # Observability configurations
│   ├── dashboards.tf        # Payment dashboards
│   ├── alerts.tf            # Payment failure alerts
│   └── fraud-detection.tf   # Fraud monitoring
└── README.md                # This file
```

## 🚀 Deployment

### Prerequisites
- Terraform >= 1.0
- Cloud provider CLI configured
- Payment provider accounts (Polar, Stripe, PayPal, Apple Pay, Google Pay)
- Mobile app store developer accounts
- SSL certificates for webhook endpoints

### Quick Start
```bash
# Navigate to payments service
cd terraform/microservices/payments

# Initialize Terraform
terraform init

# Plan deployment
terraform plan -var-file="../../terraform.tfvars"

# Deploy service
terraform apply -var-file="../../terraform.tfvars"
```

### Environment-Specific Deployment
```bash
# Development (sandbox mode)
terraform apply -var="environment=dev" -var="use_sandbox=true"

# Production (live payments)
terraform apply -var="environment=prod" -var="use_sandbox=false"
```

## 🔧 Configuration

### Required Variables
```hcl
# Service Configuration
service_name     = "payments-service"
environment      = "dev"
instance_count   = 3
cpu_units        = 1024
memory_mb        = 2048

# Payment Provider Feature Flags
ENABLE_POLAR=true
ENABLE_STRIPE=true
ENABLE_PAYPAL=true
ENABLE_APPLE_PAY=true
ENABLE_GOOGLE_PAY=true
ENABLE_MOBILE_IAP=true
ENABLE_CRYPTO_PAYMENTS=false

# Polar Configuration
POLAR_ACCESS_TOKEN=your-polar-access-token
POLAR_ORGANIZATION_ID=your-polar-org-id
POLAR_WEBHOOK_SECRET=your-polar-webhook-secret
POLAR_SANDBOX_MODE=true

# Stripe Configuration
STRIPE_PUBLISHABLE_KEY=pk_test_your-stripe-publishable-key
STRIPE_SECRET_KEY=sk_test_your-stripe-secret-key
STRIPE_WEBHOOK_SECRET=whsec_your-stripe-webhook-secret
STRIPE_CONNECT_CLIENT_ID=ca_your-stripe-connect-client-id

# PayPal Configuration
PAYPAL_CLIENT_ID=your-paypal-client-id
PAYPAL_CLIENT_SECRET=your-paypal-client-secret
PAYPAL_WEBHOOK_ID=your-paypal-webhook-id
PAYPAL_SANDBOX_MODE=true

# Apple Pay Configuration
APPLE_PAY_MERCHANT_ID=merchant.com.example
APPLE_PAY_MERCHANT_CERTIFICATE=path/to/apple-pay-cert.pem
APPLE_PAY_PROCESSING_CERTIFICATE=path/to/processing-cert.pem

# Google Pay Configuration
GOOGLE_PAY_MERCHANT_ID=your-google-pay-merchant-id
GOOGLE_PAY_GATEWAY_MERCHANT_ID=your-gateway-merchant-id

# Mobile IAP Configuration
APPLE_SHARED_SECRET=your-apple-shared-secret
APPLE_BUNDLE_ID=com.example.app
GOOGLE_PLAY_SERVICE_ACCOUNT_KEY=path/to/google-play-key.json
GOOGLE_PLAY_PACKAGE_NAME=com.example.app
```

### Subscription Plans Configuration
```hcl
# Subscription plans
subscription_plans = {
  basic = {
    name = "Basic Plan"
    price_monthly = 9.99
    price_yearly = 99.99
    features = ["feature_1", "feature_2"]
    trial_days = 7
    polar_product_id = "prod_basic"
    stripe_price_id = "price_basic_monthly"
    paypal_plan_id = "P-basic_monthly"
    apple_pay_supported = true
    google_pay_supported = true
  }
  
  premium = {
    name = "Premium Plan"
    price_monthly = 19.99
    price_yearly = 199.99
    features = ["feature_1", "feature_2", "feature_3", "feature_4"]
    trial_days = 14
    polar_product_id = "prod_premium"
    stripe_price_id = "price_premium_monthly"
    paypal_plan_id = "P-premium_monthly"
    apple_pay_supported = true
    google_pay_supported = true
  }
  
  enterprise = {
    name = "Enterprise Plan"
    price_monthly = 49.99
    price_yearly = 499.99
    features = ["all_features"]
    trial_days = 30
    polar_product_id = "prod_enterprise"
    stripe_price_id = "price_enterprise_monthly"
    paypal_plan_id = "P-enterprise_monthly"
    apple_pay_supported = true
    google_pay_supported = true
  }
}
```

### Payment Provider Configuration
```hcl
# Provider-specific settings
payment_providers = {
  polar = {
    enabled = true
    webhook_url = "https://api.example.com/webhooks/polar"
    supported_currencies = ["USD", "EUR", "GBP"]
    tax_calculation = true
  }
  
  stripe = {
    enabled = true
    webhook_url = "https://api.example.com/webhooks/stripe"
    supported_currencies = ["USD", "EUR", "GBP", "CAD", "AUD"]
    automatic_tax = true
    payment_methods = ["card", "sepa_debit", "ideal", "sofort"]
  }
  
  paypal = {
    enabled = true
    webhook_url = "https://api.example.com/webhooks/paypal"
    supported_currencies = ["USD", "EUR", "GBP", "CAD", "AUD"]
    express_checkout = true
  }
  
  apple_pay = {
    enabled = true
    supported_networks = ["visa", "mastercard", "amex", "discover"]
    merchant_capabilities = ["supports3DS", "supportsDebit", "supportsCredit"]
  }
  
  google_pay = {
    enabled = true
    gateway = "stripe"
    allowed_payment_methods = ["CARD", "TOKENIZED_CARD"]
    supported_networks = ["VISA", "MASTERCARD", "AMEX", "DISCOVER"]
  }
}
```

## 🔌 API Endpoints

### Subscription Management APIs
```
GET    /api/subscriptions              # List user subscriptions
POST   /api/subscriptions              # Create new subscription
PUT    /api/subscriptions/:id          # Update subscription
DELETE /api/subscriptions/:id          # Cancel subscription
POST   /api/subscriptions/:id/pause    # Pause subscription
POST   /api/subscriptions/:id/resume   # Resume subscription
```

### Payment Processing APIs
```
POST   /api/payments/intent            # Create payment intent
POST   /api/payments/confirm           # Confirm payment
GET    /api/payments/:id               # Get payment details
POST   /api/payments/refund            # Process refund
GET    /api/payments/methods           # List payment methods
```

### Mobile IAP APIs
```
POST   /api/iap/validate              # Validate App Store/Play Store receipt
POST   /api/iap/sync                  # Sync IAP subscription status
GET    /api/iap/products              # Get available IAP products
POST   /api/iap/restore               # Restore previous purchases
```

### Billing APIs
```
GET    /api/billing/invoices          # List invoices
GET    /api/billing/invoices/:id      # Get invoice details
POST   /api/billing/invoices/:id/pay  # Pay invoice
GET    /api/billing/upcoming          # Get upcoming invoice
POST   /api/billing/portal            # Create customer portal session
```

### Analytics APIs
```
GET    /api/analytics/revenue         # Revenue metrics (MRR, ARR)
GET    /api/analytics/churn           # Churn analysis
GET    /api/analytics/ltv             # Customer lifetime value
GET    /api/analytics/cohorts         # Cohort analysis
GET    /api/analytics/conversion      # Conversion funnel
```

## 💰 Payment Flow Examples

### Web Subscription Flow (Stripe)
```javascript
// 1. Create payment intent
const response = await fetch('/api/payments/intent', {
  method: 'POST',
  headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify({
    plan_id: 'premium_monthly',
    payment_method: 'card'
  })
});

const { client_secret } = await response.json();

// 2. Confirm payment with Stripe Elements
const { error } = await stripe.confirmCardPayment(client_secret, {
  payment_method: {
    card: cardElement,
    billing_details: { name: 'Customer Name' }
  }
});

// 3. Handle result
if (error) {
  console.error('Payment failed:', error);
} else {
  console.log('Payment succeeded!');
}
```

### Mobile IAP Flow (iOS)
```swift
// 1. Fetch available products
let productIds = Set(["basic_monthly", "premium_monthly", "enterprise_monthly"])
let request = SKProductsRequest(productIdentifiers: productIds)
request.delegate = self
request.start()

// 2. Purchase product
func purchase(product: SKProduct) {
    let payment = SKPayment(product: product)
    SKPaymentQueue.default().add(payment)
}

// 3. Validate receipt with backend
func validateReceipt(transactionId: String, receiptData: Data) {
    let request = URLRequest(url: URL(string: "https://api.example.com/api/iap/validate")!)
    // Send receipt data to backend for validation
}
```

### Polar Subscription Flow
```javascript
// 1. Create checkout session
const response = await fetch('/api/polar/checkout', {
  method: 'POST',
  headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify({
    product_id: 'prod_premium',
    success_url: 'https://app.example.com/success',
    cancel_url: 'https://app.example.com/cancel'
  })
});

const { checkout_url } = await response.json();

// 2. Redirect to Polar checkout
window.location.href = checkout_url;
```

## 📊 Revenue Analytics

### MRR Calculation
```sql
-- Monthly Recurring Revenue calculation
WITH monthly_revenue AS (
    SELECT 
        DATE_TRUNC('month', created_at) as month,
        SUM(
            CASE 
                WHEN billing_interval = 'monthly' THEN amount
                WHEN billing_interval = 'yearly' THEN amount / 12
                ELSE 0
            END
        ) as mrr
    FROM subscriptions 
    WHERE status = 'active'
    GROUP BY DATE_TRUNC('month', created_at)
)
SELECT 
    month,
    mrr,
    LAG(mrr) OVER (ORDER BY month) as previous_mrr,
    ((mrr - LAG(mrr) OVER (ORDER BY month)) / LAG(mrr) OVER (ORDER BY month)) * 100 as growth_rate
FROM monthly_revenue
ORDER BY month DESC;
```

### Churn Analysis
```sql
-- Customer churn rate calculation
WITH cohorts AS (
    SELECT 
        user_id,
        DATE_TRUNC('month', created_at) as cohort_month,
        created_at
    FROM subscriptions
    WHERE status IN ('active', 'cancelled')
),
cohort_data AS (
    SELECT 
        cohort_month,
        COUNT(DISTINCT user_id) as cohort_size,
        COUNT(DISTINCT CASE WHEN status = 'cancelled' THEN user_id END) as churned_users
    FROM cohorts c
    JOIN subscriptions s ON c.user_id = s.user_id
    GROUP BY cohort_month
)
SELECT 
    cohort_month,
    cohort_size,
    churned_users,
    ROUND((churned_users::float / cohort_size) * 100, 2) as churn_rate
FROM cohort_data
ORDER BY cohort_month DESC;
```

### Customer LTV
```sql
-- Customer Lifetime Value calculation
WITH customer_metrics AS (
    SELECT 
        user_id,
        SUM(amount) as total_revenue,
        COUNT(*) as total_payments,
        AVG(amount) as avg_payment,
        MAX(created_at) - MIN(created_at) as customer_lifespan,
        EXTRACT(days FROM MAX(created_at) - MIN(created_at)) / 30.0 as months_active
    FROM payments 
    WHERE status = 'succeeded'
    GROUP BY user_id
)
SELECT 
    AVG(total_revenue) as avg_ltv,
    AVG(avg_payment) as avg_payment_value,
    AVG(months_active) as avg_customer_lifespan_months,
    AVG(total_revenue / NULLIF(months_active, 0)) as avg_monthly_value
FROM customer_metrics
WHERE months_active > 0;
```

## 🔒 Security Features

### PCI DSS Compliance
- Secure card data handling (never store card numbers)
- Encrypted data transmission (TLS 1.2+)
- Regular security scans and penetration testing
- Access controls and audit logging

### Fraud Detection
```python
# Fraud detection rules
fraud_rules = {
    "velocity_checks": {
        "max_transactions_per_hour": 10,
        "max_amount_per_hour": 1000,
        "max_failed_attempts": 3
    },
    
    "geographic_checks": {
        "block_high_risk_countries": True,
        "require_3ds_for_international": True
    },
    
    "behavioral_analysis": {
        "unusual_purchase_patterns": True,
        "device_fingerprinting": True,
        "ip_reputation_check": True
    }
}
```

### Data Protection
```python
# Data encryption and protection
security_config = {
    "encryption": {
        "at_rest": True,
        "in_transit": True,
        "key_rotation_days": 90
    },
    
    "pii_handling": {
        "tokenization": True,
        "data_masking": True,
        "retention_days": 2555  # 7 years for financial records
    },
    
    "audit_logging": {
        "all_payment_events": True,
        "admin_actions": True,
        "data_access": True
    }
}
```

## 📈 Monitoring & Observability

### Health Checks
```
GET /health              # Basic health check
GET /health/providers    # Payment provider connectivity
GET /health/database     # Database connectivity
GET /metrics            # Prometheus metrics
```

### Key Metrics
- **Payment Success Rate**: Successful vs failed payments
- **Revenue Metrics**: MRR, ARR, churn rate, LTV
- **Provider Performance**: Response times, error rates
- **Subscription Metrics**: New subscriptions, cancellations, upgrades

### Alerting Rules
```yaml
# High payment failure rate
- alert: HighPaymentFailureRate
  expr: (failed_payments / total_payments) > 0.05
  for: 5m
  labels:
    severity: critical
  annotations:
    summary: "High payment failure rate detected"

# Revenue drop
- alert: RevenueDropAlert
  expr: (current_mrr - previous_mrr) / previous_mrr < -0.1
  for: 1h
  labels:
    severity: warning
  annotations:
    summary: "Significant revenue drop detected"
```

## 🔄 Integration Points

### Internal Services
- **User Service**: Customer account management
- **Notifications Service**: Payment confirmations, failed payment alerts
- **Analytics Service**: Revenue reporting and insights
- **AI Advisor Service**: Pricing optimization recommendations

### External Integrations
- **Payment Providers**: Polar, Stripe, PayPal, Apple Pay, Google Pay
- **Mobile Platforms**: iOS App Store, Google Play Store
- **Tax Services**: TaxJar, Avalara for tax calculation
- **Accounting**: QuickBooks, Xero for financial reporting
- **CRM**: Salesforce, HubSpot for customer data sync

## 🚦 Deployment Strategies

### Blue-Green Deployment
```bash
# Deploy to green environment
terraform apply -var="deployment_slot=green"

# Test payment functionality
curl -X POST https://payments-green.example.com/api/payments/test

# Switch traffic
terraform apply -var="active_slot=green"
```

### Feature Flag Rollout
```bash
# Enable new payment provider gradually
terraform apply -var="enable_new_provider=true" -var="new_provider_traffic_percent=10"
terraform apply -var="new_provider_traffic_percent=50"
terraform apply -var="new_provider_traffic_percent=100"
```

## 🛠️ Development

### Local Development
```bash
# Start dependencies
docker-compose up -d postgres redis

# Set environment variables
export STRIPE_SECRET_KEY="sk_test_..."
export POLAR_ACCESS_TOKEN="polar_..."
export PAYPAL_CLIENT_ID="your-paypal-client-id"

# Run payments service
npm run dev
```

### Testing
```bash
# Unit tests
npm run test

# Integration tests with payment providers
npm run test:integration

# End-to-end payment flow tests
npm run test:e2e

# Load testing
npm run test:load
```

## 🆘 Troubleshooting

### Common Issues

**Payment Failures**
```bash
# Check payment provider status
curl https://status.stripe.com/api/v2/status.json
curl https://status.polar.sh/api/v2/status.json

# Review payment logs
kubectl logs -f deployment/payments-service | grep "payment-error"

# Check webhook delivery
curl -X GET /api/webhooks/status
```

**Subscription Issues**
```bash
# Verify subscription status
curl -X GET /api/subscriptions/user/:userId

# Check billing cycle
curl -X GET /api/billing/upcoming/:customerId

# Review subscription logs
kubectl logs -f deployment/payments-service | grep "subscription"
```

**Mobile IAP Issues**
```bash
# Validate receipt manually
curl -X POST /api/iap/validate -d '{"receipt_data": "...", "platform": "ios"}'

# Check IAP sync status
curl -X GET /api/iap/sync/status/:userId

# Review IAP logs
kubectl logs -f deployment/payments-service | grep "iap"
```

## 📚 Related Documentation

- [Payment Provider Integration Guide](../../integrations/payments/README.md)
- [Mobile IAP Setup Guide](../../mobile/iap/README.md)
- [Revenue Analytics Dashboard](../../analytics/revenue/README.md)
- [Security and Compliance](../../security/payments/README.md)
- [Webhook Configuration](../../webhooks/README.md)
