# Polar Integration Guide

## Overview

Polar is a Merchant of Record (MoR) billing service designed to simplify global payments, taxes, and compliance for digital products and services. Unlike traditional payment processors, Polar handles not only payment processing but also tax calculation, compliance, and customer support, making it an excellent alternative to Stripe for businesses looking for a comprehensive billing solution.

## Key Features

- **Complete MoR Solution**: Handles payments, taxes, compliance, and customer support
- **Global Coverage**: Supports multiple currencies and payment methods worldwide
- **Subscription Management**: Robust subscription and recurring billing features
- **Tax Compliance**: Automatically calculates and files taxes across jurisdictions
- **Checkout Experience**: Customizable checkout flows with localization support
- **Analytics**: Real-time revenue and customer insights
- **Developer-Friendly**: Modern APIs and SDKs for web and mobile

## Integration Prerequisites

1. A Polar account (register at [polar.com](https://polar.com))
2. Polar API keys (available in your Polar dashboard)
3. Defined product catalog in your Polar dashboard

## Environment Variables

Add the following variables to your `.env` file:

```
# Polar Integration
ENABLE_POLAR=true
POLAR_API_KEY=your_api_key_here
POLAR_WEBHOOK_SECRET=your_webhook_secret_here
POLAR_ENVIRONMENT=development # or production
```

## Implementation Steps

### 1. Install Polar SDK

```bash
pnpm install @polar/sdk
# or
yarn add @polar/sdk
```

### 2. Initialize Polar Client

```javascript
import { PolarClient } from '@polar/sdk';

const polarClient = new PolarClient({
  apiKey: process.env.POLAR_API_KEY,
  environment: process.env.POLAR_ENVIRONMENT,
});
```

### 3. Create Checkout Sessions

```javascript
// Example: Creating a checkout session for a one-time purchase
const session = await polarClient.checkoutSessions.create({
  successUrl: 'https://yourapp.com/success',
  cancelUrl: 'https://yourapp.com/cancel',
  lineItems: [
    {
      productId: 'prod_12345',
      quantity: 1
    }
  ],
  customerEmail: 'customer@example.com',
});

// Redirect to the checkout URL
window.location.href = session.url;
```

### 4. Handle Webhooks

```javascript
import express from 'express';
import { PolarWebhookHandler } from '@polar/sdk';

const app = express();
const webhookHandler = new PolarWebhookHandler(process.env.POLAR_WEBHOOK_SECRET);

app.post('/webhooks/polar', express.raw({ type: 'application/json' }), async (req, res) => {
  try {
    const event = webhookHandler.constructEvent(req.body, req.headers['polar-signature']);
    
    switch (event.type) {
      case 'payment.succeeded':
        // Handle successful payment
        break;
      case 'subscription.created':
        // Handle subscription creation
        break;
      case 'subscription.canceled':
        // Handle subscription cancellation
        break;
      // Handle other event types
    }
    
    res.status(200).send();
  } catch (err) {
    console.error('Webhook error:', err);
    res.status(400).send(`Webhook Error: ${err.message}`);
  }
});
```

### 5. Manage Subscriptions

```javascript
// Create a subscription
const subscription = await polarClient.subscriptions.create({
  customerId: 'cus_12345',
  priceId: 'price_67890',
});

// Retrieve a subscription
const retrievedSubscription = await polarClient.subscriptions.retrieve('sub_12345');

// Update a subscription
const updatedSubscription = await polarClient.subscriptions.update('sub_12345', {
  priceId: 'price_new',
});

// Cancel a subscription
await polarClient.subscriptions.cancel('sub_12345');
```

## Mobile Integration

### React Native / Expo

```javascript
import { PolarCheckout } from '@polar/react-native';

function CheckoutScreen() {
  const handleCheckout = async () => {
    try {
      // Create a checkout session on your backend
      const response = await fetch('https://your-api.com/create-checkout', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ productId: 'prod_12345' }),
      });
      
      const { sessionId } = await response.json();
      
      // Open Polar checkout
      const result = await PolarCheckout.present({
        sessionId,
        apiKey: 'your_publishable_key',
      });
      
      if (result.status === 'completed') {
        // Handle successful payment
      }
    } catch (error) {
      console.error('Checkout error:', error);
    }
  };
  
  return (
    <Button title="Subscribe" onPress={handleCheckout} />
  );
}
```

## Testing

Polar provides test API keys and a sandbox environment for testing your integration without processing real payments.

### Test Cards

- **Successful payment**: 4242 4242 4242 4242
- **Payment requires authentication**: 4000 0025 0000 3155
- **Payment declined**: 4000 0000 0000 9995

## Comparison with Other Payment Solutions

### Polar vs. Stripe

- Polar offers a complete MoR solution, while Stripe is primarily a payment processor
- Polar handles tax compliance and filing, which requires additional setup with Stripe
- Polar has simpler pricing but may be more expensive for high-volume businesses

### Polar vs. Qonversion

- Polar is a full payment processor and MoR, while Qonversion is primarily a subscription analytics platform
- Qonversion works on top of other payment processors, while Polar is a standalone solution
- Polar has more direct payment features, while Qonversion excels at subscription analytics and A/B testing

## Best Practices

1. **Webhooks**: Always implement webhook handling for reliable payment event processing
2. **Error Handling**: Implement robust error handling for API calls and checkout flows
3. **Testing**: Thoroughly test the integration in development mode before going live
4. **Security**: Never expose your API keys in client-side code
5. **Logging**: Implement detailed logging for troubleshooting

## Troubleshooting

### Common Issues

1. **Webhook Signature Verification Failed**: Check that you're using the correct webhook secret
2. **API Key Invalid**: Ensure you're using the correct API key for your environment
3. **Product Not Found**: Verify that the product exists in your Polar dashboard

### Support Resources

- [Polar Documentation](https://docs.polar.com)
- [API Reference](https://api.polar.com)
- [Support Email](mailto:support@polar.com)

## Migration Guides

### Migrating from Stripe to Polar

1. Export customer and subscription data from Stripe
2. Import data into Polar using the migration API
3. Update API integrations to use Polar endpoints
4. Test thoroughly before switching production traffic

### Migrating from Qonversion to Polar

1. Export subscription and customer data from Qonversion
2. Set up equivalent products in Polar
3. Import customer data into Polar
4. Update mobile SDK implementation
5. Test thoroughly before full migration