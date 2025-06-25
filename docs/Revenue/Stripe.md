# Stripe Setup Guide for Nexpo

This guide provides instructions for setting up Stripe to handle payments in the Nexpo template, suitable for both web (Next.js) and mobile (Expo) platforms.

## Overview

Stripe is a payment processing platform that allows you to accept payments, manage subscriptions, and handle various financial transactions in your application.

## Prerequisites

- A Stripe account ([https://stripe.com/](https://stripe.com/))
- Node.js and pnpm installed

## Setting Up Stripe

### 1. Create a Stripe Account and Get API Keys

1. Sign up or log in to your Stripe account at [https://dashboard.stripe.com/](https://dashboard.stripe.com/).
2. Navigate to "Developers" > "API keys".
3. Copy your Publishable key (starts with `pk_`) and Secret key (starts with `sk_`). You'll need these for your application.

### 2. Update Environment Variables

Update your `.env` file in the root of your Nexpo project with the Stripe API keys:

```
STRIPE_PUBLIC_KEY=your_stripe_public_key
STRIPE_SECRET_KEY=your_stripe_secret_key
STRIPE_WEBHOOK_SECRET=your_stripe_webhook_secret (optional, for webhook handling)
```

**Important**: Never commit your Secret key to version control. Keep it secure in environment variables.

### 3. Install Stripe Dependencies

Stripe dependencies are already installed in this template. If you need to reinstall or update them, run:

```bash
pnpm install @stripe/stripe-js stripe @stripe/react-stripe-js
```

### 4. Stripe Context Setup

The Stripe context is already set up in `packages/shared-ui/src/context/StripeContext.tsx`. This context initializes Stripe with your public key:

```tsx
import React, { createContext, useContext, useEffect, useState } from 'react';
import { loadStripe, Stripe } from '@stripe/stripe-js';

interface StripeContextType {
  stripe: Stripe | null;
  isLoading: boolean;
}

const StripeContext = createContext<StripeContextType | undefined>(undefined);

export const StripeProvider: React.FC<{ children: React.ReactNode }> = ({ children }) => {
  const [stripe, setStripe] = useState<Stripe | null>(null);
  const [isLoading, setIsLoading] = useState(true);

  useEffect(() => {
    const initializeStripe = async () => {
      const stripePublicKey = process.env.NEXT_PUBLIC_STRIPE_PUBLIC_KEY || process.env.EXPO_PUBLIC_STRIPE_PUBLIC_KEY || '';
      if (stripePublicKey) {
        const stripeInstance = await loadStripe(stripePublicKey);
        setStripe(stripeInstance);
      }
      setIsLoading(false);
    };

    initializeStripe();
  }, []);

  return (
    <StripeContext.Provider value={{ stripe, isLoading }}>
      {children}
    </StripeContext.Provider>
  );
};

export const useStripeContext = () => {
  const context = useContext(StripeContext);
  if (context === undefined) {
    throw new Error('useStripeContext must be used within a StripeProvider');
  }
  return context;
};
```

### 5. Wrapping Your App with StripeProvider

Ensure your app is wrapped with the `StripeProvider` to make Stripe available throughout your application. Update both `apps/next/pages/_app.tsx` and `apps/expo/App.tsx`:

#### For Next.js (`_app.tsx`):

```tsx
import { StripeProvider } from 'shared-ui';

function MyApp({ Component, pageProps }) {
  return (
    <StripeProvider>
      <Component {...pageProps} />
    </StripeProvider>
  );
}

export default MyApp;
```

#### For Expo (`App.tsx`):

```tsx
import { StripeProvider } from 'shared-ui';

export default function App() {
  return (
    <StripeProvider>
      {/* Your app content */}
    </StripeProvider>
  );
}
```

### 6. Using Stripe for Payments

A `PaymentForm` component is already set up in `packages/shared-ui/src/components/PaymentForm.tsx` for collecting payment information on the web:

```tsx
import React, { useState } from 'react';
import { View, Text, TextInput, Button, StyleSheet, Platform } from 'react-native';
import { CardElement, useStripe, useElements } from '@stripe/react-stripe-js';
import { useStripeContext } from '../context/StripeContext';

const PaymentForm = ({ onPaymentSuccess }: { onPaymentSuccess: () => void }) => {
  const { stripe } = useStripeContext();
  const elements = useElements();
  const [name, setName] = useState('');
  const [error, setError] = useState<string | null>(null);
  const [processing, setProcessing] = useState(false);

  const handleSubmit = async (event: any) => {
    event.preventDefault();
    if (!stripe || !elements) {
      return;
    }

    setProcessing(true);
    const cardElement = elements.getElement(CardElement);

    if (!cardElement) {
      setError('Card element not found');
      setProcessing(false);
      return;
    }

    const { error, paymentMethod } = await stripe.createPaymentMethod({
      type: 'card',
      card: cardElement,
      billing_details: {
        name,
      },
    });

    if (error) {
      setError(error.message || 'An error occurred');
      setProcessing(false);
    } else {
      // Here you would typically send the paymentMethod.id to your server for further processing
      // For this example, we'll just simulate a successful payment
      setTimeout(() => {
        setProcessing(false);
        onPaymentSuccess();
      }, 1000);
    }
  };

  // ... rest of the component (UI)
};
```

#### Using PaymentForm in Your App:

```tsx
import PaymentForm from 'shared-ui';

function CheckoutPage() {
  const handlePaymentSuccess = () => {
    console.log('Payment successful!');
    // Redirect or update UI
  };

  return (
    <div>
      <h1>Checkout</h1>
      <PaymentForm onPaymentSuccess={handlePaymentSuccess} />
    </div>
  );
}
```

### 7. Server-Side Integration (Stripe Secret Key)

For full payment processing, you'll need server-side logic to create charges, subscriptions, etc., using your Stripe Secret Key. Here's an example of setting up an API endpoint in Next.js:

1. Create a new API route at `apps/next/pages/api/create-checkout-session.ts`:

```ts
import Stripe from 'stripe';

const stripe = new Stripe(process.env.STRIPE_SECRET_KEY || '', {
  apiVersion: '2022-11-15',
});

export default async function handler(req: any, res: any) {
  if (req.method === 'POST') {
    try {
      const session = await stripe.checkout.sessions.create({
        payment_method_types: ['card'],
        line_items: [
          {
            price_data: {
              currency: 'usd',
              product_data: {
                name: 'Example Product',
              },
              unit_amount: 2000, // $20.00
            },
            quantity: 1,
          },
        ],
        mode: 'payment',
        success_url: `${req.headers.origin}/success`,
        cancel_url: `${req.headers.origin}/cancel`,
      });

      res.status(200).json({ id: session.id });
    } catch (err: any) {
      res.status(500).json({ error: err.message });
    }
  } else {
    res.setHeader('Allow', 'POST');
    res.status(405).end('Method Not Allowed');
  }
}
```

2. Use this endpoint from the client side to initiate a checkout:

```tsx
const handleCheckout = async () => {
  const response = await fetch('/api/create-checkout-session', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
  });

  const { id } = await response.json();
  if (id && stripe) {
    await stripe.redirectToCheckout({ sessionId: id });
  }
};
```

### 8. Mobile Considerations (Expo)

Stripe's React Native SDK can be used for mobile payments:

1. Install the Stripe React Native package:
   ```bash
   pnpm install @stripe/stripe-react-native
   ```

2. Follow Stripe's documentation for React Native setup ([https://stripe.com/docs/payments/accept-a-payment?platform=react-native](https://stripe.com/docs/payments/accept-a-payment?platform=react-native)).

## Best Practices

- **Security**: Never expose your Stripe Secret Key in client-side code. Use it only in server-side API routes or functions.
- **Error Handling**: Implement robust error handling for failed payments or network issues.
- **User Experience**: Provide clear feedback during payment processing (loading states, success/error messages).
- **Testing**: Use Stripe's test mode and test card numbers ([https://stripe.com/docs/testing](https://stripe.com/docs/testing)) during development.

## Troubleshooting

- **Payment Form Not Rendering**: Ensure `StripeProvider` wraps your app and that your public key is correctly set in environment variables.
- **API Errors**: Check Stripe Dashboard's "Developers" > "Logs" for detailed error messages from server-side requests.
- **Mobile Payment Issues**: Verify that `@stripe/stripe-react-native` is correctly installed and configured for Expo.

For more detailed information, refer to the official Stripe documentation at [https://stripe.com/docs](https://stripe.com/docs).
