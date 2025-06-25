# Qonversion Setup Guide for Nexpo

This guide provides instructions for setting up Qonversion to handle in-app purchases and subscriptions, primarily for the mobile (Expo) part of the Nexpo template.

## Overview

Qonversion is a platform for managing in-app purchases and subscriptions, providing tools for receipt validation, subscription analytics, and access control without requiring your own server infrastructure.

## Prerequisites

- A Qonversion account ([https://qonversion.io/](https://qonversion.io/))
- Node.js and pnpm installed
- Xcode and Android Studio for mobile app development (if testing on physical devices)

## Setting Up Qonversion

### 1. Create a Qonversion Account and Project

1. Sign up or log in to your Qonversion account at [https://dashboard.qonversion.io/](https://dashboard.qonversion.io/).
2. Create a new project for your app.
3. Navigate to "Settings" > "API Keys" to copy your Project Key and Access Key.

### 2. Update Environment Variables

Update your `.env` file in the root of your Nexpo project with the Qonversion API keys:

```
EXPO_PUBLIC_QONVERSION_PROJECT_KEY=your_qonversion_project_key
```

**Important**: The Project Key is safe for client-side use, but keep the Access Key secure for server-side operations if needed.

### 3. Install Qonversion SDK

Due to potential dependency conflicts with React 18.3.1 in this template, installing Qonversion might require specific version compatibility. Try installing the latest version first:

```bash
pnpm install react-native-qonversion
```

If you encounter dependency conflicts, try a specific compatible version:

```bash
pnpm install react-native-qonversion@3.6.2
```

If issues persist, you may need to wait for Qonversion to release a version fully compatible with React 18.3.1, or consider alternative in-app purchase solutions.

### 4. Configure Qonversion for Expo

For Expo, you need to configure the native projects for iOS and Android:

1. **iOS Setup**:
   - Ensure your Expo app is configured for iOS builds.
   - Follow Qonversion's iOS setup guide ([https://documentation.qonversion.io/docs/ios](https://documentation.qonversion.io/docs/ios)) for StoreKit configuration and Capabilities setup in Xcode (you may need to use `eas build` with a local Xcode environment).

2. **Android Setup**:
   - Configure your Android app in `app.json` for Google Play billing.
   - Follow Qonversion's Android setup guide ([https://documentation.qonversion.io/docs/android](https://documentation.qonversion.io/docs/android)) for necessary permissions and billing setup.

### 5. Initialize Qonversion in Your App

Create a Qonversion context or initialization logic in your Expo app. Add the following to a file like `packages/shared-ui/src/context/QonversionContext.tsx`:

```tsx
import React, { createContext, useContext, useEffect, useState } from 'react';
import { Platform } from 'react-native';
import * as Qonversion from 'react-native-qonversion';

interface QonversionContextType {
  isInitialized: boolean;
  userId?: string;
  entitlements?: Qonversion.QonversionEntitlement[];
  loading: boolean;
}

const QonversionContext = createContext<QonversionContextType | undefined>(undefined);

export const QonversionProvider: React.FC<{ children: React.ReactNode }> = ({ children }) => {
  const [isInitialized, setIsInitialized] = useState(false);
  const [userId, setUserId] = useState<string | undefined>(undefined);
  const [entitlements, setEntitlements] = useState<Qonversion.QonversionEntitlement[] | undefined>(undefined);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    const initQonversion = async () => {
      if (Platform.OS === 'web') {
        // Qonversion is mobile-only, skip on web
        setIsInitialized(false);
        setLoading(false);
        return;
      }

      try {
        const projectKey = process.env.EXPO_PUBLIC_QONVERSION_PROJECT_KEY || '';
        if (!projectKey) {
          console.error('Qonversion project key not found in environment variables');
          setLoading(false);
          return;
        }

        await Qonversion.launchWithKey(projectKey);
        setIsInitialized(true);

        // Optionally set a user ID if you have authentication
        // const user = await Qonversion.identify('your_user_id');
        // setUserId(user.uid);

        // Check entitlements (purchased subscriptions or features)
        const userEntitlements = await Qonversion.entitlements();
        setEntitlements(userEntitlements);
      } catch (error) {
        console.error('Error initializing Qonversion:', error);
      } finally {
        setLoading(false);
      }
    };

    initQonversion();
  }, []);

  return (
    <QonversionContext.Provider value={{ isInitialized, userId, entitlements, loading }}>
      {children}
    </QonversionContext.Provider>
  );
};

export const useQonversion = () => {
  const context = useContext(QonversionContext);
  if (context === undefined) {
    throw new Error('useQonversion must be used within a QonversionProvider');
  }
  return context;
};
```

### 6. Wrapping Your App with QonversionProvider

Ensure your app is wrapped with the `QonversionProvider` to make Qonversion available throughout your mobile application. Update `apps/expo/App.tsx`:

```tsx
import { QonversionProvider } from 'shared-ui';

export default function App() {
  return (
    <QonversionProvider>
      {/* Your app content */}
    </QonversionProvider>
  );
}
```

For Next.js (web), you can skip this or add conditional logic to only use Qonversion on mobile platforms.

### 7. Implementing In-App Purchases

Use Qonversion to fetch products and handle purchases. Here's an example component in `packages/shared-ui/src/components/PurchaseScreen.tsx`:

```tsx
import React, { useState, useEffect } from 'react';
import { View, Text, Button, Platform } from 'react-native';
import * as Qonversion from 'react-native-qonversion';
import { useQonversion } from '../context/QonversionContext';

const PurchaseScreen = () => {
  const { isInitialized, entitlements, loading } = useQonversion();
  const [products, setProducts] = useState<Qonversion.QonversionProduct[] | undefined>(undefined);
  const [purchaseLoading, setPurchaseLoading] = useState(false);

  useEffect(() => {
    const fetchProducts = async () => {
      if (isInitialized) {
        try {
          const availableProducts = await Qonversion.products();
          setProducts(availableProducts);
        } catch (error) {
          console.error('Error fetching products:', error);
        }
      }
    };

    fetchProducts();
  }, [isInitialized]);

  const handlePurchase = async (productId: string) => {
    if (!isInitialized) return;

    setPurchaseLoading(true);
    try {
      const result = await Qonversion.purchase(productId);
      console.log('Purchase successful:', result);
      // Refresh entitlements if needed
      const updatedEntitlements = await Qonversion.entitlements();
      // Assuming a context update or state management here
    } catch (error) {
      console.error('Purchase error:', error);
    } finally {
      setPurchaseLoading(false);
    }
  };

  if (Platform.OS === 'web') {
    return (
      <View>
        <Text>In-app purchases are not available on web. Use Stripe for payments.</Text>
      </View>
    );
  }

  if (loading || !isInitialized) {
    return (
      <View>
        <Text>Loading subscription options...</Text>
      </View>
    );
  }

  return (
    <View>
      <Text style={{ fontSize: 20, fontWeight: 'bold', marginBottom: 20 }}>Premium Features</Text>
      {products && products.length > 0 ? (
        products.map((product) => (
          <View key={product.id} style={{ marginBottom: 15 }}>
            <Text>{product.title}</Text>
            <Text>{product.description}</Text>
            <Text>Price: {product.prettyPrice}</Text>
            <Button
              title={purchaseLoading ? 'Processing...' : 'Purchase'}
              onPress={() => handlePurchase(product.id)}
              disabled={purchaseLoading}
            />
          </View>
        ))
      ) : (
        <Text>No products available for purchase.</Text>
      )}
      {entitlements && entitlements.length > 0 && (
        <View>
          <Text style={{ color: 'green' }}>Active Subscriptions:</Text>
          {entitlements.map((entitlement) => (
            <Text key={entitlement.id}>{entitlement.productId} (Active: {entitlement.isActive ? 'Yes' : 'No'})</Text>
          ))}
        </View>
      )}
    </View>
  );
};

export default PurchaseScreen;
```

### 8. Using PurchaseScreen in Your App

Integrate the `PurchaseScreen` component into your navigation or as a modal:

```tsx
import PurchaseScreen from 'shared-ui';

function PremiumFeaturesPage() {
  return <PurchaseScreen />;
}
```

### 9. Setting Up Products in App Store Connect and Google Play Console

1. **App Store Connect (iOS)**:
   - Log in to [App Store Connect](https://appstoreconnect.apple.com/).
   - Navigate to your app > "In-App Purchases".
   - Create subscription or non-consumable products.
   - Sync these products in Qonversion dashboard under "Products".

2. **Google Play Console (Android)**:
   - Log in to [Google Play Console](https://play.google.com/console).
   - Navigate to your app > "Monetize" > "Products" > "Subscriptions" or "In-app products".
   - Create products and sync them in Qonversion dashboard.

## Best Practices

- **User Identification**: If you have user authentication (e.g., Supabase Auth), use `Qonversion.identify(userId)` to associate purchases with user accounts for cross-device access.
- **Subscription Status**: Regularly check `entitlements` to grant or revoke access to premium features based on subscription status.
- **Error Handling**: Implement robust error handling for failed purchases or initialization issues.
- **Testing**: Use test accounts in App Store Connect and Google Play Console. Qonversion also supports test environments.

## Troubleshooting

- **Dependency Conflicts**: If `react-native-qonversion` fails to install due to React version conflicts, try older versions or check Qonversion's GitHub issues for compatibility updates.
- **Initialization Errors**: Ensure your Project Key is correctly set in environment variables. Check logs for detailed error messages.
- **Purchase Failures**: Verify product setup in App Store Connect/Google Play Console matches Qonversion dashboard configurations.
- **Entitlements Not Updating**: After a purchase, refresh entitlements manually with `Qonversion.entitlements()` if needed.

For more detailed information, refer to the official Qonversion documentation at [https://documentation.qonversion.io/](https://documentation.qonversion.io/).
