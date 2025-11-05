import React, { createContext, useContext, useEffect, useState, ReactNode } from 'react';
import { loadStripe, Stripe } from '@stripe/stripe-js';
import { isTauri } from '@nexpo/shared-utils';
import { Platform } from 'react-native';

interface StripeContextType {
  stripe: Stripe | null;
  isLoading: boolean;
}

const StripeContext = createContext<StripeContextType | undefined>(undefined);

interface StripeProviderProps {
  children: React.ReactNode;
}

// Platform-aware Stripe key selection
function getStripePublicKey(): string {
  // Check if we're in Tauri environment
  if (isTauri()) {
    return process.env.TAURI_PUBLIC_STRIPE_PUBLIC_KEY || '';
  }
  
  // Check if we're in Expo/React Native environment
  if (Platform.OS === 'ios' || Platform.OS === 'android') {
    return process.env.EXPO_PUBLIC_STRIPE_PUBLIC_KEY || '';
  }
  
  // Default to Next.js web environment
  return process.env.NEXT_PUBLIC_STRIPE_PUBLIC_KEY || '';
}

const stripePublicKey = getStripePublicKey();

export const StripeProvider: React.FC<StripeProviderProps> = ({ children }) => {
  const [stripe, setStripe] = useState<Stripe | null>(null);
  const [isLoading, setIsLoading] = useState(true);

  useEffect(() => {
    const initializeStripe = async () => {
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
