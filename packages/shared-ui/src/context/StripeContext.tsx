import React, { createContext, useContext, useEffect, useState, ReactNode } from 'react';
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
