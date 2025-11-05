import React, { createContext, useContext, useEffect, useState } from 'react';
import { Platform } from 'react-native';
import { AuthContextType } from './types';
import { WebAuth } from './web/auth-web';
import { MobileAuth } from './mobile/auth-mobile';

// Create the auth context
export const AuthContext = createContext<AuthContextType | undefined>(undefined);

// Create a custom hook for using the auth context
export const useAuth = () => {
  const context = useContext(AuthContext);
  if (!context) {
    throw new Error('useAuth must be used within an AuthProvider');
  }
  return context;
}

export function AuthProvider({ children }: { children: React.ReactNode }) {
  const [auth, setAuth] = useState<AuthContextType | null>(null);
  const [isLoading, setIsLoading] = useState(true);

  useEffect(() => {
    const initAuth = async () => {
      try {
        let authInstance: AuthContextType;
        
        if (Platform.OS === 'web') {
          authInstance = new WebAuth();
        } else {
          authInstance = new MobileAuth();
        }
        
        await authInstance.initialize();
        setAuth(authInstance);
      } catch (error) {
        console.error('Failed to initialize auth:', error);
      } finally {
        setIsLoading(false);
      }
    };

    initAuth();
  }, []);

  // Show loading state while initializing
  if (isLoading) {
    return null; // Or return a loading spinner
  }

  // If auth failed to initialize, still render children but without auth context
  if (!auth) {
    return <>{children}</>;
  }

  return (
    <AuthContext.Provider value={auth}>
      {children}
    </AuthContext.Provider>
  );
}
