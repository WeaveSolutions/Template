import React from 'react';
import { SimpleAuthProvider } from './auth/SimpleAuthContext';

// Create a custom provider without direct usage of SolitoProvider
// This avoids the breaking changes between different solito versions

interface ProviderProps {
  children: any;
}

export function Provider({ children }: ProviderProps) {
  return (
    <SimpleAuthProvider>
      {/* Just pass children through since we're not using navigation in this provider */}
      {children}
    </SimpleAuthProvider>
  );
}

// Export all auth components and types
export { SimpleAuthProvider as Auth0Provider, AuthContext } from './auth/SimpleAuthContext';
export * from './auth/types';

// Export web and mobile auth implementations
export * from './auth/web';
export * from './auth/mobile';

// Export hooks (but not the conflicting useAuth from auth module)
export { useAuth } from './auth/SimpleAuthContext';
export { useAuth0 } from './auth/Auth0Context';

// Re-export all other providers (avoiding AuthProvider name collision)
export { default as DittoProvider } from './ditto/DittoProvider';

// Export Ditto provider
export * from './ditto/DittoProvider';
