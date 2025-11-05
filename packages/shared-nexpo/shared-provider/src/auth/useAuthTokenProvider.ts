import { useEffect } from 'react';
import { setAuthTokenProvider } from '@nexpo/shared-utils';
import { useAuth } from './useAuth';

/**
 * Hook to set up the auth token provider for the API client.
 * This should be called at the root of your app to enable API authentication.
 */
export function useAuthTokenProvider() {
  const auth = useAuth();

  useEffect(() => {
    // Set the token provider for the API client
    setAuthTokenProvider(async () => {
      if (auth.getAccessToken) {
        return await auth.getAccessToken();
      }
      return null;
    });

    // Clean up on unmount
    return () => {
      setAuthTokenProvider(() => Promise.resolve(null));
    };
  }, [auth]);
}
