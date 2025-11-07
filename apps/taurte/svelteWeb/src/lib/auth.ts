import { createAuth0Client, type Auth0Client, type Auth0ClientOptions } from '@auth0/auth0-spa-js';
import { authStore } from '../stores/auth.svelte';

let auth0Client: Auth0Client | null = null;

const auth0Config: Auth0ClientOptions = {
  domain: import.meta.env.VITE_AUTH0_DOMAIN || '',
  clientId: import.meta.env.VITE_AUTH0_CLIENT_ID || '',
  authorizationParams: {
    redirect_uri: window.location.origin,
    audience: import.meta.env.VITE_AUTH0_AUDIENCE || '',
  },
  useRefreshTokens: true,
  cacheLocation: 'localstorage'
};

export async function initializeAuth(): Promise<void> {
  try {
    authStore.setLoading(true);
    authStore.setError(null);

    // Skip Auth0 initialization if not configured (development mode)
    if (!auth0Config.domain || !auth0Config.clientId) {
      console.warn('Auth0 not configured. Running in development mode without authentication.');
      authStore.setLoading(false);
      return;
    }

    auth0Client = await createAuth0Client(auth0Config);

    // Check if we're returning from an Auth0 callback
    if (window.location.search.includes('code=') || window.location.search.includes('error=')) {
      try {
        await auth0Client.handleRedirectCallback();
        // Clean up the URL
        window.history.replaceState({}, document.title, window.location.pathname);
      } catch (error) {
        console.error('Error handling redirect callback:', error);
        authStore.setError('Failed to handle authentication callback');
      }
    }

    // Check if user is authenticated
    const isAuthenticated = await auth0Client.isAuthenticated();
    
    if (isAuthenticated) {
      const user = await auth0Client.getUser();
      if (user) {
        authStore.login(user);
      }
    }
  } catch (error) {
    console.error('Auth initialization failed:', error);
    authStore.setError('Failed to initialize authentication');
  } finally {
    authStore.setLoading(false);
  }
}

export async function login(): Promise<void> {
  if (!auth0Client) {
    throw new Error('Auth0 client not initialized');
  }

  try {
    await auth0Client.loginWithRedirect({
      authorizationParams: {
        redirect_uri: window.location.origin
      }
    });
  } catch (error) {
    console.error('Login failed:', error);
    authStore.setError('Login failed');
    throw error;
  }
}

export async function logout(): Promise<void> {
  if (!auth0Client) {
    throw new Error('Auth0 client not initialized');
  }

  try {
    authStore.logout();
    await auth0Client.logout({
      logoutParams: {
        returnTo: window.location.origin
      }
    });
  } catch (error) {
    console.error('Logout failed:', error);
    authStore.setError('Logout failed');
    throw error;
  }
}

export async function getAccessToken(): Promise<string | null> {
  if (!auth0Client) {
    throw new Error('Auth0 client not initialized');
  }

  try {
    return await auth0Client.getTokenSilently();
  } catch (error) {
    console.error('Failed to get access token:', error);
    return null;
  }
}

export function getAuth0Client(): Auth0Client | null {
  return auth0Client;
}
