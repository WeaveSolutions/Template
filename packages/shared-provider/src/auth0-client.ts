// Mock Auth0 client for Next.js until proper configuration is set up

// Auth0 configuration
const AUTH0_DOMAIN = process.env.NEXT_PUBLIC_AUTH0_DOMAIN || process.env.EXPO_PUBLIC_AUTH0_DOMAIN || '';
const AUTH0_CLIENT_ID = process.env.NEXT_PUBLIC_AUTH0_CLIENT_ID || process.env.EXPO_PUBLIC_AUTH0_CLIENT_ID || '';
const AUTH0_AUDIENCE = process.env.NEXT_PUBLIC_AUTH0_AUDIENCE || process.env.EXPO_PUBLIC_AUTH0_AUDIENCE || '';
const REDIRECT_URI = typeof window !== 'undefined' && window.location ? `${window.location.origin}/api/auth/callback` : '';

// Initialize Auth0 client only on the client side
export const initAuth0Client = async () => {
  if (typeof window === 'undefined') {
    // Return a mock client for SSR
    return {
      isAuthenticated: () => Promise.resolve(false),
      getUser: () => Promise.resolve(null),
      loginWithRedirect: () => Promise.resolve(),
      logout: () => Promise.resolve(),
    };
  }

  // Return a mock client for now until proper Auth0 setup
  return {
    isAuthenticated: () => Promise.resolve(false),
    getUser: () => Promise.resolve(null),
    loginWithRedirect: () => {
      window.location.href = '/api/auth/login';
      return Promise.resolve();
    },
    logout: () => {
      window.location.href = '/api/auth/logout';
      return Promise.resolve();
    },
  };
};
