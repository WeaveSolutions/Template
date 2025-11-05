import React, { createContext, useContext, useState, useEffect, useCallback } from 'react';
import * as AuthSession from 'expo-auth-session';
import * as WebBrowser from 'expo-web-browser';
import { Platform } from 'react-native';

// Platform-specific imports
let SecureStore: any;
if (Platform.OS !== 'web') {
  SecureStore = require('expo-secure-store');
}

import { 
  AUTH0_AUTHORIZE_URL, 
  AUTH0_TOKEN_URL, 
  AUTH0_DOMAIN,
  AUDIENCE, 
  CLIENT_ID, 
  SCOPES 
} from '@nexpo/shared-utils';
import { AuthContextType, User, Session, AuthResult } from './types';

export const AuthContext = createContext<AuthContextType | undefined>(undefined);

// Create redirect URI  
const redirectUri = AuthSession.makeRedirectUri({
  scheme: 'nexpo',
  path: 'auth'
});

export const Auth0Provider: React.FC<{ children: React.ReactNode }> = ({ children }) => {
  const [user, setUser] = useState<User | null>(null);
  const [session, setSession] = useState<Session | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<Error | null>(null);

  // Load user from storage on mount
  useEffect(() => {
    const loadUser = async () => {
      try {
        const userData = await SecureStore.getItemAsync('user');
        const sessionData = await SecureStore.getItemAsync('session');
        
        if (userData) {
          setUser(JSON.parse(userData));
        }
        if (sessionData) {
          setSession(JSON.parse(sessionData));
        }
      } catch (error) {
        console.error('Failed to load user:', error);
        setError(error as Error);
      } finally {
        setLoading(false);
      }
    };

    loadUser();
  }, []);

  const initialize = async (): Promise<void> => {
    // Already handled in useEffect
  };

  const signIn = async (email: string, password: string): Promise<AuthResult> => {
    try {
      setLoading(true);
      setError(null);
      
      // Create the authorization request
      const authRequest = new AuthSession.AuthRequest({
        clientId: CLIENT_ID,
        scopes: SCOPES.split(' '),
        redirectUri,
        usePKCE: true,
        responseType: AuthSession.ResponseType.Code,
        extraParams: {
          audience: AUDIENCE,
        },
      });

      // Prompt the user to log in
      const result = await authRequest.promptAsync({
        authorizationEndpoint: AUTH0_AUTHORIZE_URL,
      });

      if (result.type === 'success') {
        // Exchange the authorization code for tokens
        const tokenResponse = await AuthSession.exchangeCodeAsync(
          {
            clientId: CLIENT_ID,
            code: result.params.code,
            redirectUri,
            extraParams: {
              code_verifier: authRequest.codeVerifier || '',
            },
          },
          {
            tokenEndpoint: AUTH0_TOKEN_URL,
          }
        );

        // Create session
        const newSession: Session = {
          access_token: tokenResponse.accessToken,
          refresh_token: tokenResponse.refreshToken || undefined,
          expires_in: tokenResponse.expiresIn || 3600,
          token_type: 'Bearer',
        };

        // Store the tokens securely
        await SecureStore.setItemAsync('session', JSON.stringify(newSession));

        // Get user info
        const userInfo = await fetchUserInfo(tokenResponse.accessToken);
        setUser(userInfo);
        setSession(newSession);
        await SecureStore.setItemAsync('user', JSON.stringify(userInfo));

        return { error: null };
      }

      return { error: new Error('Login was cancelled') };
    } catch (error) {
      setError(error as Error);
      return { error: error as Error };
    } finally {
      setLoading(false);
    }
  };

  const signUp = async (email: string, password: string, metadata?: Record<string, any>): Promise<AuthResult> => {
    // For Auth0, signup uses the same flow as signin
    return signIn(email, password);
  };

  const signOut = async (): Promise<AuthResult> => {
    try {
      // Clear all stored data
      await Promise.all([
        SecureStore.deleteItemAsync('session'),
        SecureStore.deleteItemAsync('user'),
      ]);
      
      setUser(null);
      setSession(null);
      
      // Redirect to Auth0 logout endpoint
      const logoutUrl = `https://${AUTH0_DOMAIN}/v2/logout?client_id=${CLIENT_ID}&returnTo=${encodeURIComponent(redirectUri)}`;
      await WebBrowser.openAuthSessionAsync(logoutUrl, redirectUri);
      
      return { error: null };
    } catch (error) {
      setError(error as Error);
      return { error: error as Error };
    }
  };

  const getAccessToken = async (): Promise<string | null> => {
    return session?.access_token || null;
  };

  const refreshSession = async (): Promise<void> => {
    if (!session?.refresh_token) {
      throw new Error('No refresh token available');
    }
    
    // Implement token refresh logic here
    // This would typically involve calling Auth0's token endpoint
  };

  // Placeholder implementations for other required methods
  const signInWithGoogle = async (): Promise<void> => {
    throw new Error('Google sign-in not implemented in Auth0Context');
  };

  const signInWithProvider = async (provider: string): Promise<void> => {
    throw new Error(`Provider ${provider} sign-in not implemented in Auth0Context`);
  };

  const signInWithPasskey = async () => {
    return { success: false, error: 'Passkey not supported in Auth0Context' };
  };

  const registerPasskey = async () => {
    return { success: false, error: 'Passkey not supported in Auth0Context' };
  };

  const isPasskeySupported = async () => false;

  const resetPassword = async (email: string): Promise<AuthResult> => {
    return { error: new Error('Password reset not implemented') };
  };

  const updatePassword = async (password: string): Promise<AuthResult> => {
    return { error: new Error('Password update not implemented') };
  };

  const getConnectedProviders = () => [];
  const getEnabledProviders = () => ['auth0'];

  const loginWithRedirect = async (): Promise<void> => {
    await signIn('', ''); // Trigger auth flow
  };

  const logout = async (): Promise<void> => {
    await signOut();
  };

  const value: AuthContextType = {
    user,
    session,
    loading,
    error,
    isAuthenticated: !!user && !!session,
    isLoadingState: loading,
    currentUser: user,
    initialize,
    signIn,
    signUp,
    signOut,
    signInWithGoogle,
    signInWithProvider,
    signInWithPasskey,
    registerPasskey,
    isPasskeySupported,
    resetPassword,
    updatePassword,
    getConnectedProviders,
    getEnabledProviders,
    getAccessToken,
    refreshSession,
    loginWithRedirect,
    logout,
  };

  return <AuthContext.Provider value={value}>{children}</AuthContext.Provider>;
};

export const useAuth0 = (): AuthContextType => {
  const context = useContext(AuthContext);
  if (context === undefined) {
    throw new Error('useAuth0 must be used within an Auth0Provider');
  }
  return context;
};

// Helper function to fetch user info
async function fetchUserInfo(accessToken: string): Promise<User> {
  const response = await fetch(`https://${AUTH0_DOMAIN}/userinfo`, {
    headers: {
      Authorization: `Bearer ${accessToken}`,
    },
  });

  if (!response.ok) {
    throw new Error('Failed to fetch user info');
  }

  return response.json();
}
