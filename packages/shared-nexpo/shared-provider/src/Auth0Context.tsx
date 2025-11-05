import React, { createContext, useContext, useState, useEffect, ReactNode, useMemo, useCallback, Fragment } from 'react';
import { Platform } from 'react-native';
import AsyncStorage from '@react-native-async-storage/async-storage';
import { initAuth0Client } from './auth0-client';

// Auth0 configuration
const AUTH0_DOMAIN = process.env.NEXT_PUBLIC_AUTH0_DOMAIN || process.env.EXPO_PUBLIC_AUTH0_DOMAIN || '';
const AUTH0_CLIENT_ID = process.env.NEXT_PUBLIC_AUTH0_CLIENT_ID || process.env.EXPO_PUBLIC_AUTH0_CLIENT_ID || '';
const AUTH0_AUDIENCE = process.env.NEXT_PUBLIC_AUTH0_AUDIENCE || process.env.EXPO_PUBLIC_AUTH0_AUDIENCE || '';
const REDIRECT_URI = Platform.OS === 'web' 
  ? `${typeof window !== 'undefined' ? window.location.origin : ''}/api/auth/callback`
  : process.env.EXPO_PUBLIC_AUTH0_REDIRECT_URI || '';

interface Auth0User {
  sub: string;
  name?: string;
  given_name?: string;
  family_name?: string;
  preferred_username?: string;
  email?: string;
  picture?: string;
  [key: string]: any;
}

// Create a context for Auth0 authentication state
const AuthContext = createContext<{
  isAuthenticated: boolean;
  user: Auth0User | null;
  isLoading: boolean;
  loginWithRedirect: () => Promise<void>;
  logout: () => Promise<void>;
}>({
  isAuthenticated: false,
  user: null,
  isLoading: true,
  loginWithRedirect: async () => {},
  logout: async () => {},
});

// Custom hook to access the auth context
export const useAuth0 = () => useContext(AuthContext);

// Auth provider component
export const Auth0Provider = ({ children }: { children: ReactNode }) => {
  const [user, setUser] = useState<Auth0User | null>(null);
  const [isLoading, setIsLoading] = useState(true);
  const [isAuthenticated, setIsAuthenticated] = useState(false);

  useEffect(() => {
    // Function to check if code is running on client side
    const checkClientSide = () => {
      return typeof window !== 'undefined';
    };
    
    const isClientSide = checkClientSide();
    // Only initialize Auth0 on client side
    if (!isClientSide) {
      setIsLoading(false);
      return;
    }
    
    async function init() {
      try {
        const auth0Client = await initAuth0Client();
        const isAuthenticated = await auth0Client.isAuthenticated();
        const user = isAuthenticated ? await auth0Client.getUser() : null;
        setUser(user);
        setIsAuthenticated(isAuthenticated);
        setIsLoading(false);
      } catch (error) {
        console.error('Error initializing Auth0:', error);
        setIsLoading(false);
      }
    }
    init();
  }, []);

  const loginWithRedirect = async () => {
    const isClientSide = typeof window !== 'undefined';
    if (!isClientSide) return;
    window.location.href = '/api/auth/login';
  };

  const logout = async () => {
    const isClientSide = typeof window !== 'undefined';
    if (!isClientSide) return;
    window.location.href = '/api/auth/logout';
  };

  // Mobile authentication for Expo
  const checkMobileAuth = async () => {
    try {
      // Check AsyncStorage for auth data
      const token = await AsyncStorage.getItem('auth_token');
      const userDataStr = await AsyncStorage.getItem('user_data');
      
      if (token && userDataStr) {
        const userData = JSON.parse(userDataStr);
        setUser(userData);
        setIsAuthenticated(true);
      }
    } catch (error) {
      console.error('Failed to check mobile auth:', error);
    } finally {
      setIsLoading(false);
    }
  };

  // Mobile login
  const mobileLogin = async () => {
    try {
      const AuthSession = require('expo-auth-session');
      if (!AuthSession) {
        throw new Error('expo-auth-session not installed');
      }

      // Create the authorization URL
      const authUrl = `https://${AUTH0_DOMAIN}/authorize?` + new URLSearchParams({
        response_type: 'code',
        client_id: AUTH0_CLIENT_ID,
        redirect_uri: REDIRECT_URI,
        scope: 'openid profile email offline_access',
        audience: AUTH0_AUDIENCE,
      }).toString();

      // Open browser for authentication
      const result = await AuthSession.startAsync({ authUrl });
      
      if (result.type === 'success') {
        const code = result.params.code;
        
        // Exchange code for token
        const tokenResponse = await fetch(`https://${AUTH0_DOMAIN}/oauth/token`, {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
          },
          body: JSON.stringify({
            grant_type: 'authorization_code',
            client_id: AUTH0_CLIENT_ID,
            code,
            redirect_uri: REDIRECT_URI,
          }),
        });

        const tokenData = await tokenResponse.json();
        const accessToken = tokenData.access_token;
        const idToken = tokenData.id_token;
        
        if (accessToken) {
          // Store tokens
          await AsyncStorage.setItem('auth_token', accessToken);
          await AsyncStorage.setItem('id_token', idToken || '');
          
          // Get user info
          const userResponse = await fetch(`https://${AUTH0_DOMAIN}/userinfo`, {
            headers: {
              'Authorization': `Bearer ${accessToken}`,
            },
          });
          
          const userData = await userResponse.json();
          await AsyncStorage.setItem('user_data', JSON.stringify(userData));
          
          setUser(userData);
          setIsAuthenticated(true);
        }
      }
    } catch (error) {
      console.error('Mobile login failed:', error);
    }
  };

  // Mobile logout
  const mobileLogout = async () => {
    try {
      // Clear stored data
      await AsyncStorage.removeItem('auth_token');
      await AsyncStorage.removeItem('id_token');
      await AsyncStorage.removeItem('user_data');
      
      setUser(null);
      setIsAuthenticated(false);
      
      // Open logout URL in browser
      const AuthSession = require('expo-auth-session');
      if (AuthSession) {
        const isClientSide = typeof window !== 'undefined';
        if (isClientSide) {
          await AuthSession.WebBrowser.openBrowserAsync(
            `https://${AUTH0_DOMAIN}/v2/logout?client_id=${AUTH0_CLIENT_ID}&returnTo=${encodeURIComponent(REDIRECT_URI)}`
          );
        }
      }
    } catch (error) {
      console.error('Logout failed:', error);
    }
  };

  return (
    <AuthContext.Provider
      value={{
        isAuthenticated,
        user,
        isLoading,
        loginWithRedirect: Platform.OS === 'web' ? loginWithRedirect : mobileLogin,
        logout: Platform.OS === 'web' ? logout : mobileLogout,
      }}
    >
      {children}
    </AuthContext.Provider>
  );
};
