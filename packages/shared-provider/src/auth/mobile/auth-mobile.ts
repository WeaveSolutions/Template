import * as AuthSession from 'expo-auth-session';
import * as WebBrowser from 'expo-web-browser';
import * as SecureStore from 'expo-secure-store';
import { Platform } from 'react-native';
import { AuthContextType, User, Session } from '../types';

// Add type definitions for expo-secure-store
declare module 'expo-secure-store' {
  export function getItemAsync(key: string): Promise<string | null>;
  export function setItemAsync(key: string, value: string): Promise<void>;
  export function deleteItemAsync(key: string): Promise<void>;
}

const AUTH0_DOMAIN = process.env.EXPO_PUBLIC_AUTH0_DOMAIN || '';
const CLIENT_ID = process.env.EXPO_PUBLIC_AUTH0_CLIENT_ID || '';
const AUDIENCE = process.env.EXPO_PUBLIC_AUTH0_AUDIENCE || '';

const TOKEN_KEY = 'auth_token';
const REFRESH_TOKEN_KEY = 'refresh_token';
const USER_KEY = 'user_data';

export class MobileAuth implements AuthContextType {
  user: User | null = null;
  session: Session | null = null;
  loading = true;
  private accessToken: string | null = null;
  private refreshToken: string | null = null;
  private _isLoading = true;

  constructor() {
    // Initialize any mobile-specific setup
  }

  async initialize(): Promise<void> {
    try {
      // Try to load tokens and user from secure storage
      const [token, refreshToken, userData] = await Promise.all([
        SecureStore.getItemAsync(TOKEN_KEY),
        SecureStore.getItemAsync(REFRESH_TOKEN_KEY),
        SecureStore.getItemAsync(USER_KEY),
      ]);

      if (token) {
        this.accessToken = token;
        this.refreshToken = refreshToken || null;
        
        if (userData) {
          const user = JSON.parse(userData);
          this.user = user;
          this.session = {
            access_token: token || '',
            refresh_token: refreshToken || '',
            expires_in: 3600, // Default to 1 hour
            token_type: 'bearer',
            user: user
          };
        } else {
          // If we have a token but no user data, fetch it
          await this.fetchUserInfo();
        }
      }
    } catch (error) {
      console.error('Failed to initialize auth:', error);
    } finally {
      this.loading = false;
      this._isLoading = false;
    }
  }

  get isAuthenticated(): boolean {
    return !!this.user && !!this.accessToken;
  }

  get isLoadingState(): boolean {
    return this._isLoading;
  }

  get currentUser(): User | null {
    return this.user;
  }

  // Implement required methods from AuthContextType
  async signIn(email: string, password: string): Promise<{ error: Error | null }> {
    try {
      // For mobile, we'll use the web login flow which will redirect to the browser
      await this.login();
      return { error: null };
    } catch (error) {
      console.error('Sign in failed:', error);
      return { 
        error: error instanceof Error ? error : new Error('Sign in failed') 
      };
    }
  }

  async signUp(
    email: string, 
    password: string, 
    metadata?: { full_name?: string }
  ): Promise<{ error: Error | null }> {
    try {
      // For mobile, we'll use the web signup flow which will redirect to the browser
      const redirectUri = AuthSession.makeRedirectUri({
        // @ts-ignore - useProxy is valid but not in the type definitions
        useProxy: Platform.OS === 'android' || Platform.OS === 'ios'
      });
      
      const signupUrl = `https://${AUTH0_DOMAIN}/dbconnections/signup`;
      const response = await fetch(signupUrl, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          client_id: CLIENT_ID,
          email,
          password,
          connection: 'Username-Password-Authentication',
          user_metadata: metadata,
          redirect_uri: redirectUri
        })
      });
      
      if (!response.ok) {
        const error = await response.json();
        throw new Error(error.message || 'Sign up failed');
      }
      
      // After signup, log the user in
      return await this.signIn(email, password);
    } catch (error) {
      console.error('Sign up failed:', error);
      return { 
        error: error instanceof Error ? error : new Error('Sign up failed') 
      };
    }
  }

  async signOut(): Promise<{ error: Error | null }> {
    try {
      // Clear all stored data
      await Promise.all([
        SecureStore.deleteItemAsync(TOKEN_KEY),
        SecureStore.deleteItemAsync(REFRESH_TOKEN_KEY),
        SecureStore.deleteItemAsync(USER_KEY),
      ]);
      
      this.user = null;
      this.session = null;
      this.accessToken = null;
      this.refreshToken = null;
      
      return { error: null };
    } catch (error) {
      console.error('Logout failed:', error);
      return { 
        error: error instanceof Error ? error : new Error('Logout failed') 
      };
    }
  }

  async resetPassword(email: string): Promise<{ error: Error | null }> {
    try {
      const response = await fetch(`https://${AUTH0_DOMAIN}/dbconnections/change_password`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          client_id: CLIENT_ID,
          email,
          connection: 'Username-Password-Authentication'
        })
      });
      
      if (!response.ok) {
        const error = await response.json();
        throw new Error(error.message || 'Password reset failed');
      }
      
      return { error: null };
    } catch (error) {
      console.error('Password reset failed:', error);
      return { 
        error: error instanceof Error ? error : new Error('Password reset failed') 
      };
    }
  }

  async updatePassword(password: string): Promise<{ error: Error | null }> {
    try {
      if (!this.accessToken) {
        throw new Error('Not authenticated');
      }
      
      const response = await fetch(`https://${AUTH0_DOMAIN}/dbconnections/change_password`, {
        method: 'POST',
        headers: { 
          'Content-Type': 'application/json',
          'Authorization': `Bearer ${this.accessToken}`
        },
        body: JSON.stringify({
          client_id: CLIENT_ID,
          connection: 'Username-Password-Authentication',
          password
        })
      });
      
      if (!response.ok) {
        const error = await response.json();
        throw new Error(error.message || 'Password update failed');
      }
      
      return { error: null };
    } catch (error) {
      console.error('Password update failed:', error);
      return { 
        error: error instanceof Error ? error : new Error('Password update failed') 
      };
    }
  }
  
  async login(): Promise<void> {
    try {
      const redirectUri = AuthSession.makeRedirectUri({
        // @ts-ignore - useProxy is valid but not in the type definitions
        useProxy: Platform.OS === 'android' || Platform.OS === 'ios'
      });
      
      const authUrl = `https://${AUTH0_DOMAIN}/authorize?` +
        `response_type=code` +
        `&client_id=${CLIENT_ID}` +
        `&redirect_uri=${encodeURIComponent(redirectUri)}` +
        `&scope=openid profile email offline_access` +
        `&audience=${AUDIENCE}`;

      const result = await WebBrowser.openAuthSessionAsync(authUrl, redirectUri);
      
      if (result.type === 'success') {
        const code = new URLSearchParams(result.url.split('?')[1]).get('code');
        if (code) {
          await this.exchangeCodeForToken(code, redirectUri);
        } else {
          throw new Error('No authorization code received');
        }
      } else {
        throw new Error('Login was cancelled');
      }
    } catch (error) {
      console.error('Login failed:', error);
      throw error;
    }
  }

  private async exchangeCodeForToken(code: string, redirectUri: string): Promise<void> {
    try {
      const tokenUrl = `https://${AUTH0_DOMAIN}/oauth/token`;
      const response = await fetch(tokenUrl, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          grant_type: 'authorization_code',
          client_id: CLIENT_ID,
          code,
          redirect_uri: redirectUri,
        }),
      });

      if (!response.ok) {
        throw new Error('Failed to exchange code for token');
      }

      const { access_token, refresh_token, expires_in } = await response.json();
      
      // Store tokens securely
      await Promise.all([
        SecureStore.setItemAsync(TOKEN_KEY, access_token),
        refresh_token ? SecureStore.setItemAsync(REFRESH_TOKEN_KEY, refresh_token) : Promise.resolve(),
      ]);
      
      this.accessToken = access_token;
      this.refreshToken = refresh_token || null;
      
      // Fetch and store user info
      await this.fetchUserInfo();
      
    } catch (error) {
      console.error('Token exchange failed:', error);
      throw error;
    }
  }

  private async fetchUserInfo(): Promise<void> {
    if (!this.accessToken) return;
    
    try {
      const response = await fetch(`https://${AUTH0_DOMAIN}/userinfo`, {
        headers: { Authorization: `Bearer ${this.accessToken}` },
      });
      
      if (response.ok) {
        this.user = await response.json();
        await SecureStore.setItemAsync(USER_KEY, JSON.stringify(this.user));
      }
    } catch (error) {
      console.error('Failed to fetch user info:', error);
      throw error;
    }
  }

  async logout(): Promise<void> {
    try {
      // Clear all stored data
      await Promise.all([
        SecureStore.deleteItemAsync(TOKEN_KEY),
        SecureStore.deleteItemAsync(REFRESH_TOKEN_KEY),
        SecureStore.deleteItemAsync(USER_KEY),
      ]);
      
      // Clear in-memory state
      this.accessToken = null;
      this.refreshToken = null;
      this.user = null;
      
      // Optional: Call Auth0 logout endpoint
      const logoutUrl = `https://${AUTH0_DOMAIN}/v2/logout?` +
        `client_id=${CLIENT_ID}&` +
        `returnTo=${encodeURIComponent(AuthSession.makeRedirectUri({
          // @ts-ignore - useProxy is valid but not in the type definitions
          useProxy: Platform.OS === 'android' || Platform.OS === 'ios'
        }))}`;
      
      await WebBrowser.openBrowserAsync(logoutUrl);
      
    } catch (error) {
      console.error('Logout failed:', error);
      throw error;
    }
  }

  async getAccessToken(): Promise<string | null> {
    if (!this.accessToken) return null;
    
    // TODO: Implement token refresh logic here
    // Check if token is expired and refresh if needed
    
    return this.accessToken;
  }
}
