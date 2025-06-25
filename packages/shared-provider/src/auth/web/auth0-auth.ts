import { Auth0Client } from '@auth0/auth0-spa-js';
import { 
  AuthContextType, 
  User, 
  Session, 
  SignInOptions, 
  SignUpOptions,
  GoogleAuthOptions,
  PasskeyResult,
  AuthProvider
} from '../types';

export class Auth0WebAuth implements AuthContextType {
  protected auth0Client: Auth0Client | null = null;
  protected _user: User | null = null;
  protected _session: Session | null = null;
  protected _loading: boolean = true;
  protected _error: Error | null = null;

  get user() { return this._user; }
  get session() { return this._session; }
  get loading() { return this._loading; }
  get error() { return this._error; }
  get isAuthenticated() { return !!this._user && !!this._session; }
  get isLoadingState() { return this._loading; }
  get currentUser() { return this._user; }

  async initialize(): Promise<void> {
    try {
      this._loading = true;
      
      // Initialize Auth0 client
      this.auth0Client = new Auth0Client({
        domain: process.env.NEXT_PUBLIC_AUTH0_DOMAIN!,
        clientId: process.env.NEXT_PUBLIC_AUTH0_CLIENT_ID!,
        authorizationParams: {
          redirect_uri: window.location.origin,
          audience: process.env.NEXT_PUBLIC_AUTH0_AUDIENCE,
          scope: 'openid profile email offline_access',
        },
        cacheLocation: 'localstorage',
        useRefreshTokens: true,
      });

      // Check if user is already authenticated
      const isAuthenticated = await this.auth0Client.isAuthenticated();
      
      if (isAuthenticated) {
        const user = await this.auth0Client.getUser();
        const token = await this.auth0Client.getTokenSilently();
        
        this._user = this.mapAuth0User(user);
        this._session = {
          access_token: token,
          expires_in: 3600,
          token_type: 'Bearer',
        };
      }
    } catch (error) {
      this._error = error as Error;
      console.error('Auth0 initialization error:', error);
    } finally {
      this._loading = false;
    }
  }

  protected mapAuth0User(auth0User: any): User {
    return {
      id: auth0User.sub,
      sub: auth0User.sub,
      email: auth0User.email,
      email_verified: auth0User.email_verified,
      name: auth0User.name,
      given_name: auth0User.given_name,
      family_name: auth0User.family_name,
      picture: auth0User.picture,
      updated_at: auth0User.updated_at,
      ...auth0User,
    };
  }

  protected async getAccessToken(): Promise<string | null> {
    if (!this.auth0Client) return null;
    
    try {
      const token = await this.auth0Client.getTokenSilently();
      return token;
    } catch (error) {
      console.error('Failed to get access token:', error);
      return null;
    }
  }

  async signIn(options: SignInOptions): Promise<void> {
    if (!this.auth0Client) throw new Error('Auth0 not initialized');
    
    try {
      // Auth0 uses universal login, so we redirect to the login page
      await this.auth0Client.loginWithRedirect({
        authorizationParams: {
          login_hint: options.email,
          screen_hint: 'signin',
        },
      });
    } catch (error) {
      this._error = error as Error;
      throw error;
    }
  }

  async signUp(options: SignUpOptions): Promise<void> {
    if (!this.auth0Client) throw new Error('Auth0 not initialized');
    
    try {
      // Auth0 uses universal login, so we redirect to the signup page
      await this.auth0Client.loginWithRedirect({
        authorizationParams: {
          login_hint: options.email,
          screen_hint: 'signup',
          // Pass additional metadata through state
          ...(options.metadata && {
            state: btoa(JSON.stringify({
              firstName: options.firstName,
              lastName: options.lastName,
              metadata: options.metadata,
            })),
          }),
        },
      });
    } catch (error) {
      this._error = error as Error;
      throw error;
    }
  }

  async signOut(): Promise<void> {
    if (!this.auth0Client) return;
    
    try {
      await this.auth0Client.logout({
        logoutParams: {
          returnTo: window.location.origin,
        },
      });
      
      this._user = null;
      this._session = null;
    } catch (error) {
      this._error = error as Error;
      throw error;
    }
  }

  async signInWithGoogle(options?: GoogleAuthOptions): Promise<void> {
    if (!this.auth0Client) throw new Error('Auth0 not initialized');
    
    try {
      await this.auth0Client.loginWithRedirect({
        authorizationParams: {
          connection: 'google-oauth2',
          screen_hint: options?.mode || 'signin',
          scope: options?.scopes?.join(' ') || 'openid profile email',
          ...(options?.metadata && {
            state: btoa(JSON.stringify(options.metadata)),
          }),
        },
      });
    } catch (error) {
      this._error = error as Error;
      throw error;
    }
  }

  async signInWithProvider(provider: string, options?: any): Promise<void> {
    if (!this.auth0Client) throw new Error('Auth0 not initialized');
    
    try {
      await this.auth0Client.loginWithRedirect({
        authorizationParams: {
          connection: provider,
          ...options,
        },
      });
    } catch (error) {
      this._error = error as Error;
      throw error;
    }
  }

  async signInWithPasskey(): Promise<PasskeyResult> {
    if (!this.auth0Client) throw new Error('Auth0 not initialized');
    
    try {
      // Check if WebAuthn is supported
      if (!window.PublicKeyCredential) {
        return { success: false, error: 'WebAuthn not supported' };
      }

      // Use Auth0's passwordless WebAuthn flow
      await this.auth0Client.loginWithRedirect({
        authorizationParams: {
          connection: 'webauthn-platform',
          prompt: 'none',
        },
      });

      return { success: true };
    } catch (error) {
      const err = error as Error;
      return { 
        success: false, 
        error: err.message || 'Passkey authentication failed' 
      };
    }
  }

  async registerPasskey(): Promise<PasskeyResult> {
    if (!this.auth0Client) throw new Error('Auth0 not initialized');
    
    try {
      // Check if WebAuthn is supported
      if (!window.PublicKeyCredential) {
        return { success: false, error: 'WebAuthn not supported' };
      }

      // For registration, we need to be authenticated first
      if (!this.isAuthenticated) {
        return { success: false, error: 'Must be authenticated to register passkey' };
      }

      // Use Auth0 Management API to register WebAuthn credential
      const token = await this.getAccessToken();
      if (!token) {
        return { success: false, error: 'Failed to get access token' };
      }

      // This would typically involve calling your backend API which uses
      // Auth0 Management API to register the WebAuthn credential
      const response = await fetch('/api/auth/passkey/register', {
        method: 'POST',
        headers: {
          'Authorization': `Bearer ${token}`,
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          userId: this._user?.id,
        }),
      });

      if (!response.ok) {
        throw new Error('Failed to register passkey');
      }

      const result = await response.json();
      return { 
        success: true, 
        credentialId: result.credentialId 
      };
    } catch (error) {
      const err = error as Error;
      return { 
        success: false, 
        error: err.message || 'Passkey registration failed' 
      };
    }
  }

  async isPasskeySupported(): Promise<boolean> {
    // Check for WebAuthn support
    if (!window.PublicKeyCredential) {
      return false;
    }

    // Check for platform authenticator support
    try {
      const available = await PublicKeyCredential.isUserVerifyingPlatformAuthenticatorAvailable();
      return available;
    } catch {
      return false;
    }
  }

  async resetPassword(email: string): Promise<void> {
    if (!this.auth0Client) throw new Error('Auth0 not initialized');
    
    try {
      // Call your backend API to trigger Auth0 password reset
      const response = await fetch('/api/auth/reset-password', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ email }),
      });

      if (!response.ok) {
        throw new Error('Failed to send password reset email');
      }
    } catch (error) {
      this._error = error as Error;
      throw error;
    }
  }

  async updatePassword(password: string): Promise<void> {
    if (!this.auth0Client) throw new Error('Auth0 not initialized');
    if (!this.isAuthenticated) throw new Error('Not authenticated');
    
    try {
      const token = await this.getAccessToken();
      if (!token) throw new Error('Failed to get access token');

      // Call your backend API to update password via Auth0 Management API
      const response = await fetch('/api/auth/update-password', {
        method: 'POST',
        headers: {
          'Authorization': `Bearer ${token}`,
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({ password }),
      });

      if (!response.ok) {
        throw new Error('Failed to update password');
      }
    } catch (error) {
      this._error = error as Error;
      throw error;
    }
  }

  getConnectedProviders(): AuthProvider[] {
    // This would typically come from your backend/account center
    // For now, return mock data based on user metadata
    if (!this._user) return [];

    const providers: AuthProvider[] = [];
    
    // Check user identities from Auth0
    const identities = this._user['identities'] || [];
    
    identities.forEach((identity: any) => {
      providers.push({
        id: identity.provider,
        name: identity.connection,
        type: identity.isSocial ? 'oauth2' : 'database',
        connected_at: identity.profileData?.updated_at,
      });
    });

    return providers;
  }

  getEnabledProviders(): string[] {
    // Return list of enabled providers based on environment variables
    const providers: string[] = [];
    
    if (process.env.NEXT_PUBLIC_ENABLE_GOOGLE_LOGIN === 'true') {
      providers.push('google-oauth2');
    }
    if (process.env.NEXT_PUBLIC_ENABLE_GITHUB_LOGIN === 'true') {
      providers.push('github');
    }
    if (process.env.NEXT_PUBLIC_ENABLE_MICROSOFT_LOGIN === 'true') {
      providers.push('windowslive');
    }
    if (process.env.NEXT_PUBLIC_ENABLE_APPLE_LOGIN === 'true') {
      providers.push('apple');
    }
    if (process.env.NEXT_PUBLIC_ENABLE_FACEBOOK_LOGIN === 'true') {
      providers.push('facebook');
    }
    if (process.env.NEXT_PUBLIC_ENABLE_DISCORD_LOGIN === 'true') {
      providers.push('discord');
    }
    if (process.env.NEXT_PUBLIC_ENABLE_CODERABBIT_LOGIN === 'true') {
      providers.push('coderabbit');
    }
    
    return providers;
  }

  async refreshSession(): Promise<void> {
    if (!this.auth0Client) throw new Error('Auth0 not initialized');
    
    try {
      const token = await this.auth0Client.getTokenSilently({ 
        cacheMode: 'off' 
      });
      
      this._session = {
        access_token: token,
        expires_in: 3600,
        token_type: 'Bearer',
      };
    } catch (error) {
      this._error = error as Error;
      throw error;
    }
  }

  // Backward compatibility methods
  async loginWithRedirect(): Promise<void> {
    if (!this.auth0Client) throw new Error('Auth0 not initialized');
    await this.auth0Client.loginWithRedirect();
  }

  async logout(options?: any): Promise<void> {
    await this.signOut();
  }
}
