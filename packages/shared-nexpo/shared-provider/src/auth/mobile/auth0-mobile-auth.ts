import * as AuthSession from 'expo-auth-session';
import * as WebBrowser from 'expo-web-browser';
import * as SecureStore from 'expo-secure-store';
import { Platform } from 'react-native';
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

// Ensure web browser sessions complete properly
WebBrowser.maybeCompleteAuthSession();

export class Auth0MobileAuth implements AuthContextType {
  private _user: User | null = null;
  private _session: Session | null = null;
  private _loading: boolean = true;
  private _error: Error | null = null;
  private discovery: AuthSession.DiscoveryDocument | null = null;

  // Auth0 configuration
  private readonly domain = process.env.EXPO_PUBLIC_AUTH0_DOMAIN!;
  private readonly clientId = process.env.EXPO_PUBLIC_AUTH0_CLIENT_ID!;
  private readonly audience = process.env.EXPO_PUBLIC_AUTH0_AUDIENCE;
  
  get user() { return this._user; }
  get session() { return this._session; }
  get loading() { return this._loading; }
  get error() { return this._error; }
  get isAuthenticated() { return !!this._user && !!this._session; }

  async initialize(): Promise<void> {
    try {
      this._loading = true;
      
      // Initialize Auth0 discovery document
      this.discovery = await AuthSession.fetchDiscoveryAsync(
        `https://${this.domain}`
      );

      // Check for stored session
      const storedSession = await this.getStoredSession();
      if (storedSession) {
        // Validate and refresh if needed
        const isValid = await this.validateSession(storedSession);
        if (isValid) {
          this._session = storedSession;
          await this.fetchUserInfo(storedSession.access_token);
        } else {
          // Try to refresh
          await this.refreshSession();
        }
      }
    } catch (error) {
      this._error = error as Error;
      console.error('Auth0 mobile initialization error:', error);
    } finally {
      this._loading = false;
    }
  }

  private async getStoredSession(): Promise<Session | null> {
    try {
      const sessionData = await SecureStore.getItemAsync('auth0_session');
      if (sessionData) {
        return JSON.parse(sessionData);
      }
    } catch (error) {
      console.error('Failed to get stored session:', error);
    }
    return null;
  }

  private async storeSession(session: Session): Promise<void> {
    try {
      await SecureStore.setItemAsync('auth0_session', JSON.stringify(session));
    } catch (error) {
      console.error('Failed to store session:', error);
    }
  }

  private async clearSession(): Promise<void> {
    try {
      await SecureStore.deleteItemAsync('auth0_session');
    } catch (error) {
      console.error('Failed to clear session:', error);
    }
  }

  private async validateSession(session: Session): Promise<boolean> {
    // Check if token is expired
    // This is a simplified check - in production, decode the JWT
    return true; // Placeholder
  }

  private async fetchUserInfo(accessToken: string): Promise<void> {
    try {
      const response = await fetch(`https://${this.domain}/userinfo`, {
        headers: {
          Authorization: `Bearer ${accessToken}`,
        },
      });

      if (response.ok) {
        const userInfo = await response.json();
        this._user = this.mapAuth0User(userInfo);
      }
    } catch (error) {
      console.error('Failed to fetch user info:', error);
    }
  }

  private mapAuth0User(auth0User: any): User {
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

  private getRedirectUri(): string {
    if (Platform.OS === 'web') {
      return window.location.origin;
    }
    
    // For Expo apps
    const scheme = process.env.EXPO_PUBLIC_APP_SCHEME || 'nexpo';
    return AuthSession.makeRedirectUri({
      scheme,
      path: 'auth',
    });
  }

  async signIn(options: SignInOptions): Promise<void> {
    try {
      // Auth0 uses universal login
      await this.authenticate({
        login_hint: options.email,
        screen_hint: 'signin',
      });
    } catch (error) {
      this._error = error as Error;
      throw error;
    }
  }

  async signUp(options: SignUpOptions): Promise<void> {
    try {
      // Auth0 uses universal login
      const state = btoa(JSON.stringify({
        firstName: options.firstName,
        lastName: options.lastName,
        metadata: options.metadata,
      }));

      await this.authenticate({
        login_hint: options.email,
        screen_hint: 'signup',
        state,
      });
    } catch (error) {
      this._error = error as Error;
      throw error;
    }
  }

  private async authenticate(params: Record<string, string> = {}): Promise<void> {
    if (!this.discovery) {
      throw new Error('Auth0 discovery not initialized');
    }

    const redirectUri = this.getRedirectUri();
    const request = new AuthSession.AuthRequest({
      clientId: this.clientId,
      scopes: ['openid', 'profile', 'email', 'offline_access'],
      redirectUri,
      responseType: AuthSession.ResponseType.Code,
      extraParams: {
        audience: this.audience || '',
        ...params,
      },
    });

    const result = await request.promptAsync(this.discovery);

    if (result.type === 'success' && result.params.code) {
      await this.exchangeCodeForToken(result.params.code, request.codeVerifier!);
    } else if (result.type === 'error') {
      throw new Error(result.error?.message || 'Authentication failed');
    }
  }

  private async exchangeCodeForToken(code: string, codeVerifier: string): Promise<void> {
    if (!this.discovery?.tokenEndpoint) {
      throw new Error('Token endpoint not found');
    }

    const response = await fetch(this.discovery.tokenEndpoint, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/x-www-form-urlencoded',
      },
      body: new URLSearchParams({
        grant_type: 'authorization_code',
        client_id: this.clientId,
        code,
        redirect_uri: this.getRedirectUri(),
        code_verifier: codeVerifier,
      }).toString(),
    });

    if (!response.ok) {
      throw new Error('Failed to exchange code for token');
    }

    const tokenData = await response.json();
    
    this._session = {
      access_token: tokenData.access_token,
      id_token: tokenData.id_token,
      refresh_token: tokenData.refresh_token,
      expires_in: tokenData.expires_in,
      token_type: tokenData.token_type,
      scope: tokenData.scope,
    };

    await this.storeSession(this._session);
    await this.fetchUserInfo(tokenData.access_token);
  }

  async signOut(): Promise<void> {
    try {
      // Clear local session
      await this.clearSession();
      this._user = null;
      this._session = null;

      // Open logout URL in browser
      if (this.discovery?.endSessionEndpoint) {
        const logoutUrl = `${this.discovery.endSessionEndpoint}?` +
          `client_id=${this.clientId}&` +
          `returnTo=${encodeURIComponent(this.getRedirectUri())}`;
        
        await WebBrowser.openBrowserAsync(logoutUrl);
      }
    } catch (error) {
      this._error = error as Error;
      throw error;
    }
  }

  async signInWithGoogle(options?: GoogleAuthOptions): Promise<void> {
    try {
      await this.authenticate({
        connection: 'google-oauth2',
        screen_hint: options?.mode || 'signin',
        scope: options?.scopes?.join(' ') || 'openid profile email',
        ...(options?.metadata && {
          state: btoa(JSON.stringify(options.metadata)),
        }),
      });
    } catch (error) {
      this._error = error as Error;
      throw error;
    }
  }

  async signInWithProvider(provider: string, options?: any): Promise<void> {
    try {
      await this.authenticate({
        connection: provider,
        ...options,
      });
    } catch (error) {
      this._error = error as Error;
      throw error;
    }
  }

  async signInWithPasskey(): Promise<PasskeyResult> {
    // Passkeys are not yet fully supported on React Native
    // This would use platform-specific biometric APIs
    return {
      success: false,
      error: 'Passkeys not yet supported on mobile',
    };
  }

  async registerPasskey(): Promise<PasskeyResult> {
    // Passkeys are not yet fully supported on React Native
    // This would use platform-specific biometric APIs
    return {
      success: false,
      error: 'Passkeys not yet supported on mobile',
    };
  }

  async isPasskeySupported(): Promise<boolean> {
    // Check for biometric support on mobile
    // This would use expo-local-authentication
    return false;
  }

  async resetPassword(email: string): Promise<void> {
    try {
      const response = await fetch(`https://${this.domain}/dbconnections/change_password`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          client_id: this.clientId,
          email,
          connection: 'Username-Password-Authentication',
        }),
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
    if (!this.isAuthenticated) throw new Error('Not authenticated');
    
    try {
      const token = await this.getAccessToken();
      if (!token) throw new Error('Failed to get access token');

      // This would call your backend API
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
    if (!this._user) return [];

    const providers: AuthProvider[] = [];
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
    const providers: string[] = [];
    
    if (process.env.EXPO_PUBLIC_ENABLE_GOOGLE_LOGIN === 'true') {
      providers.push('google-oauth2');
    }
    if (process.env.EXPO_PUBLIC_ENABLE_GITHUB_LOGIN === 'true') {
      providers.push('github');
    }
    if (process.env.EXPO_PUBLIC_ENABLE_MICROSOFT_LOGIN === 'true') {
      providers.push('windowslive');
    }
    if (process.env.EXPO_PUBLIC_ENABLE_APPLE_LOGIN === 'true') {
      providers.push('apple');
    }
    if (process.env.EXPO_PUBLIC_ENABLE_SNAPCHAT_LOGIN === 'true') {
      providers.push('snapchat');
    }
    if (process.env.EXPO_PUBLIC_ENABLE_INSTAGRAM_LOGIN === 'true') {
      providers.push('instagram');
    }
    if (process.env.EXPO_PUBLIC_ENABLE_TIKTOK_LOGIN === 'true') {
      providers.push('tiktok');
    }
    
    return providers;
  }

  async getAccessToken(): Promise<string | null> {
    if (!this._session?.access_token) return null;
    
    // Check if token needs refresh
    // In production, decode JWT to check expiry
    
    return this._session.access_token;
  }

  async refreshSession(): Promise<void> {
    if (!this._session?.refresh_token || !this.discovery?.tokenEndpoint) {
      throw new Error('Cannot refresh session');
    }

    try {
      const response = await fetch(this.discovery.tokenEndpoint, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/x-www-form-urlencoded',
        },
        body: new URLSearchParams({
          grant_type: 'refresh_token',
          client_id: this.clientId,
          refresh_token: this._session.refresh_token,
        }).toString(),
      });

      if (!response.ok) {
        throw new Error('Failed to refresh token');
      }

      const tokenData = await response.json();
      
      this._session = {
        access_token: tokenData.access_token,
        id_token: tokenData.id_token,
        refresh_token: tokenData.refresh_token || this._session.refresh_token,
        expires_in: tokenData.expires_in,
        token_type: tokenData.token_type,
        scope: tokenData.scope,
      };

      await this.storeSession(this._session);
      await this.fetchUserInfo(tokenData.access_token);
    } catch (error) {
      this._error = error as Error;
      throw error;
    }
  }

  // Backward compatibility methods
  async loginWithRedirect(): Promise<void> {
    await this.authenticate();
  }

  async logout(options?: any): Promise<void> {
    await this.signOut();
  }
}
