import { 
  AuthContextType, 
  User, 
  Session, 
  AuthResult, 
  AuthProvider, 
  GoogleAuthOptions, 
  PasskeyResult 
} from '../types';

/**
 * Development authentication provider that simulates Auth0 behavior
 * for local development without external dependencies
 */
export class DevAuth implements AuthContextType {
  public user: User | null = null;
  public session: Session | null = null;
  public loading: boolean = false;
  public error: Error | null = null;

  // Computed properties
  get isAuthenticated(): boolean {
    return !!this.user && !!this.session;
  }

  get isLoadingState(): boolean {
    return this.loading;
  }

  get currentUser(): User | null {
    return this.user;
  }

  async initialize(): Promise<void> {
    // Simulate initialization delay
    this.loading = true;
    await new Promise(resolve => setTimeout(resolve, 100));
    
    // Check for stored user
    const storedUser = localStorage.getItem('dev-user');
    const storedSession = localStorage.getItem('dev-session');
    
    if (storedUser && storedSession) {
      this.user = JSON.parse(storedUser);
      this.session = JSON.parse(storedSession);
    }
    
    this.loading = false;
  }

  async signIn(email: string, password: string): Promise<AuthResult> {
    this.loading = true;
    this.error = null;

    try {
      // Simulate API delay
      await new Promise(resolve => setTimeout(resolve, 1000));

      // Simple validation for development
      if (!email || !password) {
        throw new Error('Email and password are required');
      }

      // Create mock user and session
      const mockUser: User = {
        id: 'dev-user-123',
        email,
        email_verified: true,
        name: 'Dev User',
        given_name: 'Dev',
        family_name: 'User',
        picture: 'https://via.placeholder.com/100',
        sub: 'dev-user-123',
        updated_at: new Date().toISOString(),
      };

      const mockSession: Session = {
        access_token: 'dev-access-token-' + Date.now(),
        id_token: 'dev-id-token-' + Date.now(),
        refresh_token: 'dev-refresh-token-' + Date.now(),
        expires_in: 3600,
        token_type: 'Bearer',
        scope: 'openid profile email',
      };

      // Store in localStorage for persistence
      localStorage.setItem('dev-user', JSON.stringify(mockUser));
      localStorage.setItem('dev-session', JSON.stringify(mockSession));

      this.user = mockUser;
      this.session = mockSession;

      return { error: null };
    } catch (error) {
      this.error = error as Error;
      return { error: error as Error };
    } finally {
      this.loading = false;
    }
  }

  async signUp(email: string, password: string, metadata?: Record<string, any>): Promise<AuthResult> {
    // For development, signup is the same as signin
    return this.signIn(email, password);
  }

  async signOut(): Promise<AuthResult> {
    try {
      // Clear stored data
      localStorage.removeItem('dev-user');
      localStorage.removeItem('dev-session');
      
      this.user = null;
      this.session = null;
      this.error = null;

      return { error: null };
    } catch (error) {
      this.error = error as Error;
      return { error: error as Error };
    }
  }

  async signInWithGoogle(options?: GoogleAuthOptions): Promise<void> {
    // Simulate Google OAuth flow
    this.loading = true;
    await new Promise(resolve => setTimeout(resolve, 1500));
    
    const result = await this.signIn('google-user@example.com', 'google-auth');
    if (result.error) {
      throw result.error;
    }
    
    this.loading = false;
  }

  async signInWithProvider(provider: string, options?: any): Promise<void> {
    // Simulate OAuth provider flow
    this.loading = true;
    await new Promise(resolve => setTimeout(resolve, 1500));
    
    const result = await this.signIn(`${provider}-user@example.com`, `${provider}-auth`);
    if (result.error) {
      throw result.error;
    }
    
    this.loading = false;
  }

  async signInWithPasskey(): Promise<PasskeyResult> {
    // Simulate passkey authentication
    await new Promise(resolve => setTimeout(resolve, 2000));
    
    const result = await this.signIn('passkey-user@example.com', 'passkey-auth');
    return {
      success: !result.error,
      error: result.error?.message,
      credentialId: result.error ? undefined : 'dev-credential-123',
    };
  }

  async registerPasskey(): Promise<PasskeyResult> {
    // Simulate passkey registration
    await new Promise(resolve => setTimeout(resolve, 2000));
    
    return {
      success: true,
      credentialId: 'dev-credential-456',
    };
  }

  async isPasskeySupported(): Promise<boolean> {
    // Simulate passkey support check
    return true;
  }

  async resetPassword(email: string): Promise<AuthResult> {
    this.loading = true;
    
    try {
      // Simulate password reset delay
      await new Promise(resolve => setTimeout(resolve, 1000));
      
      if (!email) {
        throw new Error('Email is required for password reset');
      }
      
      console.log(`[DEV] Password reset email sent to: ${email}`);
      return { error: null };
    } catch (error) {
      return { error: error as Error };
    } finally {
      this.loading = false;
    }
  }

  async updatePassword(password: string): Promise<AuthResult> {
    this.loading = true;
    
    try {
      // Simulate password update delay
      await new Promise(resolve => setTimeout(resolve, 1000));
      
      if (!password) {
        throw new Error('New password is required');
      }
      
      console.log('[DEV] Password updated successfully');
      return { error: null };
    } catch (error) {
      return { error: error as Error };
    } finally {
      this.loading = false;
    }
  }

  getConnectedProviders(): AuthProvider[] {
    // Return mock connected providers
    return [
      {
        id: 'google-oauth2',
        name: 'Google',
        type: 'oauth2',
        connected_at: new Date().toISOString(),
      },
      {
        id: 'github',
        name: 'GitHub',
        type: 'oauth2',
        connected_at: new Date().toISOString(),
      },
    ];
  }

  getEnabledProviders(): string[] {
    return ['google-oauth2', 'github', 'auth0', 'email'];
  }

  async getAccessToken(): Promise<string | null> {
    return this.session?.access_token || null;
  }

  async refreshSession(): Promise<void> {
    if (!this.session?.refresh_token) {
      throw new Error('No refresh token available');
    }
    
    // Simulate token refresh
    await new Promise(resolve => setTimeout(resolve, 500));
    
    const newSession: Session = {
      ...this.session,
      access_token: 'dev-refreshed-token-' + Date.now(),
      expires_in: 3600,
    };
    
    localStorage.setItem('dev-session', JSON.stringify(newSession));
    this.session = newSession;
  }

  // Auth0 compatibility methods
  async loginWithRedirect(): Promise<void> {
    // Simulate redirect-based login
    await this.signIn('redirect-user@example.com', 'redirect-auth');
  }

  async logout(options?: any): Promise<void> {
    await this.signOut();
  }
}
