import { Auth0WebAuth } from './auth-web-auth0';
import { DevAuth } from './auth-dev';
import { AuthContextType } from '../types';

/**
 * WebAuth intelligently selects between Auth0 and Development auth providers
 * based on environment configuration.
 */
export class WebAuth implements AuthContextType {
  private authProvider: AuthContextType;

  constructor() {
    // Check if Auth0 is properly configured
    const isAuth0Configured = this.checkAuth0Configuration();
    
    if (isAuth0Configured) {
      console.log('✅ Using Auth0 authentication provider');
      this.authProvider = new Auth0WebAuth();
    } else {
      console.warn('⚠️  Auth0 not configured, using development authentication provider');
      console.warn('⚠️  Configure Auth0 environment variables for production use');
      this.authProvider = new DevAuth();
    }
  }

  private checkAuth0Configuration(): boolean {
    const domain = process.env.NEXT_PUBLIC_AUTH0_DOMAIN;
    const clientId = process.env.NEXT_PUBLIC_AUTH0_CLIENT_ID;
    
    return !!(
      domain && 
      clientId && 
      domain !== 'your-tenant.auth0.com' && 
      clientId !== 'your-auth0-client-id' &&
      domain.includes('.auth0.com')
    );
  }

  // Delegate all AuthContextType methods to the selected provider
  get user() { return this.authProvider.user; }
  get session() { return this.authProvider.session; }
  get loading() { return this.authProvider.loading; }
  get error() { return this.authProvider.error; }
  get isAuthenticated() { return this.authProvider.isAuthenticated; }
  get isLoadingState() { return this.authProvider.isLoadingState; }
  get currentUser() { return this.authProvider.currentUser; }

  async initialize(): Promise<void> {
    return this.authProvider.initialize();
  }

  async signIn(options: any): Promise<void> {
    return this.authProvider.signIn(options);
  }

  async signUp(options: any): Promise<void> {
    return this.authProvider.signUp(options);
  }

  async signOut(): Promise<void> {
    return this.authProvider.signOut();
  }

  async getAccessToken(): Promise<string | null> {
    return this.authProvider.getAccessToken();
  }

  async refreshSession(): Promise<void> {
    return this.authProvider.refreshSession();
  }

  async signInWithGoogle(options?: any): Promise<void> {
    return this.authProvider.signInWithGoogle(options);
  }

  async signInWithProvider(provider: any, options?: any): Promise<void> {
    return this.authProvider.signInWithProvider(provider, options);
  }

  async signInWithPasskey(): Promise<any> {
    return this.authProvider.signInWithPasskey();
  }

  async registerPasskey(): Promise<any> {
    return this.authProvider.registerPasskey();
  }

  async isPasskeySupported(): Promise<boolean> {
    return this.authProvider.isPasskeySupported();
  }

  async resetPassword(email: string): Promise<void> {
    return this.authProvider.resetPassword(email);
  }

  async updatePassword(password: string): Promise<void> {
    return this.authProvider.updatePassword(password);
  }

  getConnectedProviders(): any[] {
    return this.authProvider.getConnectedProviders();
  }

  getEnabledProviders(): string[] {
    return this.authProvider.getEnabledProviders();
  }
}
