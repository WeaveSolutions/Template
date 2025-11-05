// Re-export Auth0WebAuth from auth0-auth.ts
export { Auth0WebAuth } from './auth0-auth';

// Also export the WebAuth wrapper class that extends Auth0WebAuth
import { Auth0WebAuth } from './auth0-auth';

// WebAuth extends Auth0WebAuth to maintain backward compatibility
// while transitioning to Auth0 as the authentication provider
export class WebAuth extends Auth0WebAuth {
  // All Auth0 functionality is inherited from Auth0WebAuth
  // Additional web-specific methods can be added here if needed
  
  // Override initialize to handle Auth0 callback
  async initialize(): Promise<void> {
    await super.initialize();
    
    // Handle Auth0 callback after redirect
    if (window.location.search.includes('code=') || window.location.search.includes('error=')) {
      try {
        await this.handleRedirectCallback();
      } catch (error) {
        console.error('Error handling redirect callback:', error);
      }
    }
  }
  
  // Handle the Auth0 redirect callback
  private async handleRedirectCallback(): Promise<void> {
    if (!this.auth0Client) return;
    
    try {
      const result = await this.auth0Client.handleRedirectCallback();
      
      // Clean up URL
      if (result.appState?.returnTo) {
        window.history.replaceState({}, document.title, result.appState.returnTo);
      } else {
        window.history.replaceState({}, document.title, window.location.pathname);
      }
      
      // Fetch user info
      const user = await this.auth0Client.getUser();
      const token = await this.auth0Client.getTokenSilently();
      
      this._user = this.mapAuth0User(user);
      this._session = {
        access_token: token,
        expires_in: 3600,
        token_type: 'Bearer',
      };
      
      // Handle any state that was passed through the auth flow
      if (result.appState?.state) {
        try {
          const stateData = JSON.parse(atob(result.appState.state));
          
          // If this was a signup with metadata, update the user profile
          if (stateData.firstName || stateData.lastName || stateData.metadata) {
            await this.updateUserMetadata({
              firstName: stateData.firstName,
              lastName: stateData.lastName,
              metadata: stateData.metadata,
            });
          }
        } catch (error) {
          console.error('Error parsing state data:', error);
        }
      }
    } catch (error) {
      throw error;
    }
  }
  
  // Update user metadata after successful authentication
  private async updateUserMetadata(data: any): Promise<void> {
    try {
      const token = await this.getAccessToken();
      if (!token) return;
      
      // Call backend API to update user metadata
      const response = await fetch('/api/auth/update-profile', {
        method: 'POST',
        headers: {
          'Authorization': `Bearer ${token}`,
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(data),
      });
      
      if (!response.ok) {
        console.error('Failed to update user metadata');
      }
    } catch (error) {
      console.error('Failed to update user metadata:', error);
    }
  }

  // Implement missing AuthContextType methods
  async signIn({ email, password }: SignInOptions): Promise<void> {
    await super.signIn({ email, password });
  }

  async signUp({ email, password, firstName, lastName, metadata }: SignUpOptions): Promise<void> {
    await super.signUp({ email, password, firstName, lastName, metadata });
  }

  async signOut(): Promise<void> {
    await super.signOut();
  }

  async getAccessToken(): Promise<string | null> {
    return await super.getAccessToken();
  }

  async refreshSession(): Promise<void> {
    await super.refreshSession();
  }

  async signInWithGoogle(options?: GoogleAuthOptions): Promise<void> {
    await super.signInWithGoogle(options);
  }

  async signInWithProvider(provider: string, options?: any): Promise<void> {
    await super.signInWithProvider(provider, options);
  }

  async signInWithPasskey(): Promise<PasskeyResult> {
    return await super.signInWithPasskey();
  }

  async registerPasskey(): Promise<PasskeyResult> {
    return await super.registerPasskey();
  }

  async isPasskeySupported(): Promise<boolean> {
    return await super.isPasskeySupported();
  }

  async resetPassword(email: string): Promise<void> {
    await super.resetPassword(email);
  }

  async updatePassword(password: string): Promise<void> {
    await super.updatePassword(password);
  }

  getConnectedProviders(): AuthProvider[] {
    return super.getConnectedProviders();
  }

  getEnabledProviders(): string[] {
    return super.getEnabledProviders();
  }

  async loginWithRedirect(): Promise<void> {
    await super.loginWithRedirect();
  }

  async logout(options?: any): Promise<void> {
    await super.logout(options);
  }
}
