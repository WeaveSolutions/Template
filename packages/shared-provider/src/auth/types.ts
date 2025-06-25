export interface User {
  id: string;
  email?: string;
  email_verified?: boolean;
  name?: string;
  given_name?: string;
  family_name?: string;
  picture?: string;
  sub: string;
  updated_at?: string;
  [key: string]: any; // Auth0 custom claims
}

export interface Session {
  access_token: string;
  id_token?: string;
  refresh_token?: string;
  expires_in: number;
  token_type: string;
  scope?: string;
}

export interface AuthProvider {
  id: string;
  name: string;
  type: 'oauth2' | 'passwordless' | 'database';
  connected_at?: string;
  access_token?: string;
  refresh_token?: string;
  scopes?: string[];
}

export interface SignUpOptions {
  email: string;
  password?: string;
  firstName?: string;
  lastName?: string;
  metadata?: Record<string, any>;
}

export interface SignInOptions {
  email: string;
  password: string;
}

export interface GoogleAuthOptions {
  mode: 'signin' | 'signup';
  scopes?: string[];
  metadata?: Record<string, any>;
}

export interface PasskeyResult {
  success: boolean;
  error?: string;
  credentialId?: string;
}

export interface AuthResult {
  error?: Error | null;
}

export interface AuthContextType {
  // Auth state
  user: User | null;
  session: Session | null;
  loading: boolean;
  error: Error | null;
  
  // Computed properties
  isAuthenticated: boolean;
  isLoadingState: boolean;
  currentUser: User | null;
  
  // Auth methods
  initialize: () => Promise<void>;
  
  // Email/Password auth
  signIn: (email: string, password: string) => Promise<AuthResult>;
  signUp: (email: string, password: string, metadata?: Record<string, any>) => Promise<AuthResult>;
  signOut: () => Promise<AuthResult>;
  
  // OAuth methods
  signInWithGoogle: (options?: GoogleAuthOptions) => Promise<void>;
  signInWithProvider: (provider: string, options?: any) => Promise<void>;
  
  // Passkey methods
  signInWithPasskey: () => Promise<PasskeyResult>;
  registerPasskey: () => Promise<PasskeyResult>;
  isPasskeySupported: () => Promise<boolean>;
  
  // Account management
  resetPassword: (email: string) => Promise<AuthResult>;
  updatePassword: (password: string) => Promise<AuthResult>;
  getConnectedProviders: () => AuthProvider[];
  getEnabledProviders: () => string[];
  
  // Token management
  getAccessToken: () => Promise<string | null>;
  refreshSession: () => Promise<void>;
  
  // Auth0 specific methods (for backward compatibility)
  loginWithRedirect?: () => Promise<void>;
  logout?: (options?: any) => Promise<void>;
}
