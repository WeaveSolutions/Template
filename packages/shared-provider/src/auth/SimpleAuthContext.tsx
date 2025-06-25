import React, { createContext, useContext, useState, useEffect, ReactNode } from 'react';
import { AuthContextType, User, Session, AuthResult } from './types';

// Simple AuthContext that doesn't depend on platform-specific packages
export const AuthContext = createContext<AuthContextType | undefined>(undefined);

interface AuthProviderProps {
  children: ReactNode;
}

export const SimpleAuthProvider: React.FC<AuthProviderProps> = ({ children }) => {
  const [user, setUser] = useState<User | null>(null);
  const [session, setSession] = useState<Session | null>(null);
  const [loading, setLoading] = useState<boolean>(true);
  const [error, setError] = useState<Error | null>(null);

  // Mock auth methods that return the expected AuthResult format
  const signIn = async (email: string, password: string): Promise<AuthResult> => {
    try {
      setLoading(true);
      setError(null);
      
      // This is a mock implementation - replace with actual auth logic
      console.log('Mock signIn:', email);
      
      // Simulate successful sign in
      const mockUser: User = {
        id: '1',
        email: email,
        sub: '1',
        name: email.split('@')[0]
      };
      
      const mockSession: Session = {
        access_token: 'mock-token',
        expires_in: 3600,
        token_type: 'Bearer'
      };
      
      setUser(mockUser);
      setSession(mockSession);
      
      return { error: null };
    } catch (err) {
      const error = err instanceof Error ? err : new Error('Sign in failed');
      setError(error);
      return { error };
    } finally {
      setLoading(false);
    }
  };

  const signUp = async (email: string, password: string, metadata?: Record<string, any>): Promise<AuthResult> => {
    try {
      setLoading(true);
      setError(null);
      
      // This is a mock implementation - replace with actual auth logic
      console.log('Mock signUp:', email, metadata);
      
      return { error: null };
    } catch (err) {
      const error = err instanceof Error ? err : new Error('Sign up failed');
      setError(error);
      return { error };
    } finally {
      setLoading(false);
    }
  };

  const signOut = async (): Promise<AuthResult> => {
    try {
      setLoading(true);
      setError(null);
      
      setUser(null);
      setSession(null);
      
      return { error: null };
    } catch (err) {
      const error = err instanceof Error ? err : new Error('Sign out failed');
      setError(error);
      return { error };
    } finally {
      setLoading(false);
    }
  };

  const resetPassword = async (email: string): Promise<AuthResult> => {
    try {
      console.log('Mock reset password for:', email);
      return { error: null };
    } catch (err) {
      const error = err instanceof Error ? err : new Error('Reset password failed');
      return { error };
    }
  };

  const updatePassword = async (password: string): Promise<AuthResult> => {
    try {
      console.log('Mock update password');
      return { error: null };
    } catch (err) {
      const error = err instanceof Error ? err : new Error('Update password failed');
      return { error };
    }
  };

  // Initialize on mount
  useEffect(() => {
    const initialize = async () => {
      try {
        setLoading(true);
        // Check for existing session (localStorage, etc.)
        // This is where you'd restore a saved session
        console.log('Initializing auth...');
      } catch (err) {
        console.error('Auth initialization error:', err);
        setError(err instanceof Error ? err : new Error('Initialization failed'));
      } finally {
        setLoading(false);
      }
    };

    initialize();
  }, []);

  // Mock implementations for other required methods
  const initialize = async (): Promise<void> => {
    // Already handled in useEffect
  };

  const signInWithGoogle = async (): Promise<void> => {
    console.log('Mock Google sign in');
  };

  const signInWithProvider = async (provider: string): Promise<void> => {
    console.log('Mock provider sign in:', provider);
  };

  const signInWithPasskey = async (): Promise<any> => {
    return { success: false, error: 'Passkeys not implemented' };
  };

  const registerPasskey = async (): Promise<any> => {
    return { success: false, error: 'Passkeys not implemented' };
  };

  const isPasskeySupported = async (): Promise<boolean> => {
    return false;
  };

  const getConnectedProviders = () => {
    return [];
  };

  const getEnabledProviders = () => {
    return ['email'];
  };

  const getAccessToken = async (): Promise<string | null> => {
    return session?.access_token || null;
  };

  const refreshSession = async (): Promise<void> => {
    console.log('Mock refresh session');
  };

  const contextValue: AuthContextType = {
    user,
    session,
    loading,
    error,
    // Computed properties
    get isAuthenticated() { return !!user && !!session; },
    get isLoadingState() { return loading; },
    get currentUser() { return user; },
    // Methods
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
  };

  return (
    <AuthContext.Provider value={contextValue}>
      {children}
    </AuthContext.Provider>
  );
};

export const useAuth = (): AuthContextType => {
  const context = useContext(AuthContext);
  if (context === undefined) {
    throw new Error('useAuth must be used within an AuthProvider');
  }
  return context;
};
