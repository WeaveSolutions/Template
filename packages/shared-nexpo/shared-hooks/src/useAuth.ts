import { useCallback, useContext } from 'react';
import { AuthContext, AuthContextType } from '@nexpo/shared-provider';

type AuthResult = {
  success: boolean;
  error?: Error;
};

export function useAuth() {
  const context = useContext<AuthContextType | undefined>(AuthContext);
  
  if (!context) {
    throw new Error('useAuth must be used within an AuthProvider');
  }

  const { 
    user, 
    session, 
    loading, 
    signIn: login, 
    signOut: logout, 
    signUp, 
    resetPassword, 
    updatePassword 
  } = context;

  const loginMethod = useCallback(async (email: string, password: string): Promise<AuthResult> => {
    try {
      const { error } = await login(email, password);
      if (error) throw error;
      return { success: true };
    } catch (error) {
      console.error('Login failed:', error);
      return { 
        success: false, 
        error: error instanceof Error ? error : new Error('Login failed') 
      };
    }
  }, [login]);

  const logoutMethod = useCallback(async (): Promise<AuthResult> => {
    try {
      const { error } = await logout();
      if (error) throw error;
      return { success: true };
    } catch (error) {
      console.error('Logout failed:', error);
      return { 
        success: false, 
        error: error instanceof Error ? error : new Error('Logout failed') 
      };
    }
  }, [logout]);

  const register = useCallback(async (
    email: string, 
    password: string, 
    metadata?: { full_name?: string }
  ): Promise<AuthResult> => {
    try {
      const { error } = await signUp(email, password, metadata);
      if (error) throw error;
      return { success: true };
    } catch (error) {
      console.error('Registration failed:', error);
      return { 
        success: false, 
        error: error instanceof Error ? error : new Error('Registration failed')
      };
    }
  }, [signUp]);

  const getAccessToken = useCallback(async () => {
    try {
      return session?.access_token || null;
    } catch (error) {
      console.error('Failed to get access token:', error);
      return null;
    }
  }, [session]);

  return {
    user,
    session,
    isAuthenticated: !!user,
    isLoading: loading,
    login: loginMethod,
    logout: logoutMethod,
    register,
    getAccessToken,
    resetPassword,
    updatePassword,
  };
}
