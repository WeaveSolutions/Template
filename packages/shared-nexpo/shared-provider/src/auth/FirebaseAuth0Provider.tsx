import React, { createContext, useContext, useEffect, useState, ReactNode } from 'react';
import { useAuth0 } from '@auth0/nextjs-auth0';
import { Platform } from 'react-native';
import {
  signInWithAuth0,
  signOut as firebaseSignOut,
  subscribeToAuthState,
  getCurrentUserWithClaims,
  subscribeToUserProfile,
  updateUserProfile,
} from '../../shared-utils/src/firebase-auth0';
import { User } from 'firebase/auth';

interface FirebaseAuth0ContextType {
  // Auth0 properties
  auth0User: any;
  auth0Loading: boolean;
  auth0Error: any;
  loginWithAuth0: () => void;
  logoutFromAuth0: () => void;
  
  // Firebase properties
  firebaseUser: User | null;
  firebaseLoading: boolean;
  firebaseError: any;
  userProfile: any;
  roles: string[];
  permissions: string[];
  
  // Combined methods
  logout: () => Promise<void>;
  updateProfile: (data: any) => Promise<void>;
  hasRole: (role: string) => boolean;
  hasPermission: (permission: string) => boolean;
}

const FirebaseAuth0Context = createContext<FirebaseAuth0ContextType | undefined>(undefined);

interface FirebaseAuth0ProviderProps {
  children: ReactNode;
}

export function FirebaseAuth0Provider({ children }: FirebaseAuth0ProviderProps) {
  // Auth0 state
  const {
    user: auth0User,
    error: auth0Error,
    isLoading: auth0Loading,
    loginWithRedirect,
    logout: auth0Logout,
    getAccessTokenSilently,
  } = useAuth0();

  // Firebase state
  const [firebaseUser, setFirebaseUser] = useState<User | null>(null);
  const [firebaseLoading, setFirebaseLoading] = useState(true);
  const [firebaseError, setFirebaseError] = useState<any>(null);
  const [userProfile, setUserProfile] = useState<any>(null);
  const [roles, setRoles] = useState<string[]>([]);
  const [permissions, setPermissions] = useState<string[]>([]);

  // Sync Auth0 user to Firebase
  useEffect(() => {
    const syncToFirebase = async () => {
      if (auth0User && !auth0Loading) {
        try {
          setFirebaseLoading(true);
          setFirebaseError(null);

          // Get Auth0 access token
          const accessToken = await getAccessTokenSilently();

          // Sign in to Firebase with Auth0 token
          const fbUser = await signInWithAuth0(accessToken);
          
          // Get user claims
          const userWithClaims = await getCurrentUserWithClaims();
          if (userWithClaims) {
            setRoles(userWithClaims.roles);
            setPermissions(userWithClaims.permissions);
          }

          setFirebaseUser(fbUser);
        } catch (error) {
          console.error('Firebase sync error:', error);
          setFirebaseError(error);
        } finally {
          setFirebaseLoading(false);
        }
      } else if (!auth0User && !auth0Loading) {
        // User logged out from Auth0, sign out from Firebase
        await firebaseSignOut();
        setFirebaseUser(null);
        setUserProfile(null);
        setRoles([]);
        setPermissions([]);
        setFirebaseLoading(false);
      }
    };

    syncToFirebase();
  }, [auth0User, auth0Loading, getAccessTokenSilently]);

  // Subscribe to Firebase auth state
  useEffect(() => {
    const unsubscribe = subscribeToAuthState((user) => {
      setFirebaseUser(user);
      if (!user) {
        setUserProfile(null);
        setRoles([]);
        setPermissions([]);
      }
    });

    return unsubscribe;
  }, []);

  // Subscribe to user profile updates
  useEffect(() => {
    if (firebaseUser) {
      const unsubscribe = subscribeToUserProfile(firebaseUser.uid, (profile) => {
        setUserProfile(profile);
      });

      return unsubscribe;
    }
  }, [firebaseUser]);

  // Combined logout
  const logout = async () => {
    try {
      // Sign out from Firebase first
      await firebaseSignOut();
      
      // Then logout from Auth0
      auth0Logout({
        logoutParams: {
          returnTo: window.location.origin,
        },
      });
    } catch (error) {
      console.error('Logout error:', error);
      throw error;
    }
  };

  // Update user profile
  const updateProfile = async (data: any) => {
    if (!firebaseUser) throw new Error('User not authenticated');
    
    await updateUserProfile(firebaseUser.uid, data);
  };

  // Check if user has role
  const hasRole = (role: string): boolean => {
    return roles.includes(role);
  };

  // Check if user has permission
  const hasPermission = (permission: string): boolean => {
    return permissions.includes(permission);
  };

  const value: FirebaseAuth0ContextType = {
    // Auth0
    auth0User,
    auth0Loading,
    auth0Error,
    loginWithAuth0: loginWithRedirect,
    logoutFromAuth0: auth0Logout,
    
    // Firebase
    firebaseUser,
    firebaseLoading,
    firebaseError,
    userProfile,
    roles,
    permissions,
    
    // Combined
    logout,
    updateProfile,
    hasRole,
    hasPermission,
  };

  return (
    <FirebaseAuth0Context.Provider value={value}>
      {children}
    </FirebaseAuth0Context.Provider>
  );
}

export function useFirebaseAuth0() {
  const context = useContext(FirebaseAuth0Context);
  if (context === undefined) {
    throw new Error('useFirebaseAuth0 must be used within a FirebaseAuth0Provider');
  }
  return context;
}

// HOC for protected routes
export function withAuth(Component: React.ComponentType<any>, requiredRole?: string) {
  return function ProtectedComponent(props: any) {
    const { firebaseUser, firebaseLoading, hasRole } = useFirebaseAuth0();

    if (firebaseLoading) {
      return <div>Loading...</div>;
    }

    if (!firebaseUser) {
      return <div>Please log in to access this page.</div>;
    }

    if (requiredRole && !hasRole(requiredRole)) {
      return <div>You do not have permission to access this page.</div>;
    }

    return <Component {...props} />;
  };
}

// Hook for real-time data
export function useRealtimeData<T>(
  path: string,
  query?: any
): { data: T | null; loading: boolean; error: any } {
  const [data, setData] = useState<T | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<any>(null);
  const { firebaseUser } = useFirebaseAuth0();

  useEffect(() => {
    if (!firebaseUser) {
      setData(null);
      setLoading(false);
      return;
    }

    // Implementation would depend on the specific query
    // This is a placeholder for the real-time subscription logic
    setLoading(false);
  }, [firebaseUser, path, query]);

  return { data, loading, error };
}
