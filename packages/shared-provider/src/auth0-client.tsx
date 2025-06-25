import { Platform } from 'react-native';

// Mock Auth0 client for development
export const initAuth0Client = (config: any) => {
  if (Platform.OS === 'web') {
    // For web, we'll use a simple mock implementation
    return {
      loginWithRedirect: async () => {
        console.log('Mock login redirect');
        // In a real implementation, this would redirect to Auth0
      },
      logout: async () => {
        console.log('Mock logout');
        // In a real implementation, this would clear Auth0 session
      },
      getUser: async () => {
        // Mock user for development
        return {
          sub: 'mock-user-id',
          name: 'Mock User',
          email: 'mock@example.com',
          picture: 'https://via.placeholder.com/150'
        };
      },
      isAuthenticated: async () => {
        // For development, return false to show login screen
        return false;
      }
    };
  } else {
    // For mobile, return similar mock implementation
    return {
      authorize: async () => {
        console.log('Mock mobile authorize');
      },
      clearSession: async () => {
        console.log('Mock mobile logout');
      },
      getCredentials: async () => {
        return null; // Mock no credentials for development
      }
    };
  }
};
