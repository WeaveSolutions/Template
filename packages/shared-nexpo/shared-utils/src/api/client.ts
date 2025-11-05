import { Platform } from 'react-native';
import { isTauri } from '../tauri/tauri-hooks';

// Type augmentation for environment variables
declare global {
  namespace NodeJS {
    interface ProcessEnv {
      NEXT_PUBLIC_API_URL?: string;
      EXPO_PUBLIC_API_URL?: string;
      TAURI_PUBLIC_API_URL?: string;
    }
  }
}

// Platform-aware API URL selection
function getApiBaseUrl(): string {
  // Check if we're in Tauri environment
  if (isTauri()) {
    return process.env.TAURI_PUBLIC_API_URL || 'http://localhost:8000';
  }
  
  // Check if we're in Expo/React Native environment
  if (Platform.OS === 'ios' || Platform.OS === 'android') {
    return process.env.EXPO_PUBLIC_API_URL || 'http://localhost:8000';
  }
  
  // Default to Next.js web environment
  return process.env.NEXT_PUBLIC_API_URL || 'http://localhost:8000';
}

const API_BASE_URL = getApiBaseUrl();

type RequestMethod = 'GET' | 'POST' | 'PUT' | 'DELETE' | 'PATCH';

interface RequestOptions {
  method?: RequestMethod;
  headers?: Record<string, string>;
  body?: any;
  credentials?: RequestCredentials;
}

// Token storage for non-React contexts
let authTokenProvider: (() => Promise<string | null>) | null = null;

export function setAuthTokenProvider(provider: () => Promise<string | null>) {
  authTokenProvider = provider;
}

class ApiClient {
  async request<T = any>(
    method: RequestMethod,
    endpoint: string,
    options: RequestOptions = {}
  ): Promise<T> {
    const url = `${API_BASE_URL}${endpoint}`;
  
    const headers: HeadersInit = {
      'Content-Type': 'application/json',
      ...options.headers,
    };

    // Add authorization header if token exists
    const token = await getAccessToken();
    if (token) {
      headers['Authorization'] = `Bearer ${token}`;
    }

    const config: RequestInit = {
      method,
      headers,
      credentials: options.credentials || (Platform.OS === 'web' ? 'include' : 'omit'),
    };

    if (options.body) {
      config.body = JSON.stringify(options.body);
    }

    try {
      const response = await fetch(url, config);
      
      if (!response.ok) {
        const error = await response.json().catch(() => ({}));
        throw new Error(error.message || 'Something went wrong');
      }

      // Handle empty responses
      const data = await response.json();
      return data as T;
    } catch (error) {
      console.error(`API ${method} error:`, error);
      throw error;
    }
  }

  async get<T>(endpoint: string): Promise<T> {
    return this.request<T>('GET', endpoint);
  }

  async post<T>(endpoint: string, data?: any): Promise<T> {
    return this.request<T>('POST', endpoint, data);
  }

  async put<T>(endpoint: string, data?: any): Promise<T> {
    return this.request<T>('PUT', endpoint, data);
  }

  async delete<T>(endpoint: string): Promise<T> {
    return this.request<T>('DELETE', endpoint);
  }

  async patch<T>(endpoint: string, data?: any): Promise<T> {
    return this.request<T>('PATCH', endpoint, data);
  }
}

async function getAccessToken(): Promise<string | null> {
  if (Platform.OS === 'web') {
    // For web, we rely on HTTP-only cookies
    return null;
  } else {
    // For mobile, use the token provider if set
    if (authTokenProvider) {
      try {
        return await authTokenProvider();
      } catch (error) {
        console.error('Failed to get access token:', error);
        return null;
      }
    }
    
    // Fallback: try to get token from mobile secure storage
    try {
      // Dynamic import with fallback for platforms where expo-secure-store isn't available
      const SecureStore = await import('expo-secure-store').catch(() => null);
      if (SecureStore) {
        return await SecureStore.getItemAsync('auth_token');
      }
      return null;
    } catch (error) {
      console.warn('SecureStore not available:', error);
      return null;
    }
  }
}

// Export a singleton instance
export const apiClient = new ApiClient();

// Re-export auth types from the correct path
export type { AuthContextType, AuthResult, User, Session } from '@nexpo/shared-provider';
