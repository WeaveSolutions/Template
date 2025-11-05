import { createClient } from '@supabase/supabase-js';
import { Platform } from 'react-native';

// Supabase configuration
const supabaseUrl = process.env.NEXT_PUBLIC_SUPABASE_URL || process.env.EXPO_PUBLIC_SUPABASE_URL || '';
const supabaseAnonKey = process.env.NEXT_PUBLIC_SUPABASE_ANON_KEY || process.env.EXPO_PUBLIC_SUPABASE_ANON_KEY || '';

// Validate required environment variables
if (!supabaseUrl || !supabaseAnonKey) {
  console.warn(
    'Supabase environment variables are not set. Please check your .env file and ensure NEXT_PUBLIC_SUPABASE_URL and NEXT_PUBLIC_SUPABASE_ANON_KEY are configured.'
  );
}

// Create Supabase client with platform-specific storage
// Use dummy values if env vars are not set to prevent runtime errors
export const supabase = createClient(
  supabaseUrl || 'https://placeholder.supabase.co',
  supabaseAnonKey || 'placeholder-anon-key',
  {
    auth: {
      storage: Platform.OS === 'web' ? undefined : {
        getItem: async (key: string) => {
          // For React Native, you would use AsyncStorage
          // This is a placeholder - you need to install and import AsyncStorage
          return null;
        },
        setItem: async (key: string, value: string) => {
          // For React Native, you would use AsyncStorage
          return;
        },
        removeItem: async (key: string) => {
          // For React Native, you would use AsyncStorage
          return;
        },
      },
      autoRefreshToken: true,
      persistSession: true,
      detectSessionInUrl: Platform.OS === 'web',
    },
  }
);

// Database types (you can generate these from Supabase)
export type Database = {
  public: {
    Tables: {
      profiles: {
        Row: {
          id: string;
          updated_at: string | null;
          username: string | null;
          full_name: string | null;
          avatar_url: string | null;
          email: string | null;
        };
        Insert: {
          id: string;
          updated_at?: string | null;
          username?: string | null;
          full_name?: string | null;
          avatar_url?: string | null;
          email?: string | null;
        };
        Update: {
          id?: string;
          updated_at?: string | null;
          username?: string | null;
          full_name?: string | null;
          avatar_url?: string | null;
          email?: string | null;
        };
      };
    };
  };
};
