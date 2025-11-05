export const greet = (name: string) => `Hello, ${name}!`;
export * from './supabase';
export type { Database } from './supabase';

// Firebase + Auth0 exports
export * from './firebase-auth0';
export * from './auth0-config';
export * from './mindsdb';

// Utility exports
export * from './date';

// Tauri exports
export * from './tauri';

// API client exports
export * from './api/client';

// Ditto exports
export * from './ditto-config';
export * from './ditto-service';
// Note: ditto-sync-factory is server-side only
// Import directly from './ditto-sync-factory' in server-side code only

// Tauri exports
export * from './tauri';
