export const greet = (name: string) => `Hello, ${name}!`;
export * from './supabase';
export type { Database } from './supabase';

// Firebase + Auth0 exports
export * from './firebase-auth0';
export * from './auth0-config';
export * from './mindsdb';

// Utility exports
export * from './date';

// API client exports
export * from './api/client';

// Ditto exports
export * from './ditto-config';
export * from './ditto-service';
export * from './ditto-sync-factory';
