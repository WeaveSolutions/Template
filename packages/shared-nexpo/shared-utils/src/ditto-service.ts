/**
 * Ditto Service - Cross-platform local-first database service
 * 
 * This service provides a consistent interface for using Ditto in both
 * web (Next.js) and mobile (React Native/Expo) environments.
 */

import { useEffect, useState } from 'react';

// Types for our Ditto integration
export interface DittoConfig {
  appID: string;
  token: string;
  enableCloudSync: boolean;
  syncWithPeers: boolean;
  syncWithCloud: boolean;
  cloudSyncURL?: string;
  collectionNames: string[];
}

// Interface that will be implemented differently for web and native
export interface DittoInstance {
  store: any; // The actual Ditto store instance
  isInitialized: boolean;
  isOnline: boolean;
  initialize: (config: DittoConfig) => Promise<void>;
  sync: () => Promise<void>;
  disconnect: () => Promise<void>;
  collection: (name: string) => any;
  query: (collectionName: string, query: any) => Promise<any[]>;
  insert: (collectionName: string, document: any) => Promise<string>;
  update: (collectionName: string, id: string, updateObject: any) => Promise<boolean>;
  remove: (collectionName: string, id: string) => Promise<boolean>;
  subscribe: (collectionName: string, callback: (docs: any[]) => void) => () => void;
  exportData: () => Promise<string>;
  importData: (data: string) => Promise<void>;
}

// Global store for the Ditto instance
let dittoInstance: DittoInstance | null = null;

// Platform-specific implementation will be injected at runtime
let DittoPlatform: { createInstance: () => DittoInstance } | null = null;

/**
 * Register the platform-specific Ditto implementation
 */
export function registerDittoPlatform(platform: { createInstance: () => DittoInstance }): void {
  DittoPlatform = platform;
}

/**
 * Get the Ditto instance (singleton)
 */
export function getDittoInstance(): DittoInstance {
  if (!dittoInstance) {
    if (!DittoPlatform) {
      throw new Error('Ditto platform implementation not registered. Call registerDittoPlatform first.');
    }
    dittoInstance = DittoPlatform.createInstance();
  }
  return dittoInstance;
}

/**
 * Hook for using Ditto in React components
 */
export function useDitto(config: DittoConfig) {
  const [instance, setInstance] = useState<DittoInstance | null>(null);
  const [isInitialized, setIsInitialized] = useState(false);
  const [isOnline, setIsOnline] = useState(false);
  const [error, setError] = useState<Error | null>(null);

  useEffect(() => {
    let isMounted = true;
    
    async function initializeDitto() {
      try {
        // Get or create the Ditto instance
        const ditto = getDittoInstance();
        
        // Initialize Ditto with the provided config
        await ditto.initialize(config);
        
        if (isMounted) {
          setInstance(ditto);
          setIsInitialized(ditto.isInitialized);
          setIsOnline(ditto.isOnline);
        }
      } catch (err) {
        console.error('Failed to initialize Ditto:', err);
        if (isMounted) {
          setError(err instanceof Error ? err : new Error(String(err)));
        }
      }
    }

    initializeDitto();

    // Cleanup function
    return () => {
      isMounted = false;
    };
  }, [config]);

  return { instance, isInitialized, isOnline, error };
}

/**
 * Hook for using a Ditto collection in React components
 */
export function useDittoCollection(collectionName: string) {
  const ditto = getDittoInstance();
  const [documents, setDocuments] = useState<any[]>([]);
  const [isLoading, setIsLoading] = useState(true);
  const [error, setError] = useState<Error | null>(null);

  useEffect(() => {
    if (!ditto.isInitialized) {
      return;
    }

    setIsLoading(true);

    // Set up subscription to the collection
    const unsubscribe = ditto.subscribe(collectionName, (docs) => {
      setDocuments(docs);
      setIsLoading(false);
    });

    // Initial query
    ditto.query(collectionName, {})
      .then((docs) => {
        setDocuments(docs);
        setIsLoading(false);
      })
      .catch((err) => {
        console.error(`Error querying ${collectionName}:`, err);
        setError(err instanceof Error ? err : new Error(String(err)));
        setIsLoading(false);
      });

    // Clean up subscription
    return () => {
      unsubscribe();
    };
  }, [collectionName, ditto.isInitialized]);

  // Functions to interact with the collection
  const insert = async (document: any) => {
    try {
      return await ditto.insert(collectionName, document);
    } catch (err) {
      console.error(`Error inserting into ${collectionName}:`, err);
      throw err;
    }
  };

  const update = async (id: string, updateObject: any) => {
    try {
      return await ditto.update(collectionName, id, updateObject);
    } catch (err) {
      console.error(`Error updating in ${collectionName}:`, err);
      throw err;
    }
  };

  const remove = async (id: string) => {
    try {
      return await ditto.remove(collectionName, id);
    } catch (err) {
      console.error(`Error removing from ${collectionName}:`, err);
      throw err;
    }
  };

  return {
    documents,
    isLoading,
    error,
    insert,
    update,
    remove,
  };
}

/**
 * Hook for syncing Ditto with cloud databases
 */
export function useDittoCloudSync(options: {
  syncInterval?: number; // in milliseconds
  onSyncSuccess?: () => void;
  onSyncError?: (error: Error) => void;
}) {
  const ditto = getDittoInstance();
  const [lastSyncTime, setLastSyncTime] = useState<Date | null>(null);
  const [isSyncing, setIsSyncing] = useState(false);
  const [syncError, setSyncError] = useState<Error | null>(null);

  const syncWithCloud = async () => {
    if (isSyncing || !ditto.isInitialized) return;

    setIsSyncing(true);
    setSyncError(null);

    try {
      await ditto.sync();
      setLastSyncTime(new Date());
      options.onSyncSuccess?.();
    } catch (err) {
      const error = err instanceof Error ? err : new Error(String(err));
      setSyncError(error);
      options.onSyncError?.(error);
    } finally {
      setIsSyncing(false);
    }
  };

  // Set up automatic sync
  useEffect(() => {
    if (!ditto.isInitialized) return;

    // Perform initial sync
    syncWithCloud();

    // Set up interval for periodic syncing
    const interval = options.syncInterval || 60000; // Default to 1 minute
    const timer = setInterval(syncWithCloud, interval);

    return () => {
      clearInterval(timer);
    };
  }, [ditto.isInitialized, options.syncInterval]);

  return {
    syncWithCloud,
    lastSyncTime,
    isSyncing,
    syncError,
  };
}
