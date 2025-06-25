/**
 * Ditto Provider Component
 * 
 * This provider wraps the application and initializes Ditto,
 * providing local-first database capabilities with multi-cloud sync.
 */

import React, { createContext, useContext, useEffect, useState } from 'react';
import { getDittoInstance, DittoConfig, DittoInstance, useDitto } from '../../../shared-utils/src/ditto-service';
import { createCloudSyncService, DittoCloudSync, CloudSyncConfig } from '../../../shared-utils/src/ditto-cloud-sync';

// Import platform-specific implementations
// These are dynamically loaded based on platform
import '../../../shared-utils/src/ditto-web';
// Note: ditto-native.ts is imported by the Expo app

// Context for Ditto state
interface DittoContextType {
  ditto: DittoInstance | null;
  cloudSync: DittoCloudSync | null;
  isInitialized: boolean;
  isOnline: boolean;
  isSyncing: boolean;
  lastSyncTime: Date | null;
  error: Error | null;
  syncNow: () => Promise<void>;
}

const defaultContextValue: DittoContextType = {
  ditto: null,
  cloudSync: null,
  isInitialized: false,
  isOnline: false,
  isSyncing: false,
  lastSyncTime: null,
  error: null,
  syncNow: async () => {},
};

// Create the context
const DittoContext = createContext<DittoContextType>(defaultContextValue);

// Provider props
interface DittoProviderProps {
  children: React.ReactNode;
  dittoConfig: DittoConfig;
  cloudSyncConfig?: CloudSyncConfig;
}

/**
 * Ditto Provider Component
 * Initializes and provides Ditto context throughout the app
 */
export function DittoProvider({ children, dittoConfig, cloudSyncConfig }: DittoProviderProps) {
  // Use the Ditto hook to initialize
  const { instance, isInitialized, isOnline, error } = useDitto(dittoConfig);
  
  // Cloud sync state
  const [cloudSync, setCloudSync] = useState<DittoCloudSync | null>(null);
  const [isSyncing, setIsSyncing] = useState(false);
  const [lastSyncTime, setLastSyncTime] = useState<Date | null>(null);
  
  // Initialize cloud sync if enabled
  useEffect(() => {
    if (!isInitialized || !instance || !cloudSyncConfig?.enabled) {
      return;
    }
    
    const syncService = createCloudSyncService(cloudSyncConfig);
    
    // Initialize sync service
    syncService.initialize()
      .then(() => {
        setCloudSync(syncService);
        
        // Get initial sync status
        const status = syncService.getSyncStatus();
        if (status.length > 0) {
          const lastSync = status.reduce((latest, current) => {
            if (!latest || (current.lastSync > latest)) {
              return current.lastSync;
            }
            return latest;
          }, null as Date | null);
          
          setLastSyncTime(lastSync);
        }
      })
      .catch(err => {
        console.error('Failed to initialize cloud sync:', err);
      });
      
    // Clean up sync service
    return () => {
      if (syncService) {
        syncService.dispose().catch(console.error);
      }
    };
  }, [isInitialized, instance, cloudSyncConfig]);
  
  // Handle sync now function
  const syncNow = async () => {
    if (!cloudSync) {
      return;
    }
    
    setIsSyncing(true);
    
    try {
      await cloudSync.syncAll();
      
      // Update last sync time
      const status = cloudSync.getSyncStatus();
      if (status.length > 0) {
        const lastSync = status.reduce((latest, current) => {
          if (!latest || (current.lastSync > latest)) {
            return current.lastSync;
          }
          return latest;
        }, null as Date | null);
        
        setLastSyncTime(lastSync);
      }
    } catch (err) {
      console.error('Failed to sync:', err);
    } finally {
      setIsSyncing(false);
    }
  };
  
  // Context value
  const contextValue: DittoContextType = {
    ditto: instance,
    cloudSync,
    isInitialized,
    isOnline,
    isSyncing,
    lastSyncTime,
    error,
    syncNow,
  };
  
  return (
    <DittoContext.Provider value={contextValue}>
      {children}
    </DittoContext.Provider>
  );
}

/**
 * Hook to use Ditto in components
 */
export function useDittoContext() {
  const context = useContext(DittoContext);
  
  if (context === undefined) {
    throw new Error('useDittoContext must be used within a DittoProvider');
  }
  
  return context;
}

export default DittoProvider;
