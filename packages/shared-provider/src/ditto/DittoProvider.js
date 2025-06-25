/**
 * Ditto Provider Component
 *
 * This provider wraps the application and initializes Ditto,
 * providing local-first database capabilities with multi-cloud sync.
 */
import React, { createContext, useContext, useEffect, useState } from 'react';
import { useDitto } from '../../../shared-utils/src/ditto-service';
import { createCloudSyncService } from '../../../shared-utils/src/ditto-cloud-sync';
// Import platform-specific implementations
// These are dynamically loaded based on platform
import '../../../shared-utils/src/ditto-web';
const defaultContextValue = {
    ditto: null,
    cloudSync: null,
    isInitialized: false,
    isOnline: false,
    isSyncing: false,
    lastSyncTime: null,
    error: null,
    syncNow: async () => { },
};
// Create the context
const DittoContext = createContext(defaultContextValue);
/**
 * Ditto Provider Component
 * Initializes and provides Ditto context throughout the app
 */
export function DittoProvider({ children, dittoConfig, cloudSyncConfig }) {
    // Use the Ditto hook to initialize
    const { instance, isInitialized, isOnline, error } = useDitto(dittoConfig);
    // Cloud sync state
    const [cloudSync, setCloudSync] = useState(null);
    const [isSyncing, setIsSyncing] = useState(false);
    const [lastSyncTime, setLastSyncTime] = useState(null);
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
                }, null);
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
                }, null);
                setLastSyncTime(lastSync);
            }
        }
        catch (err) {
            console.error('Failed to sync:', err);
        }
        finally {
            setIsSyncing(false);
        }
    };
    // Context value
    const contextValue = {
        ditto: instance,
        cloudSync,
        isInitialized,
        isOnline,
        isSyncing,
        lastSyncTime,
        error,
        syncNow,
    };
    return (<DittoContext.Provider value={contextValue}>
      {children}
    </DittoContext.Provider>);
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
//# sourceMappingURL=DittoProvider.js.map
