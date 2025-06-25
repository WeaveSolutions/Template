/**
 * Ditto Service - Cross-platform local-first database service
 *
 * This service provides a consistent interface for using Ditto in both
 * web (Next.js) and mobile (React Native/Expo) environments.
 */
export interface DittoConfig {
    appID: string;
    token: string;
    enableCloudSync: boolean;
    syncWithPeers: boolean;
    syncWithCloud: boolean;
    cloudSyncURL?: string;
    collectionNames: string[];
}
export interface DittoInstance {
    store: any;
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
/**
 * Register the platform-specific Ditto implementation
 */
export declare function registerDittoPlatform(platform: {
    createInstance: () => DittoInstance;
}): void;
/**
 * Get the Ditto instance (singleton)
 */
export declare function getDittoInstance(): DittoInstance;
/**
 * Hook for using Ditto in React components
 */
export declare function useDitto(config: DittoConfig): {
    instance: DittoInstance | null;
    isInitialized: boolean;
    isOnline: boolean;
    error: Error | null;
};
/**
 * Hook for using a Ditto collection in React components
 */
export declare function useDittoCollection(collectionName: string): {
    documents: any[];
    isLoading: boolean;
    error: Error | null;
    insert: (document: any) => Promise<string>;
    update: (id: string, updateObject: any) => Promise<boolean>;
    remove: (id: string) => Promise<boolean>;
};
/**
 * Hook for syncing Ditto with cloud databases
 */
export declare function useDittoCloudSync(options: {
    syncInterval?: number;
    onSyncSuccess?: () => void;
    onSyncError?: (error: Error) => void;
}): {
    syncWithCloud: () => Promise<void>;
    lastSyncTime: Date | null;
    isSyncing: boolean;
    syncError: Error | null;
};
//# sourceMappingURL=ditto-service.d.ts.map
