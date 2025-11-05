/**
 * Ditto Firebase Sync Adapter
 *
 * This module provides synchronization capabilities between Ditto
 * and Firebase Firestore databases.
 */
import { DittoInstance } from './ditto-service';
import { CloudSyncAdapter, SyncDirection, SyncStatus } from './ditto-cloud-sync';
/**
 * Configuration for Firebase sync adapter
 */
export interface FirebaseSyncConfig {
    projectId: string;
    apiKey: string;
    authDomain?: string;
    collections: string[];
    defaultDirection: SyncDirection;
}
/**
 * Firebase sync adapter for Ditto
 * Implements the CloudSyncAdapter interface
 */
export declare class FirebaseSyncAdapter implements CloudSyncAdapter {
    private ditto;
    private provider;
    private config;
    private isInitialized;
    private lastSyncTime;
    /**
     * Create a new Firebase sync adapter
     */
    constructor(config: FirebaseSyncConfig);
    /**
     * Initialize the adapter
     * @param ditto The Ditto instance to sync with
     */
    initialize(ditto: DittoInstance): Promise<void>;
    /**
     * Get the provider type
     */
    getProviderType(): string;
    /**
     * Sync data between Ditto and Firebase
     * @param collection The collection to sync
     * @param direction The direction to sync
     */
    syncCollection(collection: string, direction?: SyncDirection): Promise<void>;
    /**
     * Sync all configured collections
     */
    syncAll(): Promise<void>;
    /**
     * Get the sync status
     */
    getSyncStatus(): SyncStatus;
    /**
     * Dispose the adapter
     */
    dispose(): Promise<void>;
}
//# sourceMappingURL=ditto-firebase-sync.d.ts.map
