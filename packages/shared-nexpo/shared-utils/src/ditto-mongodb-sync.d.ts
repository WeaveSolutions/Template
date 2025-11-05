/**
 * Ditto MongoDB Sync Adapter
 *
 * This module provides synchronization capabilities between Ditto
 * and MongoDB databases.
 */
import { DittoInstance } from './ditto-service';
import { CloudSyncAdapter, SyncDirection, SyncStatus } from './ditto-cloud-sync';
/**
 * Configuration for MongoDB sync adapter
 */
export interface MongodbSyncConfig {
    connectionString: string;
    collections: string[];
    defaultDirection: SyncDirection;
}
/**
 * MongoDB sync adapter for Ditto
 * Implements the CloudSyncAdapter interface
 */
export declare class MongodbSyncAdapter implements CloudSyncAdapter {
    private ditto;
    private provider;
    private config;
    private isInitialized;
    private lastSyncTime;
    /**
     * Create a new MongoDB sync adapter
     */
    constructor(config: MongodbSyncConfig);
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
     * Sync data between Ditto and MongoDB
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
//# sourceMappingURL=ditto-mongodb-sync.d.ts.map
