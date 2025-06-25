/**
 * Ditto PostgreSQL Sync Adapter
 *
 * This module provides synchronization capabilities between Ditto
 * and PostgreSQL databases.
 */
import { DittoInstance } from './ditto-service';
import { CloudSyncAdapter, SyncDirection, SyncStatus } from './ditto-cloud-sync';
/**
 * Configuration for PostgreSQL sync adapter
 */
export interface PostgresSyncConfig {
    connectionString: string;
    collections: string[];
    defaultDirection: SyncDirection;
}
/**
 * PostgreSQL sync adapter for Ditto
 * Implements the CloudSyncAdapter interface
 */
export declare class PostgresSyncAdapter implements CloudSyncAdapter {
    private ditto;
    private provider;
    private config;
    private isInitialized;
    private lastSyncTime;
    /**
     * Create a new PostgreSQL sync adapter
     */
    constructor(config: PostgresSyncConfig);
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
     * Sync data between Ditto and PostgreSQL
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
//# sourceMappingURL=ditto-postgres-sync.d.ts.map
