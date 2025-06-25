/**
 * Ditto SQL Server Sync Adapter
 *
 * This module provides synchronization capabilities between Ditto
 * and Microsoft SQL Server databases.
 */
import { DittoInstance } from './ditto-service';
import { CloudSyncAdapter, SyncDirection, SyncStatus } from './ditto-cloud-sync';
/**
 * Configuration for SQL Server sync adapter
 */
export interface SqlServerSyncConfig {
    connectionString: string;
    collections: string[];
    defaultDirection: SyncDirection;
    schema: string;
}
/**
 * SQL Server sync adapter for Ditto
 * Implements the CloudSyncAdapter interface
 */
export declare class SqlServerSyncAdapter implements CloudSyncAdapter {
    private ditto;
    private provider;
    private config;
    private isInitialized;
    private lastSyncTime;
    /**
     * Create a new SQL Server sync adapter
     */
    constructor(config: SqlServerSyncConfig);
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
     * Sync data between Ditto and SQL Server
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
//# sourceMappingURL=ditto-sqlserver-sync.d.ts.map
