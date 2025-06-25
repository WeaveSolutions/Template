/**
 * Ditto CosmosDB Sync Adapter
 *
 * This module provides synchronization capabilities between Ditto
 * and Azure Cosmos DB databases.
 */
import { DittoInstance } from './ditto-service';
import { CloudSyncAdapter, SyncDirection, SyncStatus } from './ditto-cloud-sync';
/**
 * Configuration for CosmosDB sync adapter
 */
export interface CosmosdbSyncConfig {
    connectionString: string;
    collections: string[];
    defaultDirection: SyncDirection;
    databaseName: string;
}
/**
 * CosmosDB sync adapter for Ditto
 * Implements the CloudSyncAdapter interface
 */
export declare class CosmosdbSyncAdapter implements CloudSyncAdapter {
    private ditto;
    private provider;
    private config;
    private isInitialized;
    private lastSyncTime;
    /**
     * Create a new CosmosDB sync adapter
     */
    constructor(config: CosmosdbSyncConfig);
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
     * Sync data between Ditto and CosmosDB
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
//# sourceMappingURL=ditto-cosmosdb-sync.d.ts.map
