/**
 * Ditto IBM Cloud DB2 Sync Adapter
 *
 * This module provides synchronization capabilities between Ditto
 * and IBM Cloud DB2 databases.
 */
import { DittoInstance } from './ditto-service';
import { CloudSyncAdapter, SyncDirection, SyncStatus } from './ditto-cloud-sync';
/**
 * Configuration for IBM Cloud DB2 sync adapter
 */
export interface IbmCloudSyncConfig {
    connectionString: string;
    collections: string[];
    defaultDirection: SyncDirection;
}
/**
 * IBM Cloud DB2 sync adapter for Ditto
 * Implements the CloudSyncAdapter interface
 */
export declare class IbmCloudSyncAdapter implements CloudSyncAdapter {
    private ditto;
    private provider;
    private config;
    private isInitialized;
    private lastSyncTime;
    /**
     * Create a new IBM Cloud DB2 sync adapter
     */
    constructor(config: IbmCloudSyncConfig);
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
     * Sync data between Ditto and IBM Cloud DB2
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
//# sourceMappingURL=ditto-ibmcloud-sync.d.ts.map
