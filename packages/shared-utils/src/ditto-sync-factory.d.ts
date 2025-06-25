/**
 * Ditto Sync Factory
 *
 * This module provides a factory for creating and managing multiple cloud
 * synchronization adapters, enabling true multi-cloud synchronization.
 * Each adapter checks for feature flags before initialization.
 */
import { DittoInstance } from './ditto-service';
import { CloudSyncAdapter, SyncDirection, SyncStatus } from './ditto-cloud-sync';
import { PostgresSyncConfig } from './ditto-postgres-sync';
import { MongodbSyncConfig } from './ditto-mongodb-sync';
import { CosmosdbSyncConfig } from './ditto-cosmosdb-sync';
import { SqlServerSyncConfig } from './ditto-sqlserver-sync';
import { IbmCloudSyncConfig } from './ditto-ibmcloud-sync';
import { SupabaseSyncConfig } from './ditto-supabase-sync';
import { FirebaseSyncConfig } from './ditto-firebase-sync';
import { AwsRdsSyncConfig } from './ditto-aws-rds-sync';
import { AwsS3SyncConfig } from './ditto-aws-s3-sync';
/**
 * Multi-cloud sync configuration
 */
export interface MultiCloudSyncConfig {
    postgres?: PostgresSyncConfig;
    mongodb?: MongodbSyncConfig;
    cosmosdb?: CosmosdbSyncConfig;
    sqlserver?: SqlServerSyncConfig;
    ibmcloud?: IbmCloudSyncConfig;
    supabase?: SupabaseSyncConfig;
    firebase?: FirebaseSyncConfig;
    awsRds?: AwsRdsSyncConfig;
    awsS3?: AwsS3SyncConfig;
}
/**
 * Factory for creating and managing multiple cloud sync adapters
 */
export declare class DittoSyncFactory {
    private config;
    private adapters;
    private ditto;
    private isInitialized;
    /**
     * Create a new sync factory
     * @param config Multi-cloud sync configuration
     */
    constructor(config: MultiCloudSyncConfig);
    /**
     * Initialize all configured adapters
     * @param ditto The Ditto instance to sync with
     */
    initialize(ditto: DittoInstance): Promise<void>;
    /**
     * Get all available adapters
     */
    getAdapters(): CloudSyncAdapter[];
    /**
     * Get a specific adapter by provider type
     * @param providerType The provider type
     */
    getAdapter(providerType: string): CloudSyncAdapter | undefined;
    /**
     * Sync a collection with all configured providers
     * @param collection The collection to sync
     * @param direction The sync direction
     */
    syncCollection(collection: string, direction?: SyncDirection): Promise<void>;
    /**
     * Sync all collections with all configured providers
     */
    syncAll(): Promise<void>;
    /**
     * Get sync status for all adapters
     */
    getSyncStatus(): SyncStatus[];
    /**
     * Dispose all adapters
     */
    dispose(): Promise<void>;
}
//# sourceMappingURL=ditto-sync-factory.d.ts.map
