/**
 * Ditto Cloud Sync service
 *
 * This module provides a service for synchronizing Ditto data with cloud databases.
 */
import { DittoInstance } from './ditto-service';
import { PostgresSyncConfig } from './ditto-postgres-sync';
import { MongodbSyncConfig } from './ditto-mongodb-sync';
import { CosmosdbSyncConfig } from './ditto-cosmosdb-sync';
import { SqlServerSyncConfig } from './ditto-sqlserver-sync';
import { IbmCloudSyncConfig } from './ditto-ibmcloud-sync';
/**
 * Sync direction enum
 */
export declare enum SyncDirection {
    TO_CLOUD = "to_cloud",
    FROM_CLOUD = "from_cloud",
    BIDIRECTIONAL = "bidirectional"
}
/**
 * Sync status interface
 */
export interface SyncStatus {
    provider: string;
    lastSync: Date | null;
    collections: string[];
}
/**
 * Cloud sync adapter interface
 */
export interface CloudSyncAdapter {
    initialize(ditto: DittoInstance): Promise<void>;
    getProviderType(): string;
    syncCollection(collection: string, direction?: SyncDirection): Promise<void>;
    syncAll(): Promise<void>;
    getSyncStatus(): SyncStatus;
    dispose(): Promise<void>;
}
/**
 * Cloud sync configuration
 */
export interface CloudSyncConfig {
    enabled: boolean;
    syncUrl?: string;
    syncInterval?: number;
    batchSize?: number;
    providers?: string[];
    postgresConfig?: PostgresSyncConfig;
    mongodbConfig?: MongodbSyncConfig;
    cosmosdbConfig?: CosmosdbSyncConfig;
    sqlserverConfig?: SqlServerSyncConfig;
    ibmcloudConfig?: IbmCloudSyncConfig;
}
/**
 * Ditto Cloud Sync service
 */
export interface DittoCloudSync {
    initialize(): Promise<void>;
    syncCollection(collection: string, direction?: SyncDirection): Promise<void>;
    syncAll(): Promise<void>;
    getSyncStatus(): SyncStatus[];
    dispose(): Promise<void>;
}
//# sourceMappingURL=ditto-cloud-sync.d.ts.map
