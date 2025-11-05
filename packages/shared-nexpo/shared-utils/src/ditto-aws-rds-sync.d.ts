/**
 * Ditto AWS RDS Sync Adapter
 *
 * This module provides synchronization capabilities between Ditto
 * and AWS RDS databases (PostgreSQL, MySQL, etc.).
 */
import { DittoInstance } from './ditto-service';
import { CloudSyncAdapter, SyncDirection, SyncStatus } from './ditto-cloud-sync';
/**
 * Engine types for AWS RDS
 */
export declare enum AwsRdsEngineType {
    POSTGRES = "postgres",
    MYSQL = "mysql",
    MARIADB = "mariadb",
    AURORA = "aurora",
    SQLSERVER = "sqlserver"
}
/**
 * Configuration for AWS RDS sync adapter
 */
export interface AwsRdsSyncConfig {
    connectionString: string;
    region: string;
    accessKeyId?: string;
    secretAccessKey?: string;
    engineType: AwsRdsEngineType;
    collections: string[];
    defaultDirection: SyncDirection;
    databaseName?: string;
    schema?: string;
}
/**
 * AWS RDS sync adapter for Ditto
 * Implements the CloudSyncAdapter interface
 */
export declare class AwsRdsSyncAdapter implements CloudSyncAdapter {
    private ditto;
    private provider;
    private config;
    private isInitialized;
    private lastSyncTime;
    /**
     * Create a new AWS RDS sync adapter
     */
    constructor(config: AwsRdsSyncConfig);
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
     * Sync data between Ditto and AWS RDS
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
//# sourceMappingURL=ditto-aws-rds-sync.d.ts.map
