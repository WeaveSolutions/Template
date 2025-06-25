/**
 * Ditto AWS S3 Sync Adapter
 *
 * This module provides synchronization capabilities between Ditto
 * and AWS S3 object storage.
 */
import { DittoInstance } from './ditto-service';
import { CloudSyncAdapter, SyncDirection, SyncStatus } from './ditto-cloud-sync';
/**
 * Configuration for AWS S3 sync adapter
 */
export interface AwsS3SyncConfig {
    bucketName: string;
    region: string;
    accessKeyId?: string;
    secretAccessKey?: string;
    endpoint?: string;
    collections: string[];
    defaultDirection: SyncDirection;
    prefix?: string;
}
/**
 * AWS S3 sync adapter for Ditto
 * Implements the CloudSyncAdapter interface
 */
export declare class AwsS3SyncAdapter implements CloudSyncAdapter {
    private ditto;
    private provider;
    private config;
    private isInitialized;
    private lastSyncTime;
    /**
     * Create a new AWS S3 sync adapter
     */
    constructor(config: AwsS3SyncConfig);
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
     * Get the S3 object key for a document
     * @param collection The collection name
     * @param documentId The document ID
     */
    private getObjectKey;
    /**
     * Extract collection and document ID from S3 object key
     * @param objectKey The S3 object key
     */
    private parseObjectKey;
    /**
     * Sync data between Ditto and AWS S3
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
//# sourceMappingURL=ditto-aws-s3-sync.d.ts.map
