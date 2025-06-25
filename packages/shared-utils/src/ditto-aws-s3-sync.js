/**
 * Ditto AWS S3 Sync Adapter
 *
 * This module provides synchronization capabilities between Ditto
 * and AWS S3 object storage.
 */
import { AwsS3Provider } from '../../shared-db/src/providers/aws-s3-provider';
import { SyncDirection } from './ditto-cloud-sync';
/**
 * AWS S3 sync adapter for Ditto
 * Implements the CloudSyncAdapter interface
 */
export class AwsS3SyncAdapter {
    ditto = null;
    provider = null;
    config;
    isInitialized = false;
    lastSyncTime = null;
    /**
     * Create a new AWS S3 sync adapter
     */
    constructor(config) {
        this.config = config;
    }
    /**
     * Initialize the adapter
     * @param ditto The Ditto instance to sync with
     */
    async initialize(ditto) {
        if (this.isInitialized) {
            return;
        }
        // Store the Ditto instance
        this.ditto = ditto;
        // Initialize the AWS S3 provider
        try {
            this.provider = AwsS3Provider.getInstance({
                bucketName: this.config.bucketName,
                region: this.config.region,
                accessKeyId: this.config.accessKeyId,
                secretAccessKey: this.config.secretAccessKey,
                endpoint: this.config.endpoint
            });
            await this.provider.connect();
            this.isInitialized = true;
        }
        catch (error) {
            console.error('Failed to initialize AWS S3 sync adapter:', error);
            throw error;
        }
    }
    /**
     * Get the provider type
     */
    getProviderType() {
        return 'aws-s3';
    }
    /**
     * Get the S3 object key for a document
     * @param collection The collection name
     * @param documentId The document ID
     */
    getObjectKey(collection, documentId) {
        const prefix = this.config.prefix ? `${this.config.prefix}/` : '';
        return `${prefix}${collection}/${documentId}.json`;
    }
    /**
     * Extract collection and document ID from S3 object key
     * @param objectKey The S3 object key
     */
    parseObjectKey(objectKey) {
        const prefix = this.config.prefix ? `${this.config.prefix}/` : '';
        const regex = new RegExp(`^${prefix}([^/]+)/([^/]+)\\.json$`);
        const match = objectKey.match(regex);
        if (!match) {
            return null;
        }
        return {
            collection: match[1],
            documentId: match[2]
        };
    }
    /**
     * Sync data between Ditto and AWS S3
     * @param collection The collection to sync
     * @param direction The direction to sync
     */
    async syncCollection(collection, direction) {
        if (!this.isInitialized || !this.ditto || !this.provider) {
            throw new Error('AWS S3 sync adapter not initialized');
        }
        // Use default direction if not specified
        const syncDirection = direction || this.config.defaultDirection;
        try {
            const dittoCollection = this.ditto.collection(collection);
            if (syncDirection === SyncDirection.TO_CLOUD || syncDirection === SyncDirection.BIDIRECTIONAL) {
                // Sync from Ditto to AWS S3
                const dittoDocuments = await dittoCollection.find().exec();
                for (const doc of dittoDocuments) {
                    const objectKey = this.getObjectKey(collection, doc.id);
                    const objectContent = JSON.stringify(doc.value);
                    // Upload the document to S3
                    await this.provider.putObject(objectKey, objectContent, {
                        contentType: 'application/json',
                        metadata: {
                            'ditto-collection': collection,
                            'ditto-document-id': doc.id,
                            'ditto-sync-time': new Date().toISOString()
                        }
                    });
                }
            }
            if (syncDirection === SyncDirection.FROM_CLOUD || syncDirection === SyncDirection.BIDIRECTIONAL) {
                // Sync from AWS S3 to Ditto
                const prefix = this.config.prefix ? `${this.config.prefix}/` : '';
                const objects = await this.provider.listObjects(`${prefix}${collection}/`);
                for (const object of objects) {
                    const parsedKey = this.parseObjectKey(object.key);
                    if (parsedKey && parsedKey.collection === collection) {
                        // Get the object content from S3
                        const objectContent = await this.provider.getObject(object.key);
                        if (objectContent) {
                            // Parse the content
                            const data = JSON.parse(objectContent);
                            // Upsert to Ditto
                            await dittoCollection.upsert({ _id: parsedKey.documentId, ...data });
                        }
                    }
                }
            }
            // Update last sync time
            this.lastSyncTime = new Date();
        }
        catch (error) {
            console.error(`Failed to sync collection ${collection}:`, error);
            throw error;
        }
    }
    /**
     * Sync all configured collections
     */
    async syncAll() {
        if (!this.isInitialized) {
            throw new Error('AWS S3 sync adapter not initialized');
        }
        for (const collection of this.config.collections) {
            await this.syncCollection(collection);
        }
    }
    /**
     * Get the sync status
     */
    getSyncStatus() {
        return {
            provider: this.getProviderType(),
            lastSync: this.lastSyncTime,
            collections: this.config.collections,
        };
    }
    /**
     * Dispose the adapter
     */
    async dispose() {
        if (this.provider) {
            await this.provider.disconnect();
        }
        this.ditto = null;
        this.provider = null;
        this.isInitialized = false;
    }
}
//# sourceMappingURL=ditto-aws-s3-sync.js.map
