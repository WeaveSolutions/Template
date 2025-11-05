/**
 * Ditto AWS S3 Sync Adapter
 * 
 * This module provides synchronization capabilities between Ditto
 * and AWS S3 object storage.
 */

import { DittoInstance } from './ditto-service';
import { AwsS3Provider } from '../../shared-db/src/providers/aws-s3-provider';
import { CloudSyncAdapter, SyncDirection, SyncStatus } from './ditto-cloud-sync';

/**
 * Configuration for AWS S3 sync adapter
 */
export interface AwsS3SyncConfig {
  // AWS S3 connection info
  bucketName: string;
  // AWS credentials
  region: string;
  accessKeyId?: string;
  secretAccessKey?: string;
  // Optional endpoint for using S3-compatible storage services
  endpoint?: string;
  // Collections to sync
  collections: string[];
  // Default sync direction
  defaultDirection: SyncDirection;
  // Optional prefix for S3 objects
  prefix?: string;
}

/**
 * AWS S3 sync adapter for Ditto
 * Implements the CloudSyncAdapter interface
 */
export class AwsS3SyncAdapter implements CloudSyncAdapter {
  private ditto: DittoInstance | null = null;
  private provider: AwsS3Provider | null = null;
  private config: AwsS3SyncConfig;
  private isInitialized = false;
  private lastSyncTime: Date | null = null;
  
  /**
   * Create a new AWS S3 sync adapter
   */
  constructor(config: AwsS3SyncConfig) {
    this.config = config;
  }
  
  /**
   * Initialize the adapter
   * @param ditto The Ditto instance to sync with
   */
  async initialize(ditto: DittoInstance): Promise<void> {
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
    } catch (error) {
      console.error('Failed to initialize AWS S3 sync adapter:', error);
      throw error;
    }
  }
  
  /**
   * Get the provider type
   */
  getProviderType(): string {
    return 'aws-s3';
  }
  
  /**
   * Get the S3 object key for a document
   * @param collection The collection name
   * @param documentId The document ID
   */
  private getObjectKey(collection: string, documentId: string): string {
    const prefix = this.config.prefix ? `${this.config.prefix}/` : '';
    return `${prefix}${collection}/${documentId}.json`;
  }
  
  /**
   * Extract collection and document ID from S3 object key
   * @param objectKey The S3 object key
   */
  private parseObjectKey(objectKey: string): { collection: string; documentId: string } | null {
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
  async syncCollection(collection: string, direction?: SyncDirection): Promise<void> {
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
    } catch (error) {
      console.error(`Failed to sync collection ${collection}:`, error);
      throw error;
    }
  }
  
  /**
   * Sync all configured collections
   */
  async syncAll(): Promise<void> {
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
  getSyncStatus(): SyncStatus {
    return {
      provider: this.getProviderType(),
      lastSync: this.lastSyncTime,
      collections: this.config.collections,
    };
  }
  
  /**
   * Dispose the adapter
   */
  async dispose(): Promise<void> {
    if (this.provider) {
      await this.provider.disconnect();
    }
    
    this.ditto = null;
    this.provider = null;
    this.isInitialized = false;
  }
}
