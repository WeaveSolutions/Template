/**
 * Ditto MongoDB Sync Adapter
 * 
 * This module provides synchronization capabilities between Ditto
 * and MongoDB databases.
 */

import { DittoInstance } from './ditto-service';
import { MongodbProvider } from '../../shared-db/src/providers/mongodb-provider';
import { CloudSyncAdapter, SyncDirection, SyncStatus } from './ditto-cloud-sync';

/**
 * Configuration for MongoDB sync adapter
 */
export interface MongodbSyncConfig {
  // MongoDB connection info
  connectionString: string;
  // Collections to sync
  collections: string[];
  // Default sync direction
  defaultDirection: SyncDirection;
}

/**
 * MongoDB sync adapter for Ditto
 * Implements the CloudSyncAdapter interface
 */
export class MongodbSyncAdapter implements CloudSyncAdapter {
  private ditto: DittoInstance | null = null;
  private provider: MongodbProvider | null = null;
  private config: MongodbSyncConfig;
  private isInitialized = false;
  private lastSyncTime: Date | null = null;
  
  /**
   * Create a new MongoDB sync adapter
   */
  constructor(config: MongodbSyncConfig) {
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
    
    // Initialize the MongoDB provider
    try {
      this.provider = MongodbProvider.getInstance(this.config.connectionString);
      await this.provider.connect();
      this.isInitialized = true;
    } catch (error) {
      console.error('Failed to initialize MongoDB sync adapter:', error);
      throw error;
    }
  }
  
  /**
   * Get the provider type
   */
  getProviderType(): string {
    return 'mongodb';
  }
  
  /**
   * Sync data between Ditto and MongoDB
   * @param collection The collection to sync
   * @param direction The direction to sync
   */
  async syncCollection(collection: string, direction?: SyncDirection): Promise<void> {
    if (!this.isInitialized || !this.ditto || !this.provider) {
      throw new Error('MongoDB sync adapter not initialized');
    }
    
    // Use default direction if not specified
    const syncDirection = direction || this.config.defaultDirection;
    
    try {
      const dittoCollection = this.ditto.collection(collection);
      
      // For MongoDB, the collection name mapping is straightforward
      const mongoCollection = collection;
      
      if (syncDirection === SyncDirection.TO_CLOUD || syncDirection === SyncDirection.BIDIRECTIONAL) {
        // Sync from Ditto to MongoDB
        const dittoDocuments = await dittoCollection.find().exec();
        
        for (const doc of dittoDocuments) {
          // Convert Ditto document to MongoDB document
          const mongoDoc = {
            _id: doc.id,
            ...doc.value
          };
          
          // Upsert the document to MongoDB
          await this.provider.executeRawQuery(
            `db.${mongoCollection}.updateOne(
              { _id: "${doc.id}" },
              { $set: ${JSON.stringify(mongoDoc)} },
              { upsert: true }
            )`
          );
        }
      }
      
      if (syncDirection === SyncDirection.FROM_CLOUD || syncDirection === SyncDirection.BIDIRECTIONAL) {
        // Sync from MongoDB to Ditto
        const result = await this.provider.executeRawQuery(
          `db.${mongoCollection}.find({})`
        );
        
        for (const doc of result) {
          // Prepare the document for Ditto
          const { _id, ...data } = doc;
          
          // Upsert to Ditto
          await dittoCollection.upsert({ _id, ...data });
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
      throw new Error('MongoDB sync adapter not initialized');
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
