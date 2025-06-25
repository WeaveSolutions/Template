/**
 * Ditto IBM Cloud DB2 Sync Adapter
 * 
 * This module provides synchronization capabilities between Ditto
 * and IBM Cloud DB2 databases.
 */

import { DittoInstance } from './ditto-service';
import { IbmcloudProvider } from '../../shared-db/src/providers/ibmcloud-provider';
import { CloudSyncAdapter, SyncDirection, SyncStatus } from './ditto-cloud-sync';

/**
 * Configuration for IBM Cloud DB2 sync adapter
 */
export interface IbmCloudSyncConfig {
  // IBM Cloud DB2 connection info
  connectionString: string;
  // Collections to sync
  collections: string[];
  // Default sync direction
  defaultDirection: SyncDirection;
}

/**
 * IBM Cloud DB2 sync adapter for Ditto
 * Implements the CloudSyncAdapter interface
 */
export class IbmCloudSyncAdapter implements CloudSyncAdapter {
  private ditto: DittoInstance | null = null;
  private provider: IbmcloudProvider | null = null;
  private config: IbmCloudSyncConfig;
  private isInitialized = false;
  private lastSyncTime: Date | null = null;
  
  /**
   * Create a new IBM Cloud DB2 sync adapter
   */
  constructor(config: IbmCloudSyncConfig) {
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
    
    // Initialize the IBM Cloud DB2 provider
    try {
      this.provider = IbmcloudProvider.getInstance(this.config.connectionString);
      await this.provider.connect();
      this.isInitialized = true;
    } catch (error) {
      console.error('Failed to initialize IBM Cloud DB2 sync adapter:', error);
      throw error;
    }
  }
  
  /**
   * Get the provider type
   */
  getProviderType(): string {
    return 'ibmcloud';
  }
  
  /**
   * Sync data between Ditto and IBM Cloud DB2
   * @param collection The collection to sync
   * @param direction The direction to sync
   */
  async syncCollection(collection: string, direction?: SyncDirection): Promise<void> {
    if (!this.isInitialized || !this.ditto || !this.provider) {
      throw new Error('IBM Cloud DB2 sync adapter not initialized');
    }
    
    // Use default direction if not specified
    const syncDirection = direction || this.config.defaultDirection;
    
    try {
      const dittoCollection = this.ditto.collection(collection);
      
      // Determine the table name in IBM Cloud DB2
      // For simplicity, we use the same name but you might want a mapping
      const tableName = collection;
      
      if (syncDirection === SyncDirection.TO_CLOUD || syncDirection === SyncDirection.BIDIRECTIONAL) {
        // Sync from Ditto to IBM Cloud DB2
        const dittoDocuments = await dittoCollection.find().exec();
        
        for (const doc of dittoDocuments) {
          const docData = { id: doc.id, ...doc.value };
          
          // Upsert the document to IBM Cloud DB2
          // We're using raw queries here for simplicity
          // In a real implementation, you'd likely use Prisma
          await this.provider.executeRawQuery(
            `INSERT INTO "${tableName}" (id, data) 
             VALUES ($1, $2)
             ON CONFLICT (id) DO UPDATE 
             SET data = $2`,
            [doc.id, JSON.stringify(doc.value)]
          );
        }
      }
      
      if (syncDirection === SyncDirection.FROM_CLOUD || syncDirection === SyncDirection.BIDIRECTIONAL) {
        // Sync from IBM Cloud DB2 to Ditto
        const result = await this.provider.executeRawQuery(
          `SELECT id, data FROM "${tableName}"`,
          []
        );
        
        for (const row of result) {
          // Parse the data
          const id = row.id;
          const data = typeof row.data === 'string' ? JSON.parse(row.data) : row.data;
          
          // Upsert to Ditto
          await dittoCollection.upsert({ _id: id, ...data });
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
      throw new Error('IBM Cloud DB2 sync adapter not initialized');
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
