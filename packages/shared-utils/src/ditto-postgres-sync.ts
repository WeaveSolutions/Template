/**
 * Ditto PostgreSQL Sync Adapter
 * 
 * This module provides synchronization capabilities between Ditto
 * and PostgreSQL databases.
 */

import { DittoInstance } from './ditto-service';
import { PostgresProvider } from '../../shared-db/src/providers/postgres-provider';
import { CloudSyncAdapter, SyncDirection, SyncStatus } from './ditto-cloud-sync';

/**
 * Configuration for PostgreSQL sync adapter
 */
export interface PostgresSyncConfig {
  // PostgreSQL connection info
  connectionString: string;
  // Collections to sync
  collections: string[];
  // Default sync direction
  defaultDirection: SyncDirection;
}

/**
 * PostgreSQL sync adapter for Ditto
 * Implements the CloudSyncAdapter interface
 */
export class PostgresSyncAdapter implements CloudSyncAdapter {
  private ditto: DittoInstance | null = null;
  private provider: PostgresProvider | null = null;
  private config: PostgresSyncConfig;
  private isInitialized = false;
  private lastSyncTime: Date | null = null;
  
  /**
   * Create a new PostgreSQL sync adapter
   */
  constructor(config: PostgresSyncConfig) {
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
    
    // Initialize the PostgreSQL provider
    try {
      this.provider = PostgresProvider.getInstance(this.config.connectionString);
      // Connect by getting the client which automatically connects
      await this.provider.getClient();
      this.isInitialized = true;
    } catch (error) {
      console.error('Failed to initialize PostgreSQL sync adapter:', error);
      throw error;
    }
  }
  
  /**
   * Get the provider type
   */
  getProviderType(): string {
    return 'postgres';
  }
  
  /**
   * Sync data between Ditto and PostgreSQL
   * @param collection The collection to sync
   * @param direction The direction to sync
   */
  async syncCollection(collection: string, direction?: SyncDirection): Promise<void> {
    if (!this.isInitialized || !this.ditto || !this.provider) {
      throw new Error('PostgreSQL sync adapter not initialized');
    }
    
    // Use default direction if not specified
    const syncDirection = direction || this.config.defaultDirection;
    
    try {
      const dittoCollection = this.ditto.collection(collection);
      
      // Determine the table name in PostgreSQL
      // For simplicity, we use the same name but you might want a mapping
      const tableName = collection;
      
      if (syncDirection === SyncDirection.TO_CLOUD || syncDirection === SyncDirection.BIDIRECTIONAL) {
        // Sync from Ditto to PostgreSQL
        const dittoDocuments = await dittoCollection.find().exec();
        
        for (const doc of dittoDocuments) {
          const docData = { id: doc.id, ...doc.value };
          
          // Upsert the document to PostgreSQL
          await this.provider.$queryRaw(
            `INSERT INTO "${tableName}" (id, data) 
             VALUES ($1, $2)
             ON CONFLICT (id) DO UPDATE 
             SET data = $2`,
            [doc.id, JSON.stringify(doc.value)]
          );
        }
      }
      
      if (syncDirection === SyncDirection.FROM_CLOUD || syncDirection === SyncDirection.BIDIRECTIONAL) {
        // Sync from PostgreSQL to Ditto
        const result = await this.provider.$queryRaw(
          `SELECT id, data FROM "${tableName}"`,
          []
        );
        
        // PostgreSQL results are in the rows property
        for (const row of (result as any).rows || []) {
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
      throw new Error('PostgreSQL sync adapter not initialized');
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
