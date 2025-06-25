/**
 * Ditto SQL Server Sync Adapter
 * 
 * This module provides synchronization capabilities between Ditto
 * and Microsoft SQL Server databases.
 */

import { DittoInstance } from './ditto-service';
import { SqlserverProvider } from '../../shared-db/src/providers/sqlserver-provider';
import { CloudSyncAdapter, SyncDirection, SyncStatus } from './ditto-cloud-sync';

/**
 * Configuration for SQL Server sync adapter
 */
export interface SqlServerSyncConfig {
  // SQL Server connection info
  connectionString: string;
  // Collections to sync
  collections: string[];
  // Default sync direction
  defaultDirection: SyncDirection;
  // Schema name
  schema: string;
}

/**
 * SQL Server sync adapter for Ditto
 * Implements the CloudSyncAdapter interface
 */
export class SqlServerSyncAdapter implements CloudSyncAdapter {
  private ditto: DittoInstance | null = null;
  private provider: SqlserverProvider | null = null;
  private config: SqlServerSyncConfig;
  private isInitialized = false;
  private lastSyncTime: Date | null = null;
  
  /**
   * Create a new SQL Server sync adapter
   */
  constructor(config: SqlServerSyncConfig) {
    this.config = config;
    
    // Set default schema if not provided
    if (!this.config.schema) {
      this.config.schema = 'dbo';
    }
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
    
    // Initialize the SQL Server provider
    try {
      this.provider = SqlserverProvider.getInstance(this.config.connectionString);
      await this.provider.connect();
      this.isInitialized = true;
    } catch (error) {
      console.error('Failed to initialize SQL Server sync adapter:', error);
      throw error;
    }
  }
  
  /**
   * Get the provider type
   */
  getProviderType(): string {
    return 'sqlserver';
  }
  
  /**
   * Sync data between Ditto and SQL Server
   * @param collection The collection to sync
   * @param direction The direction to sync
   */
  async syncCollection(collection: string, direction?: SyncDirection): Promise<void> {
    if (!this.isInitialized || !this.ditto || !this.provider) {
      throw new Error('SQL Server sync adapter not initialized');
    }
    
    // Use default direction if not specified
    const syncDirection = direction || this.config.defaultDirection;
    
    try {
      const dittoCollection = this.ditto.collection(collection);
      
      // Format the table name for SQL Server
      const tableName = `[${this.config.schema}].[${collection}]`;
      
      if (syncDirection === SyncDirection.TO_CLOUD || syncDirection === SyncDirection.BIDIRECTIONAL) {
        // Sync from Ditto to SQL Server
        const dittoDocuments = await dittoCollection.find().exec();
        
        for (const doc of dittoDocuments) {
          // Check if document exists
          const existingResult = await this.provider.executeRawQuery(
            `SELECT Id FROM ${tableName} WHERE Id = @p1`,
            [doc.id]
          );
          
          if (existingResult.length > 0) {
            // Update existing document
            await this.provider.executeRawQuery(
              `UPDATE ${tableName} SET Data = @p1 WHERE Id = @p2`,
              [JSON.stringify(doc.value), doc.id]
            );
          } else {
            // Insert new document
            await this.provider.executeRawQuery(
              `INSERT INTO ${tableName} (Id, Data) VALUES (@p1, @p2)`,
              [doc.id, JSON.stringify(doc.value)]
            );
          }
        }
      }
      
      if (syncDirection === SyncDirection.FROM_CLOUD || syncDirection === SyncDirection.BIDIRECTIONAL) {
        // Sync from SQL Server to Ditto
        const result = await this.provider.executeRawQuery(
          `SELECT Id, Data FROM ${tableName}`,
          []
        );
        
        for (const row of result) {
          // Parse the data
          const id = row.Id;
          const data = typeof row.Data === 'string' ? JSON.parse(row.Data) : row.Data;
          
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
      throw new Error('SQL Server sync adapter not initialized');
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
