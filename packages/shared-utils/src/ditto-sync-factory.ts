/**
 * Ditto Sync Factory
 * 
 * This module provides a factory for creating and managing multiple cloud
 * synchronization adapters, enabling true multi-cloud synchronization.
 * Each adapter checks for feature flags before initialization.
 */

import { DittoInstance } from './ditto-service';
import { CloudSyncAdapter, SyncDirection, SyncStatus } from './ditto-cloud-sync';

// Import all our sync adapters
import { PostgresSyncAdapter, PostgresSyncConfig } from './ditto-postgres-sync';
import { MongodbSyncAdapter, MongodbSyncConfig } from './ditto-mongodb-sync';
import { CosmosdbSyncAdapter, CosmosdbSyncConfig } from './ditto-cosmosdb-sync';
import { SqlServerSyncAdapter, SqlServerSyncConfig } from './ditto-sqlserver-sync';
import { IbmCloudSyncAdapter, IbmCloudSyncConfig } from './ditto-ibmcloud-sync';
import { SupabaseSyncAdapter, SupabaseSyncConfig } from './ditto-supabase-sync';
import { FirebaseSyncAdapter, FirebaseSyncConfig } from './ditto-firebase-sync';
import { AwsRdsSyncAdapter, AwsRdsSyncConfig } from './ditto-aws-rds-sync';
import { AwsS3SyncAdapter, AwsS3SyncConfig } from './ditto-aws-s3-sync';

/**
 * Helper function to check if a feature flag is enabled
 * @param flagName The name of the feature flag
 * @returns True if enabled, false otherwise
 */
function isFeatureFlagEnabled(flagName: string): boolean {
  // Check for feature flag in various environment variables
  // Standard environment variables
  if (typeof process !== 'undefined' && process.env) {
    if (process.env[flagName] === 'true') return true;
    if (process.env[`ENABLE_${flagName}`] === 'true') return true;
    if (process.env[`FEATURE_${flagName}`] === 'true') return true;
  }
  
  // Expo environment variables
  if (typeof process !== 'undefined' && process.env) {
    if (process.env[`EXPO_PUBLIC_${flagName}`] === 'true') return true;
    if (process.env[`EXPO_PUBLIC_ENABLE_${flagName}`] === 'true') return true;
    if (process.env[`EXPO_PUBLIC_FEATURE_${flagName}`] === 'true') return true;
  }
  
  return false;
}

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
export class DittoSyncFactory {
  private adapters: Map<string, CloudSyncAdapter> = new Map();
  private ditto: DittoInstance | null = null;
  private isInitialized = false;
  
  /**
   * Create a new sync factory
   * @param config Multi-cloud sync configuration
   */
  constructor(private config: MultiCloudSyncConfig) {}
  
  /**
   * Initialize all configured adapters
   * @param ditto The Ditto instance to sync with
   */
  async initialize(ditto: DittoInstance): Promise<void> {
    if (this.isInitialized) {
      return;
    }
    
    this.ditto = ditto;
    
    // Initialize each configured adapter
    const initPromises: Promise<void>[] = [];
    
    // PostgreSQL - only initialize if both config exists AND feature flag is enabled
    if (this.config.postgres && isFeatureFlagEnabled('POSTGRES')) {
      console.log('Initializing PostgreSQL sync adapter');
      const adapter = new PostgresSyncAdapter(this.config.postgres);
      this.adapters.set('postgres', adapter);
      initPromises.push(adapter.initialize(ditto));
    } else if (this.config.postgres) {
      console.log('PostgreSQL sync adapter disabled by feature flag');
    }
    
    // MongoDB - only initialize if both config exists AND feature flag is enabled
    if (this.config.mongodb && isFeatureFlagEnabled('MONGODB')) {
      console.log('Initializing MongoDB sync adapter');
      const adapter = new MongodbSyncAdapter(this.config.mongodb);
      this.adapters.set('mongodb', adapter);
      initPromises.push(adapter.initialize(ditto));
    } else if (this.config.mongodb) {
      console.log('MongoDB sync adapter disabled by feature flag');
    }
    
    // CosmosDB - only initialize if both config exists AND feature flag is enabled
    if (this.config.cosmosdb && isFeatureFlagEnabled('COSMOSDB')) {
      console.log('Initializing CosmosDB sync adapter');
      const adapter = new CosmosdbSyncAdapter(this.config.cosmosdb);
      this.adapters.set('cosmosdb', adapter);
      initPromises.push(adapter.initialize(ditto));
    } else if (this.config.cosmosdb) {
      console.log('CosmosDB sync adapter disabled by feature flag');
    }
    
    // SQL Server - only initialize if both config exists AND feature flag is enabled
    if (this.config.sqlserver && isFeatureFlagEnabled('SQLSERVER')) {
      console.log('Initializing SQL Server sync adapter');
      const adapter = new SqlServerSyncAdapter(this.config.sqlserver);
      this.adapters.set('sqlserver', adapter);
      initPromises.push(adapter.initialize(ditto));
    } else if (this.config.sqlserver) {
      console.log('SQL Server sync adapter disabled by feature flag');
    }
    
    // IBM Cloud DB2 - only initialize if both config exists AND feature flag is enabled
    if (this.config.ibmcloud && isFeatureFlagEnabled('IBMCLOUD')) {
      console.log('Initializing IBM Cloud DB2 sync adapter');
      const adapter = new IbmCloudSyncAdapter(this.config.ibmcloud);
      this.adapters.set('ibmcloud', adapter);
      initPromises.push(adapter.initialize(ditto));
    } else if (this.config.ibmcloud) {
      console.log('IBM Cloud DB2 sync adapter disabled by feature flag');
    }
    
    // Supabase - only initialize if both config exists AND feature flag is enabled
    if (this.config.supabase && isFeatureFlagEnabled('SUPABASE')) {
      console.log('Initializing Supabase sync adapter');
      const adapter = new SupabaseSyncAdapter(this.config.supabase);
      this.adapters.set('supabase', adapter);
      initPromises.push(adapter.initialize(ditto));
    } else if (this.config.supabase) {
      console.log('Supabase sync adapter disabled by feature flag');
    }
    
    // Firebase - only initialize if both config exists AND feature flag is enabled
    if (this.config.firebase && isFeatureFlagEnabled('FIREBASE')) {
      console.log('Initializing Firebase sync adapter');
      const adapter = new FirebaseSyncAdapter(this.config.firebase);
      this.adapters.set('firebase', adapter);
      initPromises.push(adapter.initialize(ditto));
    } else if (this.config.firebase) {
      console.log('Firebase sync adapter disabled by feature flag');
    }
    
    // AWS RDS - only initialize if both config exists AND feature flag is enabled
    if (this.config.awsRds && isFeatureFlagEnabled('AWS_RDS')) {
      console.log('Initializing AWS RDS sync adapter');
      const adapter = new AwsRdsSyncAdapter(this.config.awsRds);
      this.adapters.set('awsRds', adapter);
      initPromises.push(adapter.initialize(ditto));
    } else if (this.config.awsRds) {
      console.log('AWS RDS sync adapter disabled by feature flag');
    }
    
    // AWS S3 - only initialize if both config exists AND feature flag is enabled
    if (this.config.awsS3 && isFeatureFlagEnabled('AWS_S3')) {
      console.log('Initializing AWS S3 sync adapter');
      const adapter = new AwsS3SyncAdapter(this.config.awsS3);
      this.adapters.set('awsS3', adapter);
      initPromises.push(adapter.initialize(ditto));
    } else if (this.config.awsS3) {
      console.log('AWS S3 sync adapter disabled by feature flag');
    }
    
    // Wait for all adapters to initialize
    try {
      await Promise.all(initPromises);
      this.isInitialized = true;
    } catch (error) {
      console.error('Failed to initialize one or more sync adapters:', error);
      // We'll still continue with the adapters that did initialize
      this.isInitialized = true;
    }
  }
  
  /**
   * Get all available adapters
   */
  getAdapters(): CloudSyncAdapter[] {
    return Array.from(this.adapters.values());
  }
  
  /**
   * Get a specific adapter by provider type
   * @param providerType The provider type
   */
  getAdapter(providerType: string): CloudSyncAdapter | undefined {
    return this.adapters.get(providerType);
  }
  
  /**
   * Sync a collection with all configured providers
   * @param collection The collection to sync
   * @param direction The sync direction
   */
  async syncCollection(collection: string, direction?: SyncDirection): Promise<void> {
    if (!this.isInitialized) {
      throw new Error('Sync factory not initialized');
    }
    
    const syncPromises: Promise<void>[] = [];
    
    // Sync with each adapter
    for (const adapter of this.adapters.values()) {
      syncPromises.push(adapter.syncCollection(collection, direction));
    }
    
    // Wait for all syncs to complete
    await Promise.allSettled(syncPromises);
  }
  
  /**
   * Sync all collections with all configured providers
   */
  async syncAll(): Promise<void> {
    if (!this.isInitialized) {
      throw new Error('Sync factory not initialized');
    }
    
    const syncPromises: Promise<void>[] = [];
    
    // Sync with each adapter
    for (const adapter of this.adapters.values()) {
      syncPromises.push(adapter.syncAll());
    }
    
    // Wait for all syncs to complete
    await Promise.allSettled(syncPromises);
  }
  
  /**
   * Get sync status for all adapters
   */
  getSyncStatus(): SyncStatus[] {
    return Array.from(this.adapters.values()).map(adapter => adapter.getSyncStatus());
  }
  
  /**
   * Dispose all adapters
   */
  async dispose(): Promise<void> {
    const disposePromises: Promise<void>[] = [];
    
    // Dispose each adapter
    for (const adapter of this.adapters.values()) {
      disposePromises.push(adapter.dispose());
    }
    
    // Wait for all adapters to dispose
    await Promise.all(disposePromises);
    
    // Clear adapters
    this.adapters.clear();
    this.ditto = null;
    this.isInitialized = false;
  }
}
