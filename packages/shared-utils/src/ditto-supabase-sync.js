/**
 * Ditto Supabase Sync Adapter
 *
 * This module provides synchronization capabilities between Ditto
 * and Supabase databases.
 */
import { SupabaseProvider } from '../../shared-db/src/providers/supabase-provider';
import { SyncDirection } from './ditto-cloud-sync';
/**
 * Supabase sync adapter for Ditto
 * Implements the CloudSyncAdapter interface
 */
export class SupabaseSyncAdapter {
    ditto = null;
    provider = null;
    config;
    isInitialized = false;
    lastSyncTime = null;
    /**
     * Create a new Supabase sync adapter
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
        // Initialize the Supabase provider
        try {
            this.provider = SupabaseProvider.getInstance(this.config.url, this.config.apiKey);
            await this.provider.connect();
            this.isInitialized = true;
        }
        catch (error) {
            console.error('Failed to initialize Supabase sync adapter:', error);
            throw error;
        }
    }
    /**
     * Get the provider type
     */
    getProviderType() {
        return 'supabase';
    }
    /**
     * Sync data between Ditto and Supabase
     * @param collection The collection to sync
     * @param direction The direction to sync
     */
    async syncCollection(collection, direction) {
        if (!this.isInitialized || !this.ditto || !this.provider) {
            throw new Error('Supabase sync adapter not initialized');
        }
        // Use default direction if not specified
        const syncDirection = direction || this.config.defaultDirection;
        try {
            const dittoCollection = this.ditto.collection(collection);
            // Determine the table name in Supabase - we prefix with Supabase
            const tableName = `Supabase${collection}`;
            const schema = this.config.schema || 'public';
            if (syncDirection === SyncDirection.TO_CLOUD || syncDirection === SyncDirection.BIDIRECTIONAL) {
                // Sync from Ditto to Supabase
                const dittoDocuments = await dittoCollection.find().exec();
                for (const doc of dittoDocuments) {
                    const docData = { id: doc.id, ...doc.value };
                    // Upsert the document to Supabase
                    await this.provider.executeRawQuery(`INSERT INTO "${schema}"."${tableName}" (id, data) 
             VALUES ($1, $2)
             ON CONFLICT (id) DO UPDATE 
             SET data = $2`, [doc.id, JSON.stringify(doc.value)]);
                }
            }
            if (syncDirection === SyncDirection.FROM_CLOUD || syncDirection === SyncDirection.BIDIRECTIONAL) {
                // Sync from Supabase to Ditto
                const result = await this.provider.executeRawQuery(`SELECT id, data FROM "${schema}"."${tableName}"`, []);
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
            throw new Error('Supabase sync adapter not initialized');
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
//# sourceMappingURL=ditto-supabase-sync.js.map
