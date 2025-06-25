/**
 * Ditto CosmosDB Sync Adapter
 *
 * This module provides synchronization capabilities between Ditto
 * and Azure Cosmos DB databases.
 */
import { CosmosdbProvider } from '../../shared-db/src/providers/cosmosdb-provider';
import { SyncDirection } from './ditto-cloud-sync';
/**
 * CosmosDB sync adapter for Ditto
 * Implements the CloudSyncAdapter interface
 */
export class CosmosdbSyncAdapter {
    ditto = null;
    provider = null;
    config;
    isInitialized = false;
    lastSyncTime = null;
    /**
     * Create a new CosmosDB sync adapter
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
        // Initialize the CosmosDB provider
        try {
            this.provider = CosmosdbProvider.getInstance(this.config.connectionString);
            await this.provider.connect();
            this.isInitialized = true;
        }
        catch (error) {
            console.error('Failed to initialize CosmosDB sync adapter:', error);
            throw error;
        }
    }
    /**
     * Get the provider type
     */
    getProviderType() {
        return 'cosmosdb';
    }
    /**
     * Sync data between Ditto and CosmosDB
     * @param collection The collection to sync
     * @param direction The direction to sync
     */
    async syncCollection(collection, direction) {
        if (!this.isInitialized || !this.ditto || !this.provider) {
            throw new Error('CosmosDB sync adapter not initialized');
        }
        // Use default direction if not specified
        const syncDirection = direction || this.config.defaultDirection;
        try {
            const dittoCollection = this.ditto.collection(collection);
            // Format the collection name for CosmosDB
            const cosmosCollection = collection;
            if (syncDirection === SyncDirection.TO_CLOUD || syncDirection === SyncDirection.BIDIRECTIONAL) {
                // Sync from Ditto to CosmosDB
                const dittoDocuments = await dittoCollection.find().exec();
                for (const doc of dittoDocuments) {
                    // Convert Ditto document to CosmosDB document
                    const cosmosDoc = {
                        id: doc.id,
                        ...doc.value
                    };
                    // Upsert the document to CosmosDB
                    // Note: CosmosDB has specific SQL-like query language
                    await this.provider.executeRawQuery(`
            SELECT * FROM c WHERE c.id = '${doc.id}'
          `).then(async (existingDocs) => {
                        if (existingDocs.length > 0) {
                            // Update existing document
                            await this.provider.executeRawQuery(`
                UPDATE c
                SET c = ${JSON.stringify(cosmosDoc)}
                WHERE c.id = '${doc.id}'
              `);
                        }
                        else {
                            // Insert new document
                            const container = this.provider.getClient().database(this.config.databaseName).container(cosmosCollection);
                            await container.items.create(cosmosDoc);
                        }
                    });
                }
            }
            if (syncDirection === SyncDirection.FROM_CLOUD || syncDirection === SyncDirection.BIDIRECTIONAL) {
                // Sync from CosmosDB to Ditto
                const result = await this.provider.executeRawQuery(`
          SELECT * FROM c
        `);
                for (const doc of result) {
                    // Prepare the document for Ditto
                    const { id, ...data } = doc;
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
            throw new Error('CosmosDB sync adapter not initialized');
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
//# sourceMappingURL=ditto-cosmosdb-sync.js.map
