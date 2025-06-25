/**
 * Ditto Cloud Sync service
 *
 * This module provides a service for synchronizing Ditto data with cloud databases.
 */
import { DittoSyncFactory } from './ditto-sync-factory';
/**
 * Sync direction enum
 */
export var SyncDirection;
(function (SyncDirection) {
    SyncDirection["TO_CLOUD"] = "to_cloud";
    SyncDirection["FROM_CLOUD"] = "from_cloud";
    SyncDirection["BIDIRECTIONAL"] = "bidirectional";
})(SyncDirection || (SyncDirection = {}));
/**
 * Default collection list for synchronization
 */
const DEFAULT_COLLECTIONS = ['todos', 'users', 'posts', 'documents'];
/**
 * Create a Cloud Sync service
 */
export function createCloudSyncService(config) {
    // Create shared collections list
    const collections = DEFAULT_COLLECTIONS;
    const defaultDirection = SyncDirection.BIDIRECTIONAL;
    const syncInterval = config.syncInterval || 300000; // 5 minutes default
    // Create multi-cloud config
    const multiCloudConfig = {};
    /**
     * Helper function to check if a feature flag is enabled
     * @param flagName The name of the feature flag
     * @returns True if enabled, false otherwise
     */
    const isProviderEnabled = (providerName) => {
        // Check several common feature flag patterns
        if (process.env[`ENABLE_${providerName.toUpperCase()}`] === 'true')
            return true;
        if (process.env[`FEATURE_${providerName.toUpperCase()}`] === 'true')
            return true;
        if (process.env[`DITTO_SYNC_${providerName.toUpperCase()}`] === 'true')
            return true;
        // Check Expo public vars
        if (process.env[`EXPO_PUBLIC_ENABLE_${providerName.toUpperCase()}`] === 'true')
            return true;
        if (process.env[`EXPO_PUBLIC_FEATURE_${providerName.toUpperCase()}`] === 'true')
            return true;
        if (process.env[`EXPO_PUBLIC_DITTO_SYNC_${providerName.toUpperCase()}`] === 'true')
            return true;
        // Check if provider is in the providers list and no explicit feature flag found
        return config.providers?.includes(providerName) ?? false;
    };
    // Configure each provider based on feature flags AND enabled providers list
    // PostgreSQL
    if (isProviderEnabled('postgres') && config.postgresConfig) {
        console.log('Configuring PostgreSQL for Ditto sync');
        multiCloudConfig.postgres = {
            ...config.postgresConfig,
            collections,
            defaultDirection
        };
    }
    else if (isProviderEnabled('postgres')) {
        const connString = process.env.POSTGRES_CONNECTION_STRING || '';
        if (connString) {
            console.log('Configuring PostgreSQL for Ditto sync with connection string');
            multiCloudConfig.postgres = {
                connectionString: connString,
                collections,
                defaultDirection
            };
        }
        else {
            console.log('PostgreSQL sync enabled but no connection string found');
        }
    }
    // MongoDB
    if (isProviderEnabled('mongodb') && config.mongodbConfig) {
        console.log('Configuring MongoDB for Ditto sync');
        multiCloudConfig.mongodb = {
            ...config.mongodbConfig,
            collections,
            defaultDirection
        };
    }
    else if (isProviderEnabled('mongodb')) {
        const connString = process.env.MONGODB_CONNECTION_STRING || '';
        if (connString) {
            console.log('Configuring MongoDB for Ditto sync with connection string');
            multiCloudConfig.mongodb = {
                connectionString: connString,
                collections,
                defaultDirection
            };
        }
        else {
            console.log('MongoDB sync enabled but no connection string found');
        }
    }
    // CosmosDB
    if (isProviderEnabled('cosmosdb') && config.cosmosdbConfig) {
        console.log('Configuring CosmosDB for Ditto sync');
        multiCloudConfig.cosmosdb = {
            ...config.cosmosdbConfig,
            collections,
            defaultDirection
        };
    }
    else if (isProviderEnabled('cosmosdb')) {
        const connString = process.env.COSMOSDB_CONNECTION_STRING || '';
        const dbName = process.env.COSMOSDB_DATABASE_NAME || 'dittoSync';
        if (connString) {
            console.log('Configuring CosmosDB for Ditto sync with connection string');
            multiCloudConfig.cosmosdb = {
                connectionString: connString,
                collections,
                defaultDirection,
                databaseName: dbName
            };
        }
        else {
            console.log('CosmosDB sync enabled but no connection string found');
        }
    }
    // SQL Server
    if (isProviderEnabled('sqlserver') && config.sqlserverConfig) {
        console.log('Configuring SQL Server for Ditto sync');
        multiCloudConfig.sqlserver = {
            ...config.sqlserverConfig,
            collections,
            defaultDirection
        };
    }
    else if (isProviderEnabled('sqlserver')) {
        const connString = process.env.SQLSERVER_CONNECTION_STRING || '';
        const schema = process.env.SQLSERVER_SCHEMA || 'dbo';
        if (connString) {
            console.log('Configuring SQL Server for Ditto sync with connection string');
            multiCloudConfig.sqlserver = {
                connectionString: connString,
                collections,
                defaultDirection,
                schema
            };
        }
        else {
            console.log('SQL Server sync enabled but no connection string found');
        }
    }
    // IBM Cloud DB2
    if (isProviderEnabled('ibmcloud') && config.ibmcloudConfig) {
        console.log('Configuring IBM Cloud DB2 for Ditto sync');
        multiCloudConfig.ibmcloud = {
            ...config.ibmcloudConfig,
            collections,
            defaultDirection
        };
    }
    else if (isProviderEnabled('ibmcloud')) {
        const connString = process.env.IBMCLOUD_CONNECTION_STRING || '';
        if (connString) {
            console.log('Configuring IBM Cloud DB2 for Ditto sync with connection string');
            multiCloudConfig.ibmcloud = {
                connectionString: connString,
                collections,
                defaultDirection
            };
        }
        else {
            console.log('IBM Cloud DB2 sync enabled but no connection string found');
        }
    }
    // Log provider configuration status
    console.log('Ditto cloud sync providers configured:', Object.keys(multiCloudConfig).length > 0 ?
        Object.keys(multiCloudConfig).join(', ') : 'None');
    // Create sync factory
    const syncFactory = new DittoSyncFactory(multiCloudConfig);
    // Store ditto instance when initialized
    let dittoInstance = null;
    // Create sync interval handler
    let syncIntervalId = null;
    return {
        async initialize() {
            // Listen for Ditto instance
            const dittoPromise = new Promise((resolve) => {
                // Check if Ditto is available in window or global
                const ditto = (typeof window !== 'undefined' && window.dittoInstance) ||
                    (typeof global !== 'undefined' && global.dittoInstance);
                if (ditto) {
                    resolve(ditto);
                }
                else {
                    // Wait for Ditto to be available
                    const checkInterval = setInterval(() => {
                        const ditto = (typeof window !== 'undefined' && window.dittoInstance) ||
                            (typeof global !== 'undefined' && global.dittoInstance);
                        if (ditto) {
                            clearInterval(checkInterval);
                            resolve(ditto);
                        }
                    }, 100);
                    // Timeout after 10 seconds
                    setTimeout(() => {
                        clearInterval(checkInterval);
                        throw new Error('Ditto not found after 10 seconds');
                    }, 10000);
                }
            });
            try {
                // Wait for Ditto to be available
                dittoInstance = await dittoPromise;
                // Initialize sync factory
                await syncFactory.initialize(dittoInstance);
                // Set up automatic sync if interval is provided
                if (syncInterval > 0) {
                    syncIntervalId = setInterval(async () => {
                        try {
                            await syncFactory.syncAll();
                        }
                        catch (error) {
                            console.error('Error during automatic sync:', error);
                        }
                    }, syncInterval);
                }
            }
            catch (error) {
                console.error('Failed to initialize cloud sync:', error);
                throw error;
            }
        },
        async syncCollection(collection, direction) {
            await syncFactory.syncCollection(collection, direction);
        },
        async syncAll() {
            await syncFactory.syncAll();
        },
        getSyncStatus() {
            return syncFactory.getSyncStatus();
        },
        async dispose() {
            // Clear sync interval
            if (syncIntervalId) {
                clearInterval(syncIntervalId);
                syncIntervalId = null;
            }
            // Dispose sync factory
            await syncFactory.dispose();
            dittoInstance = null;
        }
    };
}
lastSyncTime: (Map) = new Map();
constructor(config, CloudSyncConfig);
{
    this.config = config;
}
/**
 * Initialize the cloud sync service
 */
async;
initialize();
Promise < void  > {
    // Initialize DB clients for each enabled provider
    const: providers = this.config.providers,
    if(providers) { }, : .postgres
};
{
    this.dbClients.set('postgres', await DatabaseClient.getInstance('postgres'));
}
if (providers.mongodb) {
    this.dbClients.set('mongodb', await DatabaseClient.getInstance('mongodb'));
}
if (providers.supabase) {
    this.dbClients.set('supabase', await DatabaseClient.getInstance('supabase'));
}
if (providers.cosmosdb) {
    this.dbClients.set('cosmosdb', await DatabaseClient.getInstance('cosmosdb'));
}
if (providers.sqlserver) {
    this.dbClients.set('sqlserver', await DatabaseClient.getInstance('sqlserver'));
}
if (providers.ibmcloud) {
    this.dbClients.set('ibmcloud', await DatabaseClient.getInstance('ibmcloud'));
}
// Set up initial sync states
for (const mapping of this.config.collections) {
    this.isSyncing.set(mapping.dittoCollection, false);
    this.lastSyncTime.set(mapping.dittoCollection, new Date(0)); // Start with epoch
}
// Start auto-sync if enabled
if (this.config.enabled && this.config.autoSync) {
    this.startAutoSync();
}
console.log('DittoCloudSync initialized with providers:', Array.from(this.dbClients.keys()));
/**
 * Start automatic synchronization for all collections
 */
startAutoSync();
void {
    : .config.collections
};
{
    if (this.syncTimers.has(mapping.dittoCollection)) {
        clearInterval(this.syncTimers.get(mapping.dittoCollection));
    }
    const timer = setInterval(() => {
        this.syncCollection(mapping);
    }, this.config.syncInterval);
    this.syncTimers.set(mapping.dittoCollection, timer);
}
console.log('AutoSync started with interval:', this.config.syncInterval, 'ms');
/**
 * Stop automatic synchronization
 */
stopAutoSync();
void {
    : .syncTimers.entries()
};
{
    clearInterval(timer);
    this.syncTimers.delete(collection);
}
console.log('AutoSync stopped');
/**
 * Manually trigger synchronization for all collections
 */
async;
syncAll();
Promise < void  > {
    : .config.collections
};
{
    await this.syncCollection(mapping);
}
/**
 * Synchronize a specific collection with its cloud counterpart
 */
async;
syncCollection(mapping, CollectionMapping);
Promise < void  > {
    const: { dittoCollection, cloudTable, primaryKey, provider, mappings } = mapping,
    : .isSyncing.get(dittoCollection)
};
{
    return;
}
// Check if we have a DB client for this provider
if (!this.dbClients.has(provider)) {
    console.warn(`No DB client for provider: ${provider}, skipping sync`);
    return;
}
const dbClient = this.dbClients.get(provider);
try {
    this.isSyncing.set(dittoCollection, true);
    // 1. Get data from Ditto
    const dittoData = await this.ditto.query(dittoCollection, {});
    // 2. Get data from cloud database
    const query = `SELECT * FROM ${cloudTable}`;
    const cloudData = await dbClient.executeRaw(query);
    // 3. Prepare data for comparison
    const dittoMap = new Map();
    const cloudMap = new Map();
    // Map Ditto data by ID
    for (const item of dittoData) {
        dittoMap.set(item.id, item);
    }
    // Map cloud data by primary key
    for (const item of cloudData) {
        cloudMap.set(item[primaryKey], item);
    }
    // 4. Find items to insert, update, or delete
    const toInsertInCloud = [];
    const toUpdateInCloud = [];
    const toInsertInDitto = [];
    const toUpdateInDitto = [];
    // Items in Ditto but not in cloud -> insert to cloud
    for (const [id, dittoItem] of dittoMap.entries()) {
        if (!cloudMap.has(id)) {
            toInsertInCloud.push(this.mapDittoToCloud(dittoItem, mappings));
        }
        else {
            // Compare and update if different
            const cloudItem = cloudMap.get(id);
            if (this.isDifferent(dittoItem, cloudItem, mappings)) {
                toUpdateInCloud.push({
                    id,
                    data: this.mapDittoToCloud(dittoItem, mappings),
                });
            }
        }
    }
    // Items in cloud but not in Ditto -> insert to Ditto
    for (const [id, cloudItem] of cloudMap.entries()) {
        if (!dittoMap.has(id)) {
            toInsertInDitto.push(this.mapCloudToDitto(cloudItem, mappings));
        }
        else {
            // Compare and update if different (already handled above)
        }
    }
    // 5. Apply changes in batches
    // Insert to cloud
    for (let i = 0; i < toInsertInCloud.length; i += this.config.maxBatchSize) {
        const batch = toInsertInCloud.slice(i, i + this.config.maxBatchSize);
        if (batch.length > 0) {
            const placeholders = batch.map((_, idx) => {
                const fields = Object.keys(batch[idx]);
                const values = fields.map((_, valueIdx) => `$${valueIdx + 1}`).join(', ');
                return `(${values})`;
            }).join(', ');
            const fields = Object.keys(batch[0]).join(', ');
            const query = `INSERT INTO ${cloudTable} (${fields}) VALUES ${placeholders}`;
            // Flatten values for parameterized query
            const values = batch.flatMap(item => Object.values(item));
            await dbClient.executeRaw(query, values);
        }
    }
    // Update in cloud
    for (const item of toUpdateInCloud) {
        const { id, data } = item;
        const fields = Object.keys(data);
        const setClause = fields.map((field, idx) => `${field} = $${idx + 1}`).join(', ');
        const query = `UPDATE ${cloudTable} SET ${setClause} WHERE ${primaryKey} = $${fields.length + 1}`;
        await dbClient.executeRaw(query, [...Object.values(data), id]);
    }
    // Insert to Ditto
    for (const item of toInsertInDitto) {
        await this.ditto.insert(dittoCollection, item);
    }
    // Update in Ditto
    for (const item of toUpdateInDitto) {
        const { id, data } = item;
        await this.ditto.update(dittoCollection, id, data);
    }
    // Update last sync time
    this.lastSyncTime.set(dittoCollection, new Date());
    console.log(`Sync completed for ${dittoCollection}:`, {
        cloudInserts: toInsertInCloud.length,
        cloudUpdates: toUpdateInCloud.length,
        dittoInserts: toInsertInDitto.length,
        dittoUpdates: toUpdateInDitto.length,
    });
}
catch (error) {
    console.error(`Error syncing collection ${dittoCollection}:`, error);
}
finally {
    this.isSyncing.set(dittoCollection, false);
}
mapDittoToCloud(dittoItem, any, mappings, FieldMapping[]);
any;
{
    const result = {};
    for (const mapping of mappings) {
        const { dittoField, cloudField, type } = mapping;
        if (dittoItem[dittoField] !== undefined) {
            let value = dittoItem[dittoField];
            // Convert types if needed
            if (type === 'date' && value && !(value instanceof Date)) {
                value = new Date(value);
            }
            else if (type === 'object' && typeof value !== 'object') {
                try {
                    value = JSON.parse(value);
                }
                catch (e) {
                    value = {};
                }
            }
            else if (type === 'array' && !Array.isArray(value)) {
                try {
                    value = JSON.parse(value);
                    if (!Array.isArray(value)) {
                        value = [];
                    }
                }
                catch (e) {
                    value = [];
                }
            }
            result[cloudField] = value;
        }
        else if (mapping.isRequired) {
            // Provide defaults for required fields
            switch (type) {
                case 'string':
                    result[cloudField] = '';
                    break;
                case 'number':
                    result[cloudField] = 0;
                    break;
                case 'boolean':
                    result[cloudField] = false;
                    break;
                case 'date':
                    result[cloudField] = new Date();
                    break;
                case 'object':
                    result[cloudField] = {};
                    break;
                case 'array':
                    result[cloudField] = [];
                    break;
            }
        }
    }
    return result;
}
mapCloudToDitto(cloudItem, any, mappings, FieldMapping[]);
any;
{
    const result = {};
    for (const mapping of mappings) {
        const { dittoField, cloudField, type } = mapping;
        if (cloudItem[cloudField] !== undefined) {
            let value = cloudItem[cloudField];
            // Convert types if needed
            if (type === 'object' && typeof value !== 'object') {
                try {
                    value = JSON.parse(value);
                }
                catch (e) {
                    value = {};
                }
            }
            else if (type === 'array' && !Array.isArray(value)) {
                try {
                    value = JSON.parse(value);
                    if (!Array.isArray(value)) {
                        value = [];
                    }
                }
                catch (e) {
                    value = [];
                }
            }
            result[dittoField] = value;
        }
    }
    return result;
}
isDifferent(dittoItem, any, cloudItem, any, mappings, FieldMapping[]);
boolean;
{
    for (const mapping of mappings) {
        const { dittoField, cloudField, type } = mapping;
        // Skip if field doesn't exist in one of the items
        if (dittoItem[dittoField] === undefined || cloudItem[cloudField] === undefined) {
            return true;
        }
        const dittoValue = dittoItem[dittoField];
        const cloudValue = cloudItem[cloudField];
        // Compare based on type
        if (type === 'date') {
            const d1 = dittoValue instanceof Date ? dittoValue : new Date(dittoValue);
            const d2 = cloudValue instanceof Date ? cloudValue : new Date(cloudValue);
            if (d1.getTime() !== d2.getTime()) {
                return true;
            }
        }
        else if (type === 'object' || type === 'array') {
            const v1 = typeof dittoValue === 'string' ? JSON.parse(dittoValue) : dittoValue;
            const v2 = typeof cloudValue === 'string' ? JSON.parse(cloudValue) : cloudValue;
            if (JSON.stringify(v1) !== JSON.stringify(v2)) {
                return true;
            }
        }
        else if (dittoValue !== cloudValue) {
            return true;
        }
    }
    return false;
}
/**
 * Get sync status for all collections
 */
getSyncStatus();
{
    collection: string;
    lastSync: Date;
    isSyncing: boolean;
}
[];
{
    return this.config.collections.map(mapping => ({
        collection: mapping.dittoCollection,
        lastSync: this.lastSyncTime.get(mapping.dittoCollection) || new Date(0),
        isSyncing: this.isSyncing.get(mapping.dittoCollection) || false,
    }));
}
/**
 * Clean up resources
 */
async;
dispose();
Promise < void  > {
    this: .stopAutoSync(),
    : .dbClients.values()
};
{
    await client.disconnect();
}
this.dbClients.clear();
// Export a factory function to create the sync service
export function createCloudSyncService(config) {
    return new DittoCloudSync(config);
}
//# sourceMappingURL=ditto-cloud-sync.js.map
