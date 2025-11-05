/**
 * Ditto AWS RDS Sync Adapter
 *
 * This module provides synchronization capabilities between Ditto
 * and AWS RDS databases (PostgreSQL, MySQL, etc.).
 */
import { AwsRdsProvider } from '../../shared-db/src/providers/aws-rds-provider';
import { SyncDirection } from './ditto-cloud-sync';
/**
 * Engine types for AWS RDS
 */
export var AwsRdsEngineType;
(function (AwsRdsEngineType) {
    AwsRdsEngineType["POSTGRES"] = "postgres";
    AwsRdsEngineType["MYSQL"] = "mysql";
    AwsRdsEngineType["MARIADB"] = "mariadb";
    AwsRdsEngineType["AURORA"] = "aurora";
    AwsRdsEngineType["SQLSERVER"] = "sqlserver";
})(AwsRdsEngineType || (AwsRdsEngineType = {}));
/**
 * AWS RDS sync adapter for Ditto
 * Implements the CloudSyncAdapter interface
 */
export class AwsRdsSyncAdapter {
    ditto = null;
    provider = null;
    config;
    isInitialized = false;
    lastSyncTime = null;
    /**
     * Create a new AWS RDS sync adapter
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
        // Initialize the AWS RDS provider
        try {
            this.provider = AwsRdsProvider.getInstance({
                connectionString: this.config.connectionString,
                region: this.config.region,
                accessKeyId: this.config.accessKeyId,
                secretAccessKey: this.config.secretAccessKey,
                engineType: this.config.engineType,
                databaseName: this.config.databaseName
            });
            await this.provider.connect();
            this.isInitialized = true;
        }
        catch (error) {
            console.error('Failed to initialize AWS RDS sync adapter:', error);
            throw error;
        }
    }
    /**
     * Get the provider type
     */
    getProviderType() {
        return `aws-rds-${this.config.engineType}`;
    }
    /**
     * Sync data between Ditto and AWS RDS
     * @param collection The collection to sync
     * @param direction The direction to sync
     */
    async syncCollection(collection, direction) {
        if (!this.isInitialized || !this.ditto || !this.provider) {
            throw new Error('AWS RDS sync adapter not initialized');
        }
        // Use default direction if not specified
        const syncDirection = direction || this.config.defaultDirection;
        try {
            const dittoCollection = this.ditto.collection(collection);
            // In AWS RDS, we prefix the table with AwsRds
            const tableName = `AwsRds${collection}`;
            const schema = this.config.schema || 'public';
            if (syncDirection === SyncDirection.TO_CLOUD || syncDirection === SyncDirection.BIDIRECTIONAL) {
                // Sync from Ditto to AWS RDS
                const dittoDocuments = await dittoCollection.find().exec();
                for (const doc of dittoDocuments) {
                    const docData = { id: doc.id, ...doc.value };
                    // Handle different engines
                    switch (this.config.engineType) {
                        case AwsRdsEngineType.POSTGRES:
                        case AwsRdsEngineType.AURORA: // Aurora PostgreSQL compatible
                            await this.provider.executeRawQuery(`INSERT INTO "${schema}"."${tableName}" (id, data) 
                 VALUES ($1, $2)
                 ON CONFLICT (id) DO UPDATE 
                 SET data = $2`, [doc.id, JSON.stringify(doc.value)]);
                            break;
                        case AwsRdsEngineType.MYSQL:
                        case AwsRdsEngineType.MARIADB:
                            await this.provider.executeRawQuery(`INSERT INTO ${schema}.${tableName} (id, data) 
                 VALUES (?, ?)
                 ON DUPLICATE KEY UPDATE
                 data = ?`, [doc.id, JSON.stringify(doc.value), JSON.stringify(doc.value)]);
                            break;
                        case AwsRdsEngineType.SQLSERVER:
                            await this.provider.executeRawQuery(`MERGE INTO ${schema}.${tableName} AS target
                 USING (SELECT @p1 AS id) AS source
                 ON target.id = source.id
                 WHEN MATCHED THEN
                   UPDATE SET data = @p2
                 WHEN NOT MATCHED THEN
                   INSERT (id, data) VALUES (@p1, @p2);`, [doc.id, JSON.stringify(doc.value)]);
                            break;
                        default:
                            throw new Error(`Unsupported engine type: ${this.config.engineType}`);
                    }
                }
            }
            if (syncDirection === SyncDirection.FROM_CLOUD || syncDirection === SyncDirection.BIDIRECTIONAL) {
                // Sync from AWS RDS to Ditto
                let query = '';
                // Handle different engines for querying
                switch (this.config.engineType) {
                    case AwsRdsEngineType.POSTGRES:
                    case AwsRdsEngineType.AURORA:
                        query = `SELECT id, data FROM "${schema}"."${tableName}"`;
                        break;
                    case AwsRdsEngineType.MYSQL:
                    case AwsRdsEngineType.MARIADB:
                    case AwsRdsEngineType.SQLSERVER:
                        query = `SELECT id, data FROM ${schema}.${tableName}`;
                        break;
                    default:
                        throw new Error(`Unsupported engine type: ${this.config.engineType}`);
                }
                const result = await this.provider.executeRawQuery(query, []);
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
            throw new Error('AWS RDS sync adapter not initialized');
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
//# sourceMappingURL=ditto-aws-rds-sync.js.map
