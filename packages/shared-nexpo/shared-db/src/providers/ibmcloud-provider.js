import { BaseDatabaseProvider } from './base-provider';
/**
 * IBM Cloud DB2 database provider implementation
 */
export class IBMCloudProvider extends BaseDatabaseProvider {
    static instances = new Map();
    apiKey;
    resourceInstanceId;
    /**
     * Private constructor to enforce singleton pattern per database URL
     */
    constructor(databaseUrl) {
        super(databaseUrl, 'ibmcloud');
        // Get IBM Cloud-specific configuration from environment variables
        this.apiKey = process.env.IBM_APIKEY;
        this.resourceInstanceId = process.env.IBM_RESOURCE_INSTANCE_ID;
        if (!this.apiKey) {
            console.warn('IBM_APIKEY environment variable not set. Some IBM Cloud features may be limited.');
        }
        if (!this.resourceInstanceId) {
            console.warn('IBM_RESOURCE_INSTANCE_ID environment variable not set. Some IBM Cloud features may be limited.');
        }
    }
    /**
     * Get or create an IBMCloudProvider instance for the given database URL
     * @param databaseUrl The database connection URL
     * @returns IBMCloudProvider instance
     */
    static getInstance(databaseUrl) {
        if (!IBMCloudProvider.instances.has(databaseUrl)) {
            IBMCloudProvider.instances.set(databaseUrl, new IBMCloudProvider(databaseUrl));
        }
        return IBMCloudProvider.instances.get(databaseUrl);
    }
    /**
     * Get the provider type
     */
    getProviderType() {
        return 'ibmcloud';
    }
    /**
     * Get the Prisma Client instance with IBM Cloud DB2-specific configuration
     */
    async getClient() {
        if (!this.client) {
            // Check if the connection URL is properly formatted
            if (!this.databaseUrl || !this.databaseUrl.startsWith('postgresql://')) {
                throw new Error('Invalid IBM Cloud DB2 connection URL. The URL should be in PostgreSQL format as Prisma uses PostgreSQL connector for DB2.');
            }
            this.client = await this.generateClient(this.databaseUrl);
            if (this.client) {
                // Add IBM Cloud-specific client extensions or middleware here
                this.client.$use(async (params, next) => {
                    // Handle IBM Cloud specific models
                    if (params.model === 'IbmcloudUser' || params.model === 'IbmcloudPost') {
                        // Add any IBM Cloud DB2-specific query modifications or optimizations
                        // For example, you could add automatic handling of DB2-specific data types
                    }
                    return next(params);
                });
                await this.client.$connect();
            }
        }
        if (!this.client) {
            throw new Error('Failed to initialize IBM Cloud DB2 client');
        }
        return this.client;
    }
    /**
     * Get IBM Cloud DB2 database information
     */
    async getDatabaseInfo() {
        try {
            if (!this.client) {
                await this.getClient();
            }
            // Using the client to query DB2 version and other information
            const versionInfo = await this.client.$queryRaw `
        SELECT version() as version
      `;
            return {
                version: versionInfo[0]?.version || 'Unknown',
                serviceType: 'Db2 on Cloud',
                region: process.env.IBM_CLOUD_REGION || 'Unknown'
            };
        }
        catch (error) {
            console.error('Error getting IBM Cloud DB2 database information:', error);
            return null;
        }
    }
    /**
     * Execute a raw SQL query against the IBM Cloud DB2 database
     */
    async executeRawQuery(query, ...values) {
        try {
            if (!this.client) {
                await this.getClient();
            }
            const result = await this.client.$queryRawUnsafe(query, ...values);
            return {
                rows: Array.isArray(result) ? result : [],
                rowCount: Array.isArray(result) ? result.length : 0,
                command: query.split(' ')[0],
                fields: []
            };
        }
        catch (error) {
            console.error('Error executing raw query against IBM Cloud DB2:', error);
            throw error;
        }
    }
}
//# sourceMappingURL=ibmcloud-provider.js.map