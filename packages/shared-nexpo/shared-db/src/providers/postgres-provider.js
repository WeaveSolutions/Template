import { BaseDatabaseProvider } from './base-provider';
/**
 * PostgreSQL database provider implementation
 */
export class PostgresProvider extends BaseDatabaseProvider {
    static instances = new Map();
    /**
     * Private constructor to enforce singleton pattern per database URL
     */
    constructor(databaseUrl) {
        super(databaseUrl, 'postgres');
    }
    /**
     * Get or create a PostgresProvider instance for the given database URL
     * @param databaseUrl The database connection URL
     * @returns PostgresProvider instance
     */
    static getInstance(databaseUrl) {
        if (!PostgresProvider.instances.has(databaseUrl)) {
            PostgresProvider.instances.set(databaseUrl, new PostgresProvider(databaseUrl));
        }
        return PostgresProvider.instances.get(databaseUrl);
    }
    /**
     * Get the provider type
     */
    getProviderType() {
        return 'postgres';
    }
    /**
     * Get the Prisma Client instance with PostgreSQL-specific configuration
     */
    async getClient() {
        if (!this.client) {
            this.client = await this.generateClient(this.databaseUrl);
            if (this.client) {
                // Add PostgreSQL-specific client extensions or middleware here
                this.client.$use(async (params, next) => {
                    // Example middleware for PostgreSQL-specific operations
                    if (params.model === 'PostgresUser') {
                        // Add any PostgreSQL-specific logic here
                        // For example, you could add automatic timestamps or soft delete functionality
                    }
                    return next(params);
                });
                await this.client.$connect();
            }
        }
        if (!this.client) {
            throw new Error('Failed to initialize PostgreSQL client');
        }
        return this.client;
    }
    /**
     * Execute a raw SQL query
     * @param query SQL query string
     * @param values Optional parameter values
     */
    /**
     * Execute a raw SQL query with type safety
     * @param query SQL query string
     * @param values Optional parameter values
     * @returns Promise with query results
     */
    async $queryRaw(query, values) {
        const client = await this.getClient();
        const queryClient = client;
        if (values && values.length > 0) {
            return queryClient.$queryRawUnsafe(query, ...values);
        }
        return queryClient.$queryRawUnsafe(query);
    }
    /**
     * Check if PostgreSQL-specific features are available
     */
    async checkPostgresFeatures() {
        try {
            const client = await this.getClient();
            const queryClient = client;
            // Check PostgreSQL version
            const versionResult = await queryClient.$queryRawUnsafe('SELECT version() as version');
            // Check available extensions
            const extensionsResult = await queryClient.$queryRawUnsafe('SELECT name FROM pg_available_extensions WHERE installed_version IS NOT NULL');
            return {
                version: versionResult?.[0]?.version || 'unknown',
                extensions: extensionsResult?.map(ext => ext.name) || []
            };
        }
        catch (error) {
            console.error('Error checking PostgreSQL features:', error);
            return {
                version: 'unknown',
                extensions: []
            };
        }
    }
    /**
     * Clean up all PostgresProvider instances
     */
    static async disconnectAll() {
        await Promise.all(Array.from(PostgresProvider.instances.values()).map(provider => provider.disconnect().catch(error => {
            console.error('Error disconnecting PostgresProvider:', error);
        })));
        PostgresProvider.instances.clear();
    }
}
//# sourceMappingURL=postgres-provider.js.map