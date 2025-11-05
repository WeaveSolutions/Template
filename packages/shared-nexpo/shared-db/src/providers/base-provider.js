// No need for path/url imports - simplifying to avoid TypeScript errors
// Use a more reliable way to get the base directory
const __dirname = './';
export class BaseDatabaseProvider {
    client = null;
    databaseUrl;
    providerName;
    isConnected = false;
    constructor(databaseUrl, providerName) {
        this.databaseUrl = databaseUrl;
        this.providerName = providerName;
    }
    /**
     * Get the database URL
     */
    getDatabaseUrl() {
        return this.databaseUrl;
    }
    /**
     * Get the path to the Prisma Client for this provider
     */
    getClientPath() {
        return `@prisma/client-${this.providerName}`;
    }
    /**
     * Get the Prisma Client instance, creating it if it doesn't exist
     */
    async getClient() {
        if (!this.client) {
            this.client = await this.generateClient(this.databaseUrl);
        }
        if (!this.isConnected) {
            const client = this.client;
            await client.$connect();
            this.isConnected = true;
        }
        // We know client is not null here because we just set it above
        return this.client;
    }
    /**
     * Disconnect from the database
     */
    async disconnect() {
        if (this.client && this.isConnected) {
            const client = this.client;
            await client.$disconnect();
            this.isConnected = false;
        }
    }
    /**
     * Generate a new Prisma Client instance with the correct provider configuration
     */
    async generateClient(databaseUrl) {
        try {
            // Dynamically import the generated Prisma client
            const prismaModule = await import(this.getClientPath());
            const { PrismaClient } = prismaModule;
            // Create a new Prisma client instance with the provided database URL
            const client = new PrismaClient({
                datasources: {
                    db: {
                        url: databaseUrl,
                    },
                },
                log: this.getLogLevels(),
            });
            // Add type-safe middleware support if the client supports it
            if (client && typeof client.$use === 'function') {
                client.$use(async (params, next) => {
                    // Base middleware - can be overridden by child classes
                    return next(params);
                });
            }
            return client;
        }
        catch (error) {
            console.error(`Failed to initialize ${this.providerName} client:`, error);
            throw new Error(`Failed to initialize ${this.providerName} client`);
        }
    }
    /**
     * Get the appropriate log levels based on environment
     */
    getLogLevels() {
        if (process.env.NODE_ENV === 'production') {
            return ['error'];
        }
        return ['query', 'error', 'warn'];
    }
    /**
     * Execute a transaction with the current client
     */
    async $transaction(fn) {
        const client = await this.getClient();
        const prismaClient = client;
        return prismaClient.$transaction(fn);
    }
    /**
     * Check if the database connection is healthy
     */
    async checkHealth() {
        try {
            const client = await this.getClient();
            const prismaClient = client;
            await prismaClient.$queryRaw('SELECT 1');
            return true;
        }
        catch (error) {
            console.error('Database health check failed:', error);
            return false;
        }
    }
}
//# sourceMappingURL=base-provider.js.map