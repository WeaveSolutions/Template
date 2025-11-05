import { BaseDatabaseProvider } from './base-provider';
import type { ProviderType, PrismaClient } from './base-provider';
type PostgresFeatures = {
    version: string;
    extensions: string[];
};
type QueryResult<T = unknown> = {
    rows: T[];
    rowCount: number;
    command: string;
    oid: number;
    fields: Array<{
        name: string;
        tableID: number;
        columnID: number;
        dataTypeID: number;
        dataTypeSize: number;
        dataTypeModifier: number;
        format: string;
    }>;
};
/**
 * PostgreSQL database provider implementation
 */
export declare class PostgresProvider extends BaseDatabaseProvider<PrismaClient> {
    private static instances;
    /**
     * Private constructor to enforce singleton pattern per database URL
     */
    private constructor();
    /**
     * Get or create a PostgresProvider instance for the given database URL
     * @param databaseUrl The database connection URL
     * @returns PostgresProvider instance
     */
    static getInstance(databaseUrl: string): PostgresProvider;
    /**
     * Get the provider type
     */
    getProviderType(): ProviderType;
    /**
     * Get the Prisma Client instance with PostgreSQL-specific configuration
     */
    getClient(): Promise<PrismaClient>;
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
    $queryRaw<T = QueryResult>(query: string, values?: unknown[]): Promise<T>;
    /**
     * Check if PostgreSQL-specific features are available
     */
    checkPostgresFeatures(): Promise<PostgresFeatures>;
    /**
     * Clean up all PostgresProvider instances
     */
    static disconnectAll(): Promise<void>;
}
export {};
//# sourceMappingURL=postgres-provider.d.ts.map