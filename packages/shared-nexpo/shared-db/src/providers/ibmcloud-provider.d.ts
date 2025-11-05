import { BaseDatabaseProvider } from './base-provider';
import type { ProviderType, PrismaClient } from './base-provider';
type IBMCloudFeatures = {
    version: string;
    serviceType: string;
    region: string;
};
type QueryResult<T = unknown> = {
    rows: T[];
    rowCount: number;
    command: string;
    fields: Array<{
        name: string;
        dataType: string;
    }>;
};
/**
 * IBM Cloud DB2 database provider implementation
 */
export declare class IBMCloudProvider extends BaseDatabaseProvider<PrismaClient> {
    private static instances;
    private apiKey?;
    private resourceInstanceId?;
    /**
     * Private constructor to enforce singleton pattern per database URL
     */
    private constructor();
    /**
     * Get or create an IBMCloudProvider instance for the given database URL
     * @param databaseUrl The database connection URL
     * @returns IBMCloudProvider instance
     */
    static getInstance(databaseUrl: string): IBMCloudProvider;
    /**
     * Get the provider type
     */
    getProviderType(): ProviderType;
    /**
     * Get the Prisma Client instance with IBM Cloud DB2-specific configuration
     */
    getClient(): Promise<PrismaClient>;
    /**
     * Get IBM Cloud DB2 database information
     */
    getDatabaseInfo(): Promise<IBMCloudFeatures | null>;
    /**
     * Execute a raw SQL query against the IBM Cloud DB2 database
     */
    executeRawQuery<T = unknown>(query: string, ...values: any[]): Promise<QueryResult<T>>;
}
export {};
//# sourceMappingURL=ibmcloud-provider.d.ts.map