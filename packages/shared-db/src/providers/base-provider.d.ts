type PrismaClientBase = {
    $connect(): Promise<void>;
    $disconnect(): Promise<void>;
    $transaction<T>(fn: (tx: any) => Promise<T>): Promise<T>;
    $queryRaw<T = any>(query: string | TemplateStringsArray, ...values: any[]): Promise<T>;
    $queryRawUnsafe<T = any>(query: string, ...values: any[]): Promise<T>;
    [key: string]: any;
};
type BasePrismaClient = PrismaClientBase & {
    $use: (middleware: any) => void;
};
type PrismaClient = BasePrismaClient & {
    [key: string]: any;
};
export type { PrismaClientBase };
export type MiddlewareParams = {
    model?: string;
    action: string;
    args: any;
    dataPath: string[];
    runInTransaction: boolean;
};
import type { Prisma as PrismaNamespace, PrismaClient as PrismaClientType } from '@prisma/client';
export type { PrismaNamespace as Prisma, PrismaClientType as PrismaClient };
export type ProviderType = 'postgres' | 'mongodb' | 'supabase' | 'cosmosdb' | 'sqlserver' | 'ibmcloud';
export interface DatabaseProvider {
    getClient(): Promise<PrismaClient>;
    disconnect(): Promise<void>;
    getDatabaseUrl(): string;
    getClientPath(): string;
}
export declare abstract class BaseDatabaseProvider<T extends PrismaClient = PrismaClient> implements DatabaseProvider {
    protected client: T | null;
    protected databaseUrl: string;
    protected providerName: ProviderType;
    protected isConnected: boolean;
    constructor(databaseUrl: string, providerName: ProviderType);
    /**
     * Get the database URL
     */
    getDatabaseUrl(): string;
    /**
     * Get the path to the Prisma Client for this provider
     */
    getClientPath(): string;
    /**
     * Get the Prisma Client instance, creating it if it doesn't exist
     */
    getClient(): Promise<T>;
    /**
     * Disconnect from the database
     */
    disconnect(): Promise<void>;
    /**
     * Generate a new Prisma Client instance with the correct provider configuration
     */
    protected generateClient(databaseUrl: string): Promise<T>;
    /**
     * Get the appropriate log levels based on environment
     */
    protected getLogLevels(): string[];
    /**
     * Execute a transaction with the current client
     */
    $transaction<T>(fn: (tx: any) => Promise<T>): Promise<T>;
    /**
     * Check if the database connection is healthy
     */
    checkHealth(): Promise<boolean>;
}
//# sourceMappingURL=base-provider.d.ts.map