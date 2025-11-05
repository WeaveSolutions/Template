import { PrismaClient } from '@prisma/client';
import { BaseDatabaseProvider } from './base-provider';
export declare class CosmosDbProvider extends BaseDatabaseProvider {
    private static instance;
    private constructor();
    static getInstance(databaseUrl: string): CosmosDbProvider;
    getClient(): Promise<PrismaClient>;
    disconnect(): Promise<void>;
}
//# sourceMappingURL=cosmosdb-provider.d.ts.map