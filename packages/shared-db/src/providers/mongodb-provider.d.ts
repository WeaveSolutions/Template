import { PrismaClient } from '@prisma/client';
import { BaseDatabaseProvider } from './base-provider';
export declare class MongoDbProvider extends BaseDatabaseProvider {
    private static instance;
    private constructor();
    static getInstance(databaseUrl: string): MongoDbProvider;
    getClient(): Promise<PrismaClient>;
    disconnect(): Promise<void>;
}
//# sourceMappingURL=mongodb-provider.d.ts.map