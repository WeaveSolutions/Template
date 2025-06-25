import { PrismaClient } from '@prisma/client';
import { BaseDatabaseProvider } from './base-provider';
export declare class SqlServerProvider extends BaseDatabaseProvider {
    private static instance;
    private constructor();
    static getInstance(databaseUrl: string): SqlServerProvider;
    getClient(): Promise<PrismaClient>;
    disconnect(): Promise<void>;
}
//# sourceMappingURL=sqlserver-provider.d.ts.map