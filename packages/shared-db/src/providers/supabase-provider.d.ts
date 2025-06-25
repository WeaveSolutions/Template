import { PrismaClient } from '@prisma/client';
import { BaseDatabaseProvider } from './base-provider';
export declare class SupabaseProvider extends BaseDatabaseProvider {
    private static instance;
    private constructor();
    static getInstance(databaseUrl: string): SupabaseProvider;
    getClient(): Promise<PrismaClient>;
    disconnect(): Promise<void>;
}
//# sourceMappingURL=supabase-provider.d.ts.map