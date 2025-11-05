import { BaseDatabaseProvider } from './base-provider';
export class SupabaseProvider extends BaseDatabaseProvider {
    static instance;
    constructor(databaseUrl) {
        super(databaseUrl, 'supabase');
    }
    static getInstance(databaseUrl) {
        if (!SupabaseProvider.instance) {
            SupabaseProvider.instance = new SupabaseProvider(databaseUrl);
        }
        return SupabaseProvider.instance;
    }
    async getClient() {
        if (!this.client) {
            this.client = await this.generateClient(this.databaseUrl);
            await this.client.$connect();
        }
        return this.client;
    }
    async disconnect() {
        if (this.client) {
            await this.client.$disconnect();
            this.client = null;
        }
    }
}
//# sourceMappingURL=supabase-provider.js.map