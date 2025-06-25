import { BaseDatabaseProvider } from './base-provider';
export class CosmosDbProvider extends BaseDatabaseProvider {
    static instance;
    constructor(databaseUrl) {
        super(databaseUrl, 'cosmosdb');
    }
    static getInstance(databaseUrl) {
        if (!CosmosDbProvider.instance) {
            CosmosDbProvider.instance = new CosmosDbProvider(databaseUrl);
        }
        return CosmosDbProvider.instance;
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
//# sourceMappingURL=cosmosdb-provider.js.map