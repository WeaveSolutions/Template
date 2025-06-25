import { BaseDatabaseProvider } from './base-provider';
export class MongoDbProvider extends BaseDatabaseProvider {
    static instance;
    constructor(databaseUrl) {
        super(databaseUrl, 'mongodb');
    }
    static getInstance(databaseUrl) {
        if (!MongoDbProvider.instance) {
            MongoDbProvider.instance = new MongoDbProvider(databaseUrl);
        }
        return MongoDbProvider.instance;
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
//# sourceMappingURL=mongodb-provider.js.map