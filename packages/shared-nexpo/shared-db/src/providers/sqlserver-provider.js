import { BaseDatabaseProvider } from './base-provider';
export class SqlServerProvider extends BaseDatabaseProvider {
    static instance;
    constructor(databaseUrl) {
        super(databaseUrl, 'sqlserver');
    }
    static getInstance(databaseUrl) {
        if (!SqlServerProvider.instance) {
            SqlServerProvider.instance = new SqlServerProvider(databaseUrl);
        }
        return SqlServerProvider.instance;
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
//# sourceMappingURL=sqlserver-provider.js.map