import { BaseDatabaseProvider, ProviderType, PrismaClient } from './base-provider';

export class CosmosDBProvider extends BaseDatabaseProvider<PrismaClient> {
  private static instance: CosmosDBProvider;

  private constructor(databaseUrl: string) {
    super(databaseUrl, 'cosmosdb' as ProviderType);
  }

  public static getInstance(databaseUrl: string): CosmosDBProvider {
    if (!CosmosDBProvider.instance) {
      CosmosDBProvider.instance = new CosmosDBProvider(databaseUrl);
    }
    return CosmosDBProvider.instance;
  }

  async getClient(): Promise<PrismaClient> {
    if (!this.client) {
      this.client = await this.generateClient(this.databaseUrl);
      await this.client.$connect();
    }
    return this.client;
  }

  async disconnect(): Promise<void> {
    if (this.client) {
      await this.client.$disconnect();
      this.client = null;
    }
  }

  /**
   * Override the client path for CosmosDB
   */
  getClientPath(): string {
    return '@prisma/client-cosmosdb';
  }

  /**
   * CosmosDB-specific health check
   */
  async checkHealth(): Promise<boolean> {
    try {
      const client = await this.getClient();
      // CosmosDB uses a different query syntax
      await client.$queryRaw`SELECT VALUE 1`;
      return true;
    } catch (error) {
      console.error('CosmosDB health check failed:', error);
      return false;
    }
  }

  /**
   * Get CosmosDB-specific configuration
   */
  getCosmosConfig() {
    return {
      endpoint: process.env.COSMOS_ENDPOINT,
      key: process.env.COSMOS_KEY,
      database: process.env.COSMOS_DATABASE,
      container: process.env.COSMOS_CONTAINER,
    };
  }
}
