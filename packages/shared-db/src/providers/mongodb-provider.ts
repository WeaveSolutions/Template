import { BaseDatabaseProvider, ProviderType, PrismaClient } from './base-provider';

export class MongoDBProvider extends BaseDatabaseProvider<PrismaClient> {
  private static instance: MongoDBProvider;

  private constructor(databaseUrl: string) {
    super(databaseUrl, 'mongodb' as ProviderType);
  }

  public static getInstance(databaseUrl: string): MongoDBProvider {
    if (!MongoDBProvider.instance) {
      MongoDBProvider.instance = new MongoDBProvider(databaseUrl);
    }
    return MongoDBProvider.instance;
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
   * Override the client path for MongoDB
   */
  getClientPath(): string {
    return '@prisma/client-mongodb';
  }

  /**
   * MongoDB-specific health check
   */
  async checkHealth(): Promise<boolean> {
    try {
      const client = await this.getClient();
      // MongoDB uses a different query syntax
      await (client as any).$runCommandRaw({ ping: 1 });
      return true;
    } catch (error) {
      console.error('MongoDB health check failed:', error);
      return false;
    }
  }

  /**
   * Get MongoDB-specific configuration
   */
  getMongoConfig() {
    const url = new URL(this.databaseUrl);
    return {
      host: url.hostname,
      port: url.port || '27017',
      database: url.pathname.slice(1),
      username: url.username,
      authSource: url.searchParams.get('authSource') || 'admin',
    };
  }
}
