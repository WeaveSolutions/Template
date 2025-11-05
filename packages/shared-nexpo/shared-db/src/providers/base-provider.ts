// Import types from Prisma
import type { PrismaClient } from '@prisma/client';

// Export the PrismaClient type
export type { PrismaClient };

// Type for Prisma middleware params
export type MiddlewareParams = {
  model?: string;
  action: string;
  args: any;
  dataPath: string[];
  runInTransaction: boolean;
};

export type ProviderType = 'postgres' | 'mongodb' | 'supabase' | 'cosmosdb' | 'sqlserver' | 'ibmcloud';

export interface DatabaseProvider {
  getClient(): Promise<PrismaClient>;
  disconnect(): Promise<void>;
  getDatabaseUrl(): string;
  getClientPath(): string;
}

export abstract class BaseDatabaseProvider<T extends PrismaClient = PrismaClient> implements DatabaseProvider {
  protected client: T | null = null;
  protected databaseUrl: string;
  protected providerName: ProviderType;
  protected isConnected: boolean = false;

  constructor(databaseUrl: string, providerName: ProviderType) {
    this.databaseUrl = databaseUrl;
    this.providerName = providerName;
  }

  /**
   * Get the database URL
   */
  getDatabaseUrl(): string {
    return this.databaseUrl;
  }

  /**
   * Get the path to the Prisma Client for this provider
   */
  getClientPath(): string {
    return `@prisma/client-${this.providerName}`;
  }

  /**
   * Get the Prisma Client instance, creating it if it doesn't exist
   */
  async getClient(): Promise<T> {
    if (!this.client) {
      this.client = await this.generateClient(this.databaseUrl);
    }
    
    if (!this.isConnected) {
      await this.client.$connect();
      this.isConnected = true;
    }
    
    return this.client;
  }

  /**
   * Disconnect from the database
   */
  async disconnect(): Promise<void> {
    if (this.client && this.isConnected) {
      await this.client.$disconnect();
      this.isConnected = false;
    }
  }

  /**
   * Generate a new Prisma Client instance with the correct provider configuration
   */
  protected async generateClient(databaseUrl: string): Promise<T> {
    try {
      // Dynamically import the generated Prisma client
      const prismaModule = await import(this.getClientPath());
      const { PrismaClient } = prismaModule;
      
      // Create a new Prisma client instance with the provided database URL
      const client = new PrismaClient({
        datasources: {
          db: {
            url: databaseUrl,
          },
        },
        log: this.getLogLevels(),
      }) as T;
      
      return client;
    } catch (error) {
      console.error(`Failed to initialize ${this.providerName} client:`, error);
      throw new Error(`Failed to initialize ${this.providerName} client`);
    }
  }

  /**
   * Get the appropriate log levels based on environment
   */
  protected getLogLevels() {
    if (process.env.NODE_ENV === 'production') {
      return ['error'];
    }
    return ['query', 'error', 'warn'];
  }

  /**
   * Execute a transaction with the current client
   */
  async $transaction<T>(fn: (tx: any) => Promise<T>): Promise<T> {
    const client = await this.getClient();
    return client.$transaction(fn);
  }

  /**
   * Check if the database connection is healthy
   */
  async checkHealth(): Promise<boolean> {
    try {
      const client = await this.getClient();
      await client.$queryRaw`SELECT 1`;
      return true;
    } catch (error) {
      console.error('Database health check failed:', error);
      return false;
    }
  }
}
