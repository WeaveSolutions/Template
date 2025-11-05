import { BaseDatabaseProvider, type MiddlewareParams, ProviderType, PrismaClient } from './base-provider';

type PostgresFeatures = {
  version: string;
  extensions: string[];
};

type QueryClient = {
  $queryRawUnsafe: <T = any>(query: string, ...values: any[]) => Promise<T>;
  $queryRaw: <T = any>(query: TemplateStringsArray | string, ...values: any[]) => Promise<T>;
};

// Type for raw query results
type QueryResult<T = unknown> = {
  rows: T[];
  rowCount: number;
  command: string;
  oid: number;
  fields: Array<{
    name: string;
    tableID: number;
    columnID: number;
    dataTypeID: number;
    dataTypeSize: number;
    dataTypeModifier: number;
    format: string;
  }>;
};

/**
 * PostgreSQL database provider implementation
 */
export class PostgresProvider extends BaseDatabaseProvider<PrismaClient> {
  private static instance: PostgresProvider;

  /**
   * Private constructor to enforce singleton pattern per database URL
   */
  private constructor(databaseUrl: string) {
    super(databaseUrl, 'postgres' as ProviderType);
  }

  /**
   * Get or create a PostgresProvider instance for the given database URL
   * @param databaseUrl The database connection URL
   * @returns PostgresProvider instance
   */
  public static getInstance(databaseUrl: string): PostgresProvider {
    if (!PostgresProvider.instance) {
      PostgresProvider.instance = new PostgresProvider(databaseUrl);
    }
    return PostgresProvider.instance;
  }

  /**
   * Get the provider type
   */
  getProviderType(): ProviderType {
    return 'postgres';
  }

  /**
   * Generate a new Prisma Client instance with PostgreSQL-specific configuration
   */
  protected async generateClient(databaseUrl: string): Promise<PrismaClient> {
    try {
      const client = await super.generateClient(databaseUrl);
      
      // PostgreSQL-specific configuration can be added here if needed
      // without using middleware that causes type conflicts
      
      return client;
    } catch (error) {
      console.error('Failed to initialize PostgreSQL client:', error);
      throw new Error('Failed to initialize PostgreSQL client');
    }
  }

  /**
   * Get the Prisma Client instance with PostgreSQL-specific configuration
   */
  override async getClient(): Promise<PrismaClient> {
    if (!this.client) {
      this.client = await this.generateClient(this.databaseUrl);
      await this.client.$connect();
    }
    
    return this.client;
  }

  /**
   * Execute a raw SQL query
   * @param query SQL query string
   * @param values Optional parameter values
   */
  /**
   * Execute a raw SQL query with type safety
   * @param query SQL query string
   * @param values Optional parameter values
   * @returns Promise with query results
   */
  async $queryRaw<T = QueryResult>(query: string, values?: unknown[]): Promise<T> {
    const client = await this.getClient();
    const queryClient = client as unknown as QueryClient;
    
    if (values && values.length > 0) {
      return queryClient.$queryRawUnsafe(query, ...values);
    }
    return queryClient.$queryRawUnsafe(query);
  }

  /**
   * Check if PostgreSQL-specific features are available
   */
  async checkPostgresFeatures(): Promise<PostgresFeatures> {
    try {
      const client = await this.getClient();
      const queryClient = client as unknown as QueryClient;
      
      // Check PostgreSQL version
      const versionResult = await queryClient.$queryRawUnsafe<Array<{ version: string }>>(
        'SELECT version() as version'
      );
      
      // Check available extensions
      const extensionsResult = await queryClient.$queryRawUnsafe<Array<{ name: string }>>(
        'SELECT name FROM pg_available_extensions WHERE installed_version IS NOT NULL'
      );
      
      return {
        version: versionResult?.[0]?.version || 'unknown',
        extensions: extensionsResult?.map(ext => ext.name) || []
      };
    } catch (error) {
      console.error('Error checking PostgreSQL features:', error);
      return {
        version: 'unknown',
        extensions: []
      };
    }
  }

  /**
   * Disconnect the client
   */
  async disconnect(): Promise<void> {
    if (this.client) {
      await this.client.$disconnect();
      this.client = null;
    }
  }

  /**
   * Clean up all PostgresProvider instances
   */
  static async disconnectAll(): Promise<void> {
    if (PostgresProvider.instance) {
      await PostgresProvider.instance.disconnect();
      PostgresProvider.instance = null as any;
    }
  }

  /**
   * Override the client path for PostgreSQL
   */
  getClientPath(): string {
    return '@prisma/client-postgres';
  }

  /**
   * PostgreSQL-specific configuration
   */
  getPostgresConfig() {
    const url = new URL(this.databaseUrl);
    return {
      host: url.hostname,
      port: url.port || '5432',
      database: url.pathname.slice(1),
      user: url.username,
      ssl: url.searchParams.get('sslmode') !== 'disable',
    };
  }
}
