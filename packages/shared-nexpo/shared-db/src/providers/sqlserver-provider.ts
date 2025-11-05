import { BaseDatabaseProvider, ProviderType, PrismaClient } from './base-provider';

export class SqlServerProvider extends BaseDatabaseProvider<PrismaClient> {
  private static instance: SqlServerProvider;

  private constructor(databaseUrl: string) {
    super(databaseUrl, 'sqlserver' as ProviderType);
  }

  public static getInstance(databaseUrl: string): SqlServerProvider {
    if (!SqlServerProvider.instance) {
      SqlServerProvider.instance = new SqlServerProvider(databaseUrl);
    }
    return SqlServerProvider.instance;
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
   * Override the client path for SQL Server
   */
  getClientPath(): string {
    return '@prisma/client-sqlserver';
  }

  /**
   * SQL Server-specific configuration
   */
  getSqlServerConfig() {
    const url = new URL(this.databaseUrl);
    return {
      server: url.hostname,
      port: url.port || '1433',
      database: url.pathname.slice(1),
      user: url.username,
      trustServerCertificate: url.searchParams.get('trustServerCertificate') === 'true',
    };
  }
}
