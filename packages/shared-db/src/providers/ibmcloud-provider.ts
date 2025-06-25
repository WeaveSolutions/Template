import { BaseDatabaseProvider, type MiddlewareParams, ProviderType, PrismaClient } from './base-provider';

type IBMCloudFeatures = {
  version: string;
  serviceType: string;
  region: string;
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
  fields: Array<{
    name: string;
    dataType: string;
  }>;
};

/**
 * IBM Cloud DB2 database provider implementation
 */
export class IBMCloudProvider extends BaseDatabaseProvider<PrismaClient> {
  private static instance: IBMCloudProvider;
  private apiKey?: string;
  private resourceInstanceId?: string;

  /**
   * Private constructor to enforce singleton pattern per database URL
   */
  private constructor(databaseUrl: string) {
    super(databaseUrl, 'ibmcloud' as ProviderType);
    
    // Get IBM Cloud-specific configuration from environment variables
    this.apiKey = process.env.IBM_APIKEY;
    this.resourceInstanceId = process.env.IBM_RESOURCE_INSTANCE_ID;
    
    if (!this.apiKey) {
      console.warn('IBM_APIKEY environment variable not set. Some IBM Cloud features may be limited.');
    }
    
    if (!this.resourceInstanceId) {
      console.warn('IBM_RESOURCE_INSTANCE_ID environment variable not set. Some IBM Cloud features may be limited.');
    }
  }

  /**
   * Get or create an IBMCloudProvider instance for the given database URL
   * @param databaseUrl The database connection URL
   * @returns IBMCloudProvider instance
   */
  public static getInstance(databaseUrl: string): IBMCloudProvider {
    if (!IBMCloudProvider.instance) {
      IBMCloudProvider.instance = new IBMCloudProvider(databaseUrl);
    }
    return IBMCloudProvider.instance;
  }

  /**
   * Get the provider type
   */
  getProviderType(): ProviderType {
    return 'ibmcloud';
  }

  /**
   * Generate a new Prisma Client instance with IBM Cloud-specific configuration
   */
  protected async generateClient(databaseUrl: string): Promise<PrismaClient> {
    try {
      const client = await super.generateClient(databaseUrl);
      
      // IBM Cloud-specific configuration can be added here if needed
      // without using middleware that causes type conflicts
      
      return client;
    } catch (error) {
      console.error('Failed to initialize IBM Cloud client:', error);
      throw new Error('Failed to initialize IBM Cloud client');
    }
  }

  /**
   * Get the Prisma Client instance with IBM Cloud DB2-specific configuration
   */
  override async getClient(): Promise<PrismaClient> {
    if (!this.client) {
      // Check if the connection URL is properly formatted
      if (!this.databaseUrl || !this.databaseUrl.startsWith('postgresql://')) {
        throw new Error('Invalid IBM Cloud DB2 connection URL. The URL should be in PostgreSQL format as Prisma uses PostgreSQL connector for DB2.');
      }
      
      this.client = await this.generateClient(this.databaseUrl);
      
      if (this.client) {
        await this.client.$connect();
      }
    }
    
    if (!this.client) {
      throw new Error('Failed to initialize IBM Cloud DB2 client');
    }
    
    return this.client;
  }

  /**
   * Disconnect the Prisma Client instance
   */
  async disconnect(): Promise<void> {
    if (this.client) {
      await this.client.$disconnect();
      this.client = null;
    }
  }

  /**
   * Override the client path for IBM Cloud
   */
  getClientPath(): string {
    return '@prisma/client-ibmcloud';
  }

  /**
   * IBM Cloud-specific configuration
   */
  getIBMCloudConfig() {
    return {
      apiKey: process.env.IBM_CLOUD_API_KEY,
      region: process.env.IBM_CLOUD_REGION || 'us-south',
      serviceId: process.env.IBM_CLOUD_SERVICE_ID,
    };
  }

  /**
   * Get IBM Cloud DB2 database information
   */
  async getDatabaseInfo(): Promise<IBMCloudFeatures | null> {
    try {
      if (!this.client) {
        await this.getClient();
      }
      
      // Using the client to query DB2 version and other information
      const versionInfo = await (this.client as unknown as QueryClient).$queryRaw<Array<{ version: string }>>`
        SELECT version() as version
      `;
      
      return {
        version: versionInfo[0]?.version || 'Unknown',
        serviceType: 'Db2 on Cloud',
        region: process.env.IBM_CLOUD_REGION || 'Unknown'
      };
    } catch (error) {
      console.error('Error getting IBM Cloud DB2 database information:', error);
      return null;
    }
  }

  /**
   * Execute a raw SQL query against the IBM Cloud DB2 database
   */
  async executeRawQuery<T = unknown>(query: string, ...values: any[]): Promise<QueryResult<T>> {
    try {
      if (!this.client) {
        await this.getClient();
      }
      
      const result = await (this.client as unknown as QueryClient).$queryRawUnsafe<T[]>(query, ...values);
      
      return {
        rows: Array.isArray(result) ? result : [],
        rowCount: Array.isArray(result) ? result.length : 0,
        command: query.split(' ')[0],
        fields: []
      };
    } catch (error) {
      console.error('Error executing raw query against IBM Cloud DB2:', error);
      throw error;
    }
  }
}
