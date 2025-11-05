import { z } from 'zod';
import { PostgresProvider } from './providers/postgres-provider';
import { MongoDbProvider } from './providers/mongodb-provider';
import { SupabaseProvider } from './providers/supabase-provider';
import { CosmosDbProvider } from './providers/cosmosdb-provider';
import { SqlServerProvider } from './providers/sqlserver-provider';
import { IBMCloudProvider } from './providers/ibmcloud-provider';
import { DatabaseProvider } from './providers/base-provider';

export type ProviderType = 'postgres' | 'mongodb' | 'supabase' | 'cosmosdb' | 'sqlserver' | 'ibmcloud';

// Environment variable validation
const envSchema = z.object({
  // Feature flags
  ENABLE_POSTGRES: z.string().default('false').transform((val: string) => val === 'true'),
  ENABLE_MONGODB: z.string().default('false').transform((val: string) => val === 'true'),
  ENABLE_SUPABASE: z.string().default('false').transform((val: string) => val === 'true'),
  ENABLE_COSMOSDB: z.string().default('false').transform((val: string) => val === 'true'),
  ENABLE_SQLSERVER: z.string().default('false').transform((val: string) => val === 'true'),
  ENABLE_IBM: z.string().default('false').transform((val: string) => val === 'true'),
  
  // Default provider
  DEFAULT_DATABASE_PROVIDER: z
    .enum(['postgres', 'mongodb', 'supabase', 'cosmosdb', 'sqlserver', 'ibmcloud'] as const)
    .default('postgres'),
    
  // PostgreSQL
  POSTGRES_PRISMA_URL: z.string().url().optional(),
  POSTGRES_URL_NON_POOLING: z.string().url().optional(),
  
  // MongoDB
  MONGODB_URI: z.string().url().optional(),
  
  // Supabase
  SUPABASE_DATABASE_URL: z.string().url().optional(),
  SUPABASE_DIRECT_URL: z.string().url().optional(),
  
  // Azure
  COSMOSDB_MONGODB_URI: z.string().url().optional(),
  SQLSERVER_URL: z.string().url().optional(),
  
  // IBM Cloud
  DB2_URL: z.string().url().optional(),
  IBM_APIKEY: z.string().optional(),
  IBM_RESOURCE_INSTANCE_ID: z.string().optional(),
  
  // Logging
  DATABASE_LOG_LEVEL: z
    .enum(['error', 'warn', 'info', 'debug'] as const)
    .default('info'),
  PRISMA_LOGGING: z
    .string()
    .default('false')
    .transform((val: string) => val === 'true'),
    
  NODE_ENV: z.enum(['development', 'test', 'production']).default('development'),
});

type EnvVars = z.infer<typeof envSchema>;

// Define the provider configuration interface
interface ProviderConfig {
  envKey: string;
  urlKey: string;
  directUrlKey?: string;
  defaultUrl: string;
  ProviderClass: any; // Ideally, this would be a more specific type
}

// Map of provider types to their configuration
const PROVIDER_CONFIG: Record<ProviderType, ProviderConfig> = {
  postgres: {
    envKey: 'ENABLE_POSTGRES',
    urlKey: 'POSTGRES_PRISMA_URL',
    directUrlKey: 'POSTGRES_URL_NON_POOLING',
    defaultUrl: 'postgresql://postgres:postgres@localhost:5432/postgres',
    ProviderClass: PostgresProvider,
  },
  mongodb: {
    envKey: 'ENABLE_MONGODB',
    urlKey: 'MONGODB_URI',
    defaultUrl: 'mongodb://localhost:27017/app',
    ProviderClass: MongoDbProvider,
    directUrlKey: undefined,
  },
  supabase: {
    envKey: 'ENABLE_SUPABASE',
    urlKey: 'SUPABASE_DATABASE_URL',
    directUrlKey: 'SUPABASE_DIRECT_URL',
    defaultUrl: 'postgresql://postgres:postgres@localhost:5432/supabase',
    ProviderClass: SupabaseProvider,
  },
  cosmosdb: {
    envKey: 'ENABLE_COSMOSDB',
    urlKey: 'COSMOSDB_MONGODB_URI',
    defaultUrl: 'mongodb://localhost:10255/app?ssl=true&replicaSet=globaldb',
    ProviderClass: CosmosDbProvider,
    directUrlKey: undefined,
  },
  sqlserver: {
    envKey: 'ENABLE_SQLSERVER',
    urlKey: 'SQLSERVER_URL',
    defaultUrl: 'sqlserver://localhost:1433;database=app;user=sa;password=yourStrong(!)Password;encrypt=true;trustServerCertificate=true',
    ProviderClass: SqlServerProvider,
    directUrlKey: undefined,
  },
  ibmcloud: {
    envKey: 'ENABLE_IBM',
    urlKey: 'DB2_URL',
    defaultUrl: 'postgresql://user:password@localhost:50000/ibmdb',
    ProviderClass: IBMCloudProvider,
    directUrlKey: undefined,
  },
};

export class DatabaseFactory {
  private static instances: Map<ProviderType, DatabaseProvider> = new Map();
  private static env: EnvVars;
  private static initialized = false;

  private constructor() {}

  private static initialize(): void {
    if (this.initialized) return;
    
    // Parse and validate environment variables
    this.env = envSchema.parse(process.env);
    
    // Configure Prisma logging
    if (this.env.PRISMA_LOGGING) {
      process.env.PRISMA_LOG_QUERIES = 'true';
      process.env.PRISMA_LOG_LEVEL = this.env.DATABASE_LOG_LEVEL;
    }
    
    this.initialized = true;
  }

  /**
   * Get a database provider by type
   * @param providerType The type of database provider to get
   * @returns The database provider instance
   * @throws Error if the provider is not enabled or not properly configured
   */
  public static getProvider(providerType: ProviderType): DatabaseProvider {
    this.initialize();
    
    // Return existing instance if available
    if (this.instances.has(providerType)) {
      return this.instances.get(providerType)!;
    }

    // Get provider configuration
    const config = PROVIDER_CONFIG[providerType];
    
    // Check if provider is enabled
    if (!this.env[config.envKey as keyof EnvVars]) {
      throw new Error(
        `Database provider '${providerType}' is not enabled. Set ENABLE_${providerType.toUpperCase()}=true in your environment.`
      );
    }
    
    // Get the database URL from environment or use default
    const databaseUrl = this.env[config.urlKey as keyof EnvVars] || config.defaultUrl;
    
    if (!databaseUrl) {
      throw new Error(
        `Missing required environment variable for ${providerType}: ${config.urlKey}`
      );
    }
    
    // Create new provider instance
    let provider: DatabaseProvider;
    
    try {
      // Use the appropriate constructor based on provider type
      if ('getInstance' in config.ProviderClass) {
        // Singleton pattern providers (like PostgresProvider)
        provider = (config.ProviderClass as any).getInstance(databaseUrl);
      } else {
        // Regular providers
        provider = new (config.ProviderClass as any)(databaseUrl);
      }
      
      // Cache the instance
      this.instances.set(providerType, provider);
      return provider;
      
    } catch (error) {
      throw new Error(
        `Failed to initialize ${providerType} provider: ${error instanceof Error ? error.message : String(error)}`
      );
    }
  }

  /**
   * Disconnect all active database connections
   */
  public static async disconnectAll(): Promise<void> {
    try {
      await Promise.all(
        Array.from(this.instances.values()).map(provider => 
          provider.disconnect().catch(error => {
            console.error('Error disconnecting database provider:', error);
          })
        )
      );
    } finally {
      this.instances.clear();
    }
  }

  /**
   * Get the default database provider based on environment configuration
   * @returns The default database provider instance
   * @throws Error if no providers are enabled or default provider is not properly configured
   */
  public static getDefaultProvider(): DatabaseProvider {
    this.initialize();
    
    // Get the default provider from environment
    const defaultProvider = this.env.DEFAULT_DATABASE_PROVIDER as ProviderType;
    
    try {
      // First try the default provider
      return this.getProvider(defaultProvider);
    } catch (error) {
      // If default provider fails, try to find any enabled provider
      for (const [providerType, config] of Object.entries(PROVIDER_CONFIG) as [ProviderType, any][]) {
        if (this.env[config.envKey as keyof EnvVars]) {
          try {
            return this.getProvider(providerType);
          } catch (e) {
            // Continue to next provider
            console.warn(`Skipping ${providerType} provider:`, e instanceof Error ? e.message : String(e));
          }
        }
      }
      
      // No enabled providers found
      throw new Error(
        `No database providers are properly configured. Please enable and configure at least one provider.\n` +
        `Enabled providers: ${Object.entries(PROVIDER_CONFIG)
          .filter(([_, config]) => this.env[config.envKey as keyof EnvVars])
          .map(([type]) => type)
          .join(', ') || 'None'}`
      );
    }
  }
  
  /**
   * Get all enabled database providers
   * @returns A map of enabled provider types to their instances
   */
  public static getEnabledProviders(): Map<ProviderType, DatabaseProvider> {
    this.initialize();
    
    const enabledProviders = new Map<ProviderType, DatabaseProvider>();
    
    for (const [providerType, config] of Object.entries(PROVIDER_CONFIG) as [ProviderType, any][]) {
      if (this.env[config.envKey as keyof EnvVars]) {
        try {
          const provider = this.getProvider(providerType);
          enabledProviders.set(providerType, provider);
        } catch (error) {
          console.warn(`Failed to initialize ${providerType} provider:`, error);
        }
      }
    }
    
    return enabledProviders;
  }
}
