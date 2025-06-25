import { PrismaClient as PrismaClientType } from '@prisma/client';
import { PrismaClient } from '@prisma/client';
import { DatabaseFactory, type ProviderType } from './db-factory';
import type { DatabaseProvider } from './providers/base-provider';

// Add global declaration for prisma
declare global {
  var prisma: PrismaClient | any;
}

/**
 * DatabaseClient provides a singleton interface to the database with support for
 * multiple database providers using feature flags.
 */
class DatabaseClient {
  private static instance: DatabaseClient;
  private provider: DatabaseProvider | null = null;
  private prismaClient: PrismaClient | null = null; 
  private providerType: ProviderType | null = null;
  private isInitializing = false;
  private initPromise: Promise<void> | null = null;

  private constructor() {
    // Private constructor to enforce singleton pattern
  }

  /**
   * Get the singleton instance of DatabaseClient
   */
  public static getInstance(): DatabaseClient {
    if (!DatabaseClient.instance) {
      DatabaseClient.instance = new DatabaseClient();
    }
    return DatabaseClient.instance;
  }

  /**
   * Initialize the database client with the specified provider or use the default
   * @param providerType Optional provider type to use instead of the default
   */
  private async initialize(providerType?: ProviderType): Promise<void> {
    if (this.provider && (!providerType || this.providerType === providerType)) {
      return; // Already initialized with the requested provider
    }

    if (this.isInitializing && this.initPromise) {
      return this.initPromise; // Return existing init promise
    }

    this.isInitializing = true;
    this.initPromise = (async () => {
      try {
        // Disconnect from current provider if any
        if (this.prismaClient) {
          await this.disconnect();
        }

        // Get the specified provider or default
        this.provider = providerType 
          ? DatabaseFactory.getProvider(providerType)
          : DatabaseFactory.getDefaultProvider();
        
        this.providerType = providerType || null;
        
        // Initialize the Prisma client
        this.prismaClient = await this.provider.getClient();
        
        // Store in global for development to prevent too many connections
        if (process.env.NODE_ENV !== 'production') {
          // Use globalThis instead of global for better TypeScript compatibility
          (globalThis as any).prisma = this.prismaClient;
        }
        
        console.log(`[Database] Connected to ${this.getProviderType()} database`);
      } catch (error) {
        console.error('[Database] Failed to initialize:', error);
        throw new Error(
          `Failed to initialize database client: ${error instanceof Error ? error.message : String(error)}`
        );
      } finally {
        this.isInitializing = false;
        this.initPromise = null;
      }
    })();

    return this.initPromise;
  }

  /**
   * Get the Prisma client, initializing it if necessary
   * @param providerType Optional provider type to use
   */
  public async getClient(providerType?: ProviderType): Promise<PrismaClient> {
    if (providerType && this.providerType !== providerType) {
      await this.initialize(providerType);
    } else if (!this.prismaClient) {
      await this.initialize();
    }
    
    if (!this.prismaClient) {
      throw new Error('Database client not initialized');
    }
    
    return this.prismaClient;
  }

  /**
   * Disconnect from the database
   */
  public async disconnect(): Promise<void> {
    if (this.prismaClient) {
      try {
        await this.prismaClient.$disconnect();
        console.log(`[Database] Disconnected from ${this.getProviderType()} database`);
      } catch (error) {
        console.error('[Database] Error disconnecting:', error);
        throw error;
      } finally {
        this.prismaClient = null;
        this.provider = null;
        this.providerType = null;
        
        if (process.env.NODE_ENV !== 'production') {
          global.prisma = undefined;
        }
      }
    }
  }

  /**
   * Get the database URL for the current provider
   */
  public getDatabaseUrl(): string {
    if (!this.provider) {
      throw new Error('Database provider not initialized');
    }
    return this.provider.getDatabaseUrl();
  }

  /**
   * Get the current provider type
   */
  public getProviderType(): string {
    if (!this.providerType) {
      throw new Error('Database provider not initialized');
    }
    return this.providerType;
  }

  /**
   * Get all enabled providers
   */
  public static getEnabledProviders(): Map<ProviderType, DatabaseProvider> {
    return DatabaseFactory.getEnabledProviders();
  }
}

// Create and export the singleton instance
const db = DatabaseClient.getInstance();

// Export types and utilities
export { db, DatabaseClient };
export type { ProviderType };
export * from '@prisma/client';

export default db;
