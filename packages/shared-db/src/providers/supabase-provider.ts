import { BaseDatabaseProvider, ProviderType, PrismaClient } from './base-provider';

export class SupabaseProvider extends BaseDatabaseProvider<PrismaClient> {
  private static instance: SupabaseProvider;

  private constructor(databaseUrl: string) {
    super(databaseUrl, 'supabase' as ProviderType);
  }

  public static getInstance(databaseUrl: string): SupabaseProvider {
    if (!SupabaseProvider.instance) {
      SupabaseProvider.instance = new SupabaseProvider(databaseUrl);
    }
    return SupabaseProvider.instance;
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
   * Override the client path for Supabase
   */
  getClientPath(): string {
    return '@prisma/client-supabase';
  }

  /**
   * Supabase-specific configuration
   */
  getSupabaseConfig() {
    return {
      url: process.env.SUPABASE_URL,
      anonKey: process.env.SUPABASE_ANON_KEY,
      serviceKey: process.env.SUPABASE_SERVICE_KEY,
    };
  }
}
