// BI Dashboard API Client
// Platform-agnostic interface with multiple implementations

import type {
  KPIData,
  TrendData,
  UserGrowthData,
  SubscriptionBreakdown,
  DashboardFilters,
  APIResponse
} from '../types';
import {
  mockKPIData,
  mockMRRTrend,
  mockUserGrowth,
  mockSubscriptionBreakdown,
  delay
} from '../mock-data';

// ==================== API Client Interface ====================

export interface BIApiClient {
  /**
   * Fetch current KPI data
   */
  fetchKPIData(filters?: DashboardFilters): Promise<KPIData>;

  /**
   * Fetch MRR trend data
   */
  fetchMRRTrend(startDate: Date, endDate: Date): Promise<TrendData[]>;

  /**
   * Fetch user growth data
   */
  fetchUserGrowth(startDate: Date, endDate: Date): Promise<UserGrowthData[]>;

  /**
   * Fetch subscription breakdown
   */
  fetchSubscriptionBreakdown(): Promise<SubscriptionBreakdown[]>;

  /**
   * Check API health
   */
  health(): Promise<boolean>;
}

// ==================== Mock Implementation ====================

export class MockBIApi implements BIApiClient {
  private simulateDelay: boolean;
  private delayMs: number;

  constructor(options?: { simulateDelay?: boolean; delayMs?: number }) {
    this.simulateDelay = options?.simulateDelay ?? true;
    this.delayMs = options?.delayMs ?? 500;
  }

  async fetchKPIData(filters?: DashboardFilters): Promise<KPIData> {
    if (this.simulateDelay) await delay(this.delayMs);
    
    // In a real implementation, apply filters here
    console.log('[MockBIApi] Fetching KPI data with filters:', filters);
    
    return { ...mockKPIData };
  }

  async fetchMRRTrend(startDate: Date, endDate: Date): Promise<TrendData[]> {
    if (this.simulateDelay) await delay(this.delayMs);
    
    console.log('[MockBIApi] Fetching MRR trend:', { startDate, endDate });
    
    return [...mockMRRTrend];
  }

  async fetchUserGrowth(startDate: Date, endDate: Date): Promise<UserGrowthData[]> {
    if (this.simulateDelay) await delay(this.delayMs);
    
    console.log('[MockBIApi] Fetching user growth:', { startDate, endDate });
    
    return [...mockUserGrowth];
  }

  async fetchSubscriptionBreakdown(): Promise<SubscriptionBreakdown[]> {
    if (this.simulateDelay) await delay(this.delayMs);
    
    console.log('[MockBIApi] Fetching subscription breakdown');
    
    return [...mockSubscriptionBreakdown];
  }

  async health(): Promise<boolean> {
    return true;
  }
}

// ==================== Firebase Implementation Stub ====================

export class FirebaseBIApi implements BIApiClient {
  private projectId: string;
  private apiKey: string;

  constructor(config: { projectId: string; apiKey: string }) {
    this.projectId = config.projectId;
    this.apiKey = config.apiKey;
  }

  async fetchKPIData(filters?: DashboardFilters): Promise<KPIData> {
    // TODO: Implement Firestore queries
    throw new Error('FirebaseBIApi not yet implemented. Use MockBIApi for now.');
  }

  async fetchMRRTrend(startDate: Date, endDate: Date): Promise<TrendData[]> {
    // TODO: Implement Firestore time-series query
    throw new Error('FirebaseBIApi not yet implemented. Use MockBIApi for now.');
  }

  async fetchUserGrowth(startDate: Date, endDate: Date): Promise<UserGrowthData[]> {
    // TODO: Implement Firestore time-series query
    throw new Error('FirebaseBIApi not yet implemented. Use MockBIApi for now.');
  }

  async fetchSubscriptionBreakdown(): Promise<SubscriptionBreakdown[]> {
    // TODO: Implement Firestore aggregation query
    throw new Error('FirebaseBIApi not yet implemented. Use MockBIApi for now.');
  }

  async health(): Promise<boolean> {
    // TODO: Ping Firebase
    return false;
  }
}

// ==================== Supabase Implementation Stub ====================

export class SupabaseBIApi implements BIApiClient {
  private supabaseUrl: string;
  private supabaseKey: string;

  constructor(config: { supabaseUrl: string; supabaseKey: string }) {
    this.supabaseUrl = config.supabaseUrl;
    this.supabaseKey = config.supabaseKey;
  }

  async fetchKPIData(filters?: DashboardFilters): Promise<KPIData> {
    // TODO: Implement Supabase PostgreSQL queries
    throw new Error('SupabaseBIApi not yet implemented. Use MockBIApi for now.');
  }

  async fetchMRRTrend(startDate: Date, endDate: Date): Promise<TrendData[]> {
    // TODO: Implement Supabase time-series query
    throw new Error('SupabaseBIApi not yet implemented. Use MockBIApi for now.');
  }

  async fetchUserGrowth(startDate: Date, endDate: Date): Promise<UserGrowthData[]> {
    // TODO: Implement Supabase time-series query
    throw new Error('SupabaseBIApi not yet implemented. Use MockBIApi for now.');
  }

  async fetchSubscriptionBreakdown(): Promise<SubscriptionBreakdown[]> {
    // TODO: Implement Supabase aggregation query
    throw new Error('SupabaseBIApi not yet implemented. Use MockBIApi for now.');
  }

  async health(): Promise<boolean> {
    // TODO: Ping Supabase
    return false;
  }
}

// ==================== MongoDB Implementation Stub ====================

export class MongoDBBIApi implements BIApiClient {
  private connectionString: string;
  private database: string;

  constructor(config: { connectionString: string; database: string }) {
    this.connectionString = config.connectionString;
    this.database = config.database;
  }

  async fetchKPIData(filters?: DashboardFilters): Promise<KPIData> {
    // TODO: Implement MongoDB aggregation pipeline
    throw new Error('MongoDBBIApi not yet implemented. Use MockBIApi for now.');
  }

  async fetchMRRTrend(startDate: Date, endDate: Date): Promise<TrendData[]> {
    // TODO: Implement MongoDB time-series query
    throw new Error('MongoDBBIApi not yet implemented. Use MockBIApi for now.');
  }

  async fetchUserGrowth(startDate: Date, endDate: Date): Promise<UserGrowthData[]> {
    // TODO: Implement MongoDB time-series query
    throw new Error('MongoDBBIApi not yet implemented. Use MockBIApi for now.');
  }

  async fetchSubscriptionBreakdown(): Promise<SubscriptionBreakdown[]> {
    // TODO: Implement MongoDB aggregation pipeline
    throw new Error('MongoDBBIApi not yet implemented. Use MockBIApi for now.');
  }

  async health(): Promise<boolean> {
    // TODO: Ping MongoDB
    return false;
  }
}

// ==================== Factory Function ====================

export type BIApiType = 'mock' | 'firebase' | 'supabase' | 'mongodb';

export interface BIApiConfig {
  type: BIApiType;
  firebase?: {
    projectId: string;
    apiKey: string;
  };
  supabase?: {
    supabaseUrl: string;
    supabaseKey: string;
  };
  mongodb?: {
    connectionString: string;
    database: string;
  };
  mock?: {
    simulateDelay?: boolean;
    delayMs?: number;
  };
}

/**
 * Factory function to create the appropriate BI API client
 */
export function createBIClient(config: BIApiConfig): BIApiClient {
  switch (config.type) {
    case 'mock':
      return new MockBIApi(config.mock);
    
    case 'firebase':
      if (!config.firebase) {
        throw new Error('Firebase config required for firebase API type');
      }
      return new FirebaseBIApi(config.firebase);
    
    case 'supabase':
      if (!config.supabase) {
        throw new Error('Supabase config required for supabase API type');
      }
      return new SupabaseBIApi(config.supabase);
    
    case 'mongodb':
      if (!config.mongodb) {
        throw new Error('MongoDB config required for mongodb API type');
      }
      return new MongoDBBIApi(config.mongodb);
    
    default:
      throw new Error(`Unknown API type: ${config.type}`);
  }
}
