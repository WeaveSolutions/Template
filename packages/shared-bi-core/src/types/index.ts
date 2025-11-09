// Core BI Dashboard Types
// Shared across all platforms (Nexpo & Taurte, Web/Mobile/Desktop)

export interface KPIData {
  // Revenue Metrics
  mrr: number;
  arr: number;
  arpu: number;
  
  // User Metrics
  totalUsers: number;
  activeUsers: number;
  dau: number;
  mau: number;
  
  // Subscription Metrics
  activeSubscriptions: number;
  churnRate: number;
  
  // Financial Metrics
  ltv: number;
  cac: number;
  ltvCacRatio: number;
  
  // Satisfaction
  nps: number;
  
  // Technical
  uptime: number;
  avgLoadTime: number;
  errorRate: number;
}

export interface ChartDataPoint {
  label: string;
  value: number;
}

export interface TimeSeriesDataPoint {
  timestamp: Date;
  value: number;
}

export interface TrendData {
  month: string;
  value: number;
}

export interface UserGrowthData {
  month: string;
  dau: number;
  mau: number;
}

export interface SubscriptionBreakdown {
  plan: 'Free' | 'Pro' | 'Enterprise';
  count: number;
  percentage: number;
}

export type DashboardTab = 'overview' | 'revenue' | 'users' | 'looker';

export interface BIDashboardConfig {
  lookerStudioUrl: string;
  refreshInterval?: number; // milliseconds
  enableRealtime?: boolean;
}

// API Response Types
export interface APIResponse<T> {
  data: T;
  error?: string;
  timestamp: Date;
}

export interface PaginatedResponse<T> {
  data: T[];
  page: number;
  pageSize: number;
  total: number;
  hasMore: boolean;
}

// Filter Types
export interface DateRangeFilter {
  startDate: Date;
  endDate: Date;
}

export interface DashboardFilters {
  dateRange?: DateRangeFilter;
  platform?: 'web' | 'mobile_ios' | 'mobile_android' | 'desktop';
  plan?: 'Free' | 'Pro' | 'Enterprise';
}
