// Mock BI Dashboard Data
// Shared across all platforms for development and testing

import type {
  KPIData,
  TrendData,
  UserGrowthData,
  SubscriptionBreakdown
} from '../types';

export const mockKPIData: KPIData = {
  // Revenue Metrics
  mrr: 98750.00,
  arr: 1185000.00,
  arpu: 11.05,
  
  // User Metrics
  totalUsers: 15420,
  activeUsers: 8930,
  dau: 3245,
  mau: 8930,
  
  // Subscription Metrics
  activeSubscriptions: 1250,
  churnRate: 3.2,
  
  // Financial Metrics
  ltv: 450.00,
  cac: 120.00,
  ltvCacRatio: 3.75,
  
  // Satisfaction
  nps: 42,
  
  // Technical
  uptime: 99.94,
  avgLoadTime: 1.2,
  errorRate: 0.03
};

export const mockMRRTrend: TrendData[] = [
  { month: 'Jan', value: 85000 },
  { month: 'Feb', value: 87500 },
  { month: 'Mar', value: 91000 },
  { month: 'Apr', value: 93500 },
  { month: 'May', value: 96000 },
  { month: 'Jun', value: 98750 }
];

export const mockARRTrend: TrendData[] = mockMRRTrend.map(item => ({
  month: item.month,
  value: item.value * 12
}));

export const mockUserGrowth: UserGrowthData[] = [
  { month: 'Jan', dau: 2850, mau: 7200 },
  { month: 'Feb', dau: 2920, mau: 7550 },
  { month: 'Mar', dau: 3050, mau: 8100 },
  { month: 'Apr', dau: 3120, mau: 8450 },
  { month: 'May', dau: 3180, mau: 8720 },
  { month: 'Jun', dau: 3245, mau: 8930 }
];

export const mockSubscriptionBreakdown: SubscriptionBreakdown[] = [
  { plan: 'Free', count: 12500, percentage: 81 },
  { plan: 'Pro', count: 2100, percentage: 14 },
  { plan: 'Enterprise', count: 820, percentage: 5 }
];

// Generate mock data for longer time periods
export function generateMockTrendData(months: number, baseValue: number, growthRate: number = 0.05): TrendData[] {
  const monthNames = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
  const data: TrendData[] = [];
  
  for (let i = 0; i < months; i++) {
    const monthIndex = i % 12;
    const value = baseValue * Math.pow(1 + growthRate, i);
    data.push({
      month: monthNames[monthIndex],
      value: Math.round(value)
    });
  }
  
  return data;
}

// Simulate API delay for realistic testing
export function delay(ms: number): Promise<void> {
  return new Promise(resolve => setTimeout(resolve, ms));
}
