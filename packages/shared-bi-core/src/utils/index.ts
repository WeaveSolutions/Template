// BI Dashboard Utility Functions
// Shared calculations, formatters, and validators

import type { KPIData } from '../types';

// ==================== Calculations ====================

/**
 * Calculate Average Revenue Per User
 */
export function calculateARPU(mrr: number, activeUsers: number): number {
  if (activeUsers === 0) return 0;
  return mrr / activeUsers;
}

/**
 * Calculate Annual Recurring Revenue from MRR
 */
export function calculateARR(mrr: number): number {
  return mrr * 12;
}

/**
 * Calculate LTV:CAC Ratio
 */
export function calculateLTVCACRatio(ltv: number, cac: number): number {
  if (cac === 0) return 0;
  return ltv / cac;
}

/**
 * Calculate DAU/MAU Ratio (Stickiness)
 */
export function calculateDAUMAURatio(dau: number, mau: number): number {
  if (mau === 0) return 0;
  return (dau / mau) * 100;
}

/**
 * Calculate Churn Rate
 */
export function calculateChurnRate(
  customersAtStart: number,
  customersLost: number
): number {
  if (customersAtStart === 0) return 0;
  return (customersLost / customersAtStart) * 100;
}

/**
 * Calculate Customer Lifetime Value
 */
export function calculateLTV(arpu: number, churnRate: number): number {
  if (churnRate === 0) return 0;
  return arpu / (churnRate / 100);
}

/**
 * Calculate Payback Period (months)
 */
export function calculatePaybackPeriod(cac: number, arpu: number): number {
  if (arpu === 0) return 0;
  return cac / arpu;
}

// ==================== Formatters ====================

/**
 * Format currency values
 */
export function formatCurrency(value: number, currency: string = 'USD'): string {
  return new Intl.NumberFormat('en-US', {
    style: 'currency',
    currency,
    minimumFractionDigits: 2,
    maximumFractionDigits: 2
  }).format(value);
}

/**
 * Format percentage values
 */
export function formatPercentage(value: number, decimals: number = 1): string {
  return `${value.toFixed(decimals)}%`;
}

/**
 * Format large numbers with abbreviations (K, M, B)
 */
export function formatNumber(value: number): string {
  if (value >= 1_000_000_000) {
    return `${(value / 1_000_000_000).toFixed(1)}B`;
  }
  if (value >= 1_000_000) {
    return `${(value / 1_000_000).toFixed(1)}M`;
  }
  if (value >= 1_000) {
    return `${(value / 1_000).toFixed(1)}K`;
  }
  return value.toLocaleString();
}

/**
 * Format date to readable string
 */
export function formatDate(date: Date): string {
  return new Intl.DateTimeFormat('en-US', {
    year: 'numeric',
    month: 'short',
    day: 'numeric'
  }).format(date);
}

/**
 * Format relative time (e.g., "2 hours ago")
 */
export function formatRelativeTime(date: Date): string {
  const now = new Date();
  const diffMs = now.getTime() - date.getTime();
  const diffSecs = Math.floor(diffMs / 1000);
  const diffMins = Math.floor(diffSecs / 60);
  const diffHours = Math.floor(diffMins / 60);
  const diffDays = Math.floor(diffHours / 24);

  if (diffSecs < 60) return 'just now';
  if (diffMins < 60) return `${diffMins} minute${diffMins !== 1 ? 's' : ''} ago`;
  if (diffHours < 24) return `${diffHours} hour${diffHours !== 1 ? 's' : ''} ago`;
  if (diffDays < 30) return `${diffDays} day${diffDays !== 1 ? 's' : ''} ago`;
  
  return formatDate(date);
}

// ==================== Validators ====================

/**
 * Validate KPI data completeness
 */
export function validateKPIData(data: Partial<KPIData>): boolean {
  const requiredFields: (keyof KPIData)[] = [
    'mrr', 'arr', 'totalUsers', 'activeUsers', 'activeSubscriptions'
  ];
  
  return requiredFields.every(field => 
    data[field] !== undefined && 
    data[field] !== null &&
    typeof data[field] === 'number'
  );
}

/**
 * Check if KPI value is within healthy range
 */
export function isHealthyKPI(metric: keyof KPIData, value: number): boolean {
  const thresholds: Partial<Record<keyof KPIData, { min?: number; max?: number }>> = {
    churnRate: { max: 5 }, // Healthy if < 5%
    ltvCacRatio: { min: 3 }, // Healthy if >= 3
    uptime: { min: 99.9 }, // Healthy if >= 99.9%
    errorRate: { max: 1 }, // Healthy if < 1%
    nps: { min: 0 } // Healthy if positive
  };

  const threshold = thresholds[metric];
  if (!threshold) return true;

  if (threshold.min !== undefined && value < threshold.min) return false;
  if (threshold.max !== undefined && value > threshold.max) return false;

  return true;
}

// ==================== Trend Analysis ====================

/**
 * Calculate growth rate between two values
 */
export function calculateGrowthRate(oldValue: number, newValue: number): number {
  if (oldValue === 0) return 0;
  return ((newValue - oldValue) / oldValue) * 100;
}

/**
 * Determine trend direction
 */
export function getTrendDirection(growthRate: number): 'up' | 'down' | 'stable' {
  if (growthRate > 1) return 'up';
  if (growthRate < -1) return 'down';
  return 'stable';
}

/**
 * Check if trend is positive for a given metric
 */
export function isPositiveTrend(metric: keyof KPIData, growthRate: number): boolean {
  // For these metrics, down is good
  const inverseMetrics: (keyof KPIData)[] = ['churnRate', 'cac', 'errorRate', 'avgLoadTime'];
  
  if (inverseMetrics.includes(metric)) {
    return growthRate < 0;
  }
  
  return growthRate > 0;
}
