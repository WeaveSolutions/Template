// BI Dashboard Configuration
// Shared constants, thresholds, and configuration values

/**
 * Looker Studio Embed URLs
 * Each metric type (MRR/ARR, MAU/DAU) has its own dedicated Looker Studio report
 * These URLs should be configured in your environment variables
 */
export const LOOKER_STUDIO_EMBEDS = {
  revenue: {
    mrr: process.env.NEXT_PUBLIC_LOOKER_MRR_URL || process.env.LOOKER_MRR_URL || 'https://lookerstudio.google.com/embed/reporting/YOUR_MRR_REPORT_ID',
    arr: process.env.NEXT_PUBLIC_LOOKER_ARR_URL || process.env.LOOKER_ARR_URL || 'https://lookerstudio.google.com/embed/reporting/YOUR_ARR_REPORT_ID'
  },
  users: {
    mau: process.env.NEXT_PUBLIC_LOOKER_MAU_URL || process.env.LOOKER_MAU_URL || 'https://lookerstudio.google.com/embed/reporting/YOUR_MAU_REPORT_ID',
    dau: process.env.NEXT_PUBLIC_LOOKER_DAU_URL || process.env.LOOKER_DAU_URL || 'https://lookerstudio.google.com/embed/reporting/YOUR_DAU_REPORT_ID'
  },
  // Generic Looker Studio URL for the "Looker Studio" tab
  dashboard: process.env.NEXT_PUBLIC_LOOKER_STUDIO_URL || process.env.LOOKER_STUDIO_URL || 'https://lookerstudio.google.com'
};

// Legacy export for backward compatibility
export const LOOKER_STUDIO_URL = LOOKER_STUDIO_EMBEDS.dashboard;

// Chart color palette
export const CHART_COLORS = {
  primary: '#3B82F6',    // Blue
  success: '#10B981',    // Green
  warning: '#F59E0B',    // Amber
  danger: '#EF4444',     // Red
  info: '#06B6D4',       // Cyan
  purple: '#8B5CF6',     // Purple
  muted: '#6B7280',      // Gray
};

// KPI thresholds for health checks
export const KPI_THRESHOLDS = {
  churnRate: {
    healthy: 3,
    warning: 5,
    danger: 10
  },
  ltvCacRatio: {
    minimum: 3,
    target: 5
  },
  uptime: {
    minimum: 99.5,
    target: 99.9
  },
  errorRate: {
    warning: 0.5,
    danger: 1.0
  },
  nps: {
    detractor: 0,
    passive: 7,
    promoter: 9
  },
  dauMauRatio: {
    minimum: 20,
    target: 40
  }
};

// API configuration
export const API_CONFIG = {
  defaultTimeout: 30000, // 30 seconds
  retryAttempts: 3,
  retryDelay: 1000, // 1 second
};

// Dashboard refresh intervals (milliseconds)
export const REFRESH_INTERVALS = {
  realtime: 30000,     // 30 seconds
  frequent: 300000,    // 5 minutes
  normal: 900000,      // 15 minutes
  slow: 3600000,       // 1 hour
};

// Date range presets
export const DATE_RANGE_PRESETS = {
  today: {
    label: 'Today',
    days: 0
  },
  yesterday: {
    label: 'Yesterday',
    days: 1
  },
  last7Days: {
    label: 'Last 7 Days',
    days: 7
  },
  last30Days: {
    label: 'Last 30 Days',
    days: 30
  },
  last90Days: {
    label: 'Last 90 Days',
    days: 90
  },
  thisMonth: {
    label: 'This Month',
    days: new Date().getDate()
  },
  lastMonth: {
    label: 'Last Month',
    days: 30
  },
  thisYear: {
    label: 'This Year',
    days: Math.floor((Date.now() - new Date(new Date().getFullYear(), 0, 0).getTime()) / 86400000)
  }
};

// Chart configuration
export const CHART_CONFIG = {
  animation: {
    duration: 750,
    easing: 'easeInOutCubic'
  },
  responsive: true,
  maintainAspectRatio: false,
  plugins: {
    legend: {
      display: true,
      position: 'bottom' as const
    },
    tooltip: {
      enabled: true,
      mode: 'index' as const,
      intersect: false
    }
  }
};

// Platform-specific settings
export const PLATFORM_SETTINGS = {
  mobile: {
    chartHeight: 200,
    cardPadding: 12,
    fontSize: 14
  },
  tablet: {
    chartHeight: 300,
    cardPadding: 16,
    fontSize: 16
  },
  desktop: {
    chartHeight: 400,
    cardPadding: 24,
    fontSize: 16
  }
};

// Export all as default for convenience
export default {
  LOOKER_STUDIO_URL,
  CHART_COLORS,
  KPI_THRESHOLDS,
  API_CONFIG,
  REFRESH_INTERVALS,
  DATE_RANGE_PRESETS,
  CHART_CONFIG,
  PLATFORM_SETTINGS
};
