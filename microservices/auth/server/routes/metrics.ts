import { Router, Request, Response } from 'express';
import { metricsLogger } from '../middleware/logger';
import { readdirSync, statSync, readFileSync } from 'fs';
import { join } from 'path';

const router: Router = Router();

// In-memory metrics storage (use Redis in production)
const metrics = {
  requests: {
    total: 0,
    by_endpoint: {} as Record<string, number>,
    by_status: {} as Record<string, number>
  },
  auth: {
    logins: 0,
    logouts: 0,
    failed_attempts: 0,
    token_refreshes: 0
  },
  performance: {
    response_times: [] as number[],
    avg_response_time: 0
  },
  security: {
    rate_limit_hits: 0,
    suspicious_activities: 0,
    blocked_ips: [] as string[]
  }
};

interface LogStats {
  totalLogs: number;
  errorLogs: number;
  auditLogs: number;
  securityAlerts: number;
  diskUsage: number;
}

/**
 * Calculate log statistics
 */
function calculateLogStats(): LogStats {
  const logsDir = join(__dirname, '../../logs');
  let totalLogs = 0;
  let errorLogs = 0;
  let auditLogs = 0;
  let securityAlerts = 0;
  let diskUsage = 0;
  
  try {
    const files = readdirSync(logsDir);
    
    for (const file of files) {
      const filePath = join(logsDir, file);
      const stats = statSync(filePath);
      diskUsage += stats.size;
      
      if (file.includes('audit')) {
        const content = readFileSync(filePath, 'utf-8');
        const lines = content.split('\n').filter(line => line.trim());
        auditLogs += lines.length;
        totalLogs += lines.length;
      } else if (file.includes('security')) {
        const content = readFileSync(filePath, 'utf-8');
        const lines = content.split('\n').filter(line => line.trim());
        securityAlerts += lines.length;
        totalLogs += lines.length;
      } else if (file.includes('error')) {
        const content = readFileSync(filePath, 'utf-8');
        const lines = content.split('\n').filter(line => line.trim());
        errorLogs += lines.length;
        totalLogs += lines.length;
      } else if (file.includes('application')) {
        const content = readFileSync(filePath, 'utf-8');
        const lines = content.split('\n').filter(line => line.trim());
        totalLogs += lines.length;
      }
    }
  } catch (error) {
    console.error('Error reading log files:', error);
  }
  
  return {
    totalLogs,
    errorLogs,
    auditLogs,
    securityAlerts,
    diskUsage
  };
}

/**
 * Get current metrics
 */
router.get('/current', (req: Request, res: Response) => {
  // Calculate averages
  const avgResponseTime = metrics.performance.response_times.length > 0
    ? metrics.performance.response_times.reduce((a, b) => a + b, 0) / metrics.performance.response_times.length
    : 0;

  res.json({
    timestamp: new Date().toISOString(),
    uptime: process.uptime(),
    metrics: {
      requests: metrics.requests,
      auth: metrics.auth,
      performance: {
        avg_response_time_ms: Math.round(avgResponseTime),
        total_samples: metrics.performance.response_times.length
      },
      security: {
        rate_limit_hits: metrics.security.rate_limit_hits,
        suspicious_activities: metrics.security.suspicious_activities,
        blocked_ip_count: metrics.security.blocked_ips.length
      }
    }
  });
});

/**
 * Get health metrics for monitoring
 */
router.get('/health', (req: Request, res: Response) => {
  const memoryUsage = process.memoryUsage();
  const cpuUsage = process.cpuUsage();
  
  res.json({
    timestamp: new Date().toISOString(),
    service: 'auth-service',
    status: 'healthy',
    uptime_seconds: Math.floor(process.uptime()),
    memory: {
      rss_mb: Math.round(memoryUsage.rss / 1024 / 1024),
      heap_used_mb: Math.round(memoryUsage.heapUsed / 1024 / 1024),
      heap_total_mb: Math.round(memoryUsage.heapTotal / 1024 / 1024),
      external_mb: Math.round(memoryUsage.external / 1024 / 1024)
    },
    cpu: {
      user_ms: cpuUsage.user / 1000,
      system_ms: cpuUsage.system / 1000
    },
    environment: {
      node_version: process.version,
      platform: process.platform,
      pid: process.pid
    }
  });
});

/**
 * Health check endpoint
 */
router.get('/health-check', (req: Request, res: Response) => {
  res.json({
    status: 'healthy',
    timestamp: new Date().toISOString(),
    uptime: process.uptime(),
    environment: process.env.NODE_ENV,
    version: process.env.npm_package_version || '1.0.0'
  });
});

/**
 * Readiness check endpoint
 */
router.get('/ready', (req: Request, res: Response) => {
  const checks = {
    auth0: !!process.env.AUTH0_ISSUER_BASE_URL,
    redis: !!process.env.REDIS_URL,
    session: !!process.env.SESSION_SECRET,
    frontend: !!process.env.FRONTEND_URL
  };
  
  const allReady = Object.values(checks).every(check => check === true);
  
  res.status(allReady ? 200 : 503).json({
    ready: allReady,
    checks,
    timestamp: new Date().toISOString()
  });
});

/**
 * Export metrics in Prometheus format
 */
router.get('/prometheus', (req: Request, res: Response) => {
  const timestamp = Date.now();
  const lines = [
    '# HELP auth_requests_total Total number of requests',
    '# TYPE auth_requests_total counter',
    `auth_requests_total ${metrics.requests.total}`,
    '',
    '# HELP auth_logins_total Total number of successful logins',
    '# TYPE auth_logins_total counter',
    `auth_logins_total ${metrics.auth.logins}`,
    '',
    '# HELP auth_failed_attempts_total Total number of failed auth attempts',
    '# TYPE auth_failed_attempts_total counter',
    `auth_failed_attempts_total ${metrics.auth.failed_attempts}`,
    '',
    '# HELP auth_response_time_ms Average response time in milliseconds',
    '# TYPE auth_response_time_ms gauge',
    `auth_response_time_ms ${metrics.performance.avg_response_time}`,
    '',
    '# HELP auth_rate_limit_hits_total Total number of rate limit hits',
    '# TYPE auth_rate_limit_hits_total counter',
    `auth_rate_limit_hits_total ${metrics.security.rate_limit_hits}`,
    '',
    '# HELP auth_uptime_seconds Service uptime in seconds',
    '# TYPE auth_uptime_seconds gauge',
    `auth_uptime_seconds ${Math.floor(process.uptime())}`
  ];

  res.set('Content-Type', 'text/plain; version=0.0.4');
  res.send(lines.join('\n'));
});

/**
 * Metrics endpoint (Prometheus format)
 */
router.get('/prometheus-logs', (req: Request, res: Response) => {
  const stats = calculateLogStats();
  const uptime = process.uptime();
  const memoryUsage = process.memoryUsage();
  
  const metrics = `
# HELP nodejs_app_uptime_seconds Application uptime in seconds
# TYPE nodejs_app_uptime_seconds gauge
nodejs_app_uptime_seconds ${uptime}

# HELP nodejs_memory_heap_used_bytes Process heap memory used
# TYPE nodejs_memory_heap_used_bytes gauge
nodejs_memory_heap_used_bytes ${memoryUsage.heapUsed}

# HELP nodejs_memory_heap_total_bytes Process heap memory total
# TYPE nodejs_memory_heap_total_bytes gauge
nodejs_memory_heap_total_bytes ${memoryUsage.heapTotal}

# HELP nodejs_memory_external_bytes Process external memory
# TYPE nodejs_memory_external_bytes gauge
nodejs_memory_external_bytes ${memoryUsage.external}

# HELP auth_total_logs_count Total number of log entries
# TYPE auth_total_logs_count counter
auth_total_logs_count ${stats.totalLogs}

# HELP auth_error_logs_count Total number of error log entries
# TYPE auth_error_logs_count counter
auth_error_logs_count ${stats.errorLogs}

# HELP auth_audit_logs_count Total number of audit log entries
# TYPE auth_audit_logs_count counter
auth_audit_logs_count ${stats.auditLogs}

# HELP auth_security_alerts_count Total number of security alerts
# TYPE auth_security_alerts_count counter
auth_security_alerts_count ${stats.securityAlerts}

# HELP auth_logs_disk_usage_bytes Total disk usage by log files
# TYPE auth_logs_disk_usage_bytes gauge
auth_logs_disk_usage_bytes ${stats.diskUsage}
`.trim();
  
  res.set('Content-Type', 'text/plain; version=0.0.4');
  res.send(metrics);
});

/**
 * JSON metrics endpoint
 */
router.get('/json', (req: Request, res: Response) => {
  const stats = calculateLogStats();
  const memoryUsage = process.memoryUsage();
  
  res.json({
    timestamp: new Date().toISOString(),
    uptime: process.uptime(),
    memory: {
      heap_used: memoryUsage.heapUsed,
      heap_total: memoryUsage.heapTotal,
      external: memoryUsage.external,
      rss: memoryUsage.rss
    },
    logs: stats,
    environment: {
      node_version: process.version,
      env: process.env.NODE_ENV,
      pid: process.pid,
      platform: process.platform
    }
  });
});

/**
 * Feature flags endpoint
 */
router.get('/features', (req: Request, res: Response) => {
  res.json({
    oauth_providers: {
      google: process.env.ENABLE_GOOGLE_AUTH === 'true',
      facebook: process.env.ENABLE_FACEBOOK_AUTH === 'true',
      apple: process.env.ENABLE_APPLE_AUTH === 'true',
      github: process.env.ENABLE_GITHUB_AUTH === 'true',
      microsoft: process.env.ENABLE_MICROSOFT_AUTH === 'true'
    },
    authentication: {
      passwordless: process.env.ENABLE_PASSWORDLESS === 'true',
      passkeys: process.env.ENABLE_PASSKEYS === 'true',
      mfa: process.env.ENABLE_MFA === 'true'
    },
    security: {
      rate_limiting: process.env.ENABLE_RATE_LIMITING === 'true',
      jwt_validation: process.env.ENABLE_JWT_VALIDATION === 'true',
      audit_logging: process.env.ENABLE_AUDIT_LOGGING === 'true'
    }
  });
});

/**
 * Reset metrics (admin only)
 */
router.post('/reset', (req: Request, res: Response) => {
  // Log the reset
  metricsLogger('metrics_reset', 0, 'action', {
    reset_by: req.oidc?.user?.sub || 'unknown',
    timestamp: new Date().toISOString()
  });

  // Reset metrics
  metrics.requests = {
    total: 0,
    by_endpoint: {},
    by_status: {}
  };
  metrics.auth = {
    logins: 0,
    logouts: 0,
    failed_attempts: 0,
    token_refreshes: 0
  };
  metrics.performance = {
    response_times: [],
    avg_response_time: 0
  };
  metrics.security = {
    rate_limit_hits: 0,
    suspicious_activities: 0,
    blocked_ips: []
  };

  res.json({
    success: true,
    message: 'Metrics reset successfully'
  });
});

// Export metrics object for middleware to update
export { metrics };
export default router;
