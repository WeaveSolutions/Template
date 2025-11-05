import { Request, Response, NextFunction } from 'express';

// Extend Express Request type for Kong compatibility
declare global {
  namespace Express {
    interface Request {
      requestId?: string;
      kongContext?: {
        startTime: number;
        userId?: string;
        userRoles?: string[];
      };
    }
  }
}

/**
 * Kong-compatible logging middleware for CRA architecture
 * Logs in JSON format compatible with Kong API Gateway
 */
export const kongLogger = (req: Request, res: Response, next: NextFunction) => {
  const startTime = Date.now();
  const requestId = req.get('X-Request-ID') || 
                   req.get('X-Kong-Request-ID') || 
                   `auth-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  
  // Store Kong context
  req.requestId = requestId;
  req.kongContext = {
    startTime,
    userId: req.user?.sub,
    userRoles: req.user?.['https://cra.com/roles'] || []
  };
  
  // Capture the original res.json and res.send
  const originalJson = res.json;
  const originalSend = res.send;
  
  // Override res.json
  res.json = function(data: any) {
    res.locals.responseData = data;
    return originalJson.call(this, data);
  };
  
  // Override res.send
  res.send = function(data: any) {
    res.locals.responseData = data;
    const endTime = Date.now();
    const duration = endTime - startTime;
    
    // Kong-compatible log entry
    const logEntry = {
      timestamp: new Date().toISOString(),
      service: {
        name: 'auth-service',
        version: process.env.SERVICE_VERSION || '1.0.0'
      },
      request: {
        id: requestId,
        method: req.method,
        uri: req.originalUrl || req.url,
        url: `${req.protocol}://${req.get('host')}${req.originalUrl || req.url}`,
        size: req.get('Content-Length') || '0',
        headers: {
          'user-agent': req.get('User-Agent') || '',
          'x-forwarded-for': req.get('X-Forwarded-For') || req.ip,
          'x-real-ip': req.get('X-Real-IP') || req.socket.remoteAddress
        }
      },
      response: {
        status: res.statusCode,
        size: Buffer.byteLength(data || '', 'utf8').toString(),
        headers: res.getHeaders()
      },
      latencies: {
        request: duration,
        proxy: 0, // Set by Kong
        kong: 0   // Set by Kong
      },
      client_ip: req.ip || req.get('X-Real-IP') || req.get('X-Forwarded-For')?.split(',')[0] || req.socket.remoteAddress,
      authenticated_entity: req.kongContext?.userId ? {
        id: req.kongContext.userId,
        consumer_id: req.kongContext.userId
      } : undefined,
      route: {
        id: req.route?.path || req.path,
        name: req.route?.path || req.path,
        service: {
          id: 'auth-service',
          name: 'auth-service'
        }
      },
      started_at: startTime,
      tries: [{
        balancer_latency: 0,
        port: parseInt(process.env.PORT || '7000'),
        ip: '127.0.0.1'
      }]
    };
    
    // Log to stdout in JSON format (for Kong log aggregation)
    console.log(JSON.stringify(logEntry));
    
    return originalSend.call(this, data);
  };
  
  next();
};

/**
 * Audit logging for security events
 * Compatible with CRA 10-year retention policy
 */
export const auditLogger = (
  event: string,
  userId: string,
  metadata: Record<string, any> = {}
) => {
  const auditEntry = {
    timestamp: new Date().toISOString(),
    event_type: 'audit',
    service: 'auth-service',
    user_id: userId,
    event: event,
    metadata: metadata,
    retention_days: 3653, // 10 years as per CRA spec
    compliance: {
      gdpr: true,
      ccpa: true,
      hipaa: false
    }
  };
  
  console.log(JSON.stringify(auditEntry));
};

/**
 * Security event logger for threat detection
 */
export const securityLogger = (
  threatType: string,
  severity: 'low' | 'medium' | 'high' | 'critical',
  metadata: Record<string, any> = {}
) => {
  const securityEntry = {
    timestamp: new Date().toISOString(),
    event_type: 'security',
    service: 'auth-service',
    threat: {
      type: threatType,
      severity: severity,
      detected_at: new Date().toISOString()
    },
    metadata: metadata,
    alert: severity === 'high' || severity === 'critical'
  };
  
  console.log(JSON.stringify(securityEntry));
};

/**
 * Performance metrics logger
 */
export const metricsLogger = (
  metric: string,
  value: number,
  unit: string = 'ms',
  tags: Record<string, string> = {}
) => {
  const metricEntry = {
    timestamp: new Date().toISOString(),
    event_type: 'metric',
    service: 'auth-service',
    metric: {
      name: metric,
      value: value,
      unit: unit,
      tags: tags
    }
  };
  
  console.log(JSON.stringify(metricEntry));
};
