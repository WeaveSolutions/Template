import 'dotenv/config';
import express, { Request, Response, NextFunction, RequestHandler } from 'express';
import { auth as oidcAuth } from 'express-openid-connect';
import helmet from 'helmet';
import cors from 'cors';
import compression from 'compression';
import cookieParser from 'cookie-parser';
import rateLimit, { RateLimitRequestHandler } from 'express-rate-limit';
import { v4 as uuidv4 } from 'uuid';

// Middleware imports
import { kongLogger, auditLogger, securityLogger } from './middleware/logger';
import { authMiddleware, extractUserContext } from './middleware/auth';

// Route imports
import authRoutes from './routes/auth';
import userRoutes from './routes/user';
import linkingRoutes from './routes/linking';
import metricsRoutes from './routes/metrics';

// Initialize Express app
const app = express();
const PORT = parseInt(process.env.PORT || '8020', 10);

/**
 * Environment variable validation
 */
function validateEnvironment(): boolean {
  const requiredEnvVars = [
    'NODE_ENV',
    'PORT'
  ];

  // Auth0 configuration (optional in development)
  const auth0Required = process.env.NODE_ENV === 'production';
  const auth0EnvVars = [
    'AUTH0_SECRET',
    'AUTH0_BASE_URL',
    'AUTH0_ISSUER_BASE_URL',
    'AUTH0_CLIENT_ID',
    'AUTH0_CLIENT_SECRET',
    'AUTH0_AUDIENCE'
  ];

  // Check required environment variables
  const missingRequired = requiredEnvVars.filter(envVar => !process.env[envVar]);
  if (missingRequired.length > 0) {
    console.error('❌ Missing required environment variables:', missingRequired);
    process.exit(1);
  }

  // Check Auth0 configuration
  const missingAuth0 = auth0EnvVars.filter(envVar => !process.env[envVar]);
  if (auth0Required && missingAuth0.length > 0) {
    console.error('❌ Missing Auth0 environment variables (required in production):', missingAuth0);
    process.exit(1);
  }

  const enableAuth0 = missingAuth0.length === 0;
  
  if (!enableAuth0) {
    console.log('ℹ️ Running in development mode without Auth0 configuration');
    console.log('ℹ️ Auth routes will return mock responses');
    console.log('🔧 Please go to Auth0 and provide the proper env variables for production use');
  }

  return enableAuth0;
}

/**
 * Check if Auth0 is properly configured
 */
function isAuth0Configured(): boolean {
  const auth0EnvVars = [
    'AUTH0_SECRET',
    'AUTH0_BASE_URL', 
    'AUTH0_ISSUER_BASE_URL',
    'AUTH0_CLIENT_ID',
    'AUTH0_CLIENT_SECRET',
    'AUTH0_AUDIENCE'
  ];
  
  // Check if all environment variables exist and are not placeholder values
  const allVariablesExist = auth0EnvVars.every(envVar => {
    const value = process.env[envVar];
    if (!value) return false;
    
    // Check if it's a placeholder value
    if (value.includes('your-') || value.includes('example.com')) return false;
    
    // For URL variables, validate they are proper URLs
    if (envVar.includes('URL') && value) {
      try {
        new URL(value);
        return true;
      } catch {
        return false;
      }
    }
    
    return true;
  });
  
  return allVariablesExist;
}

/**
 * CSP nonce middleware
 */
function generateCSPNonce(req: Request, res: Response, next: NextFunction): void {
  res.locals.cspNonce = Buffer.from(uuidv4()).toString('base64');
  next();
}

/**
 * Request ID middleware
 */
function requestIdMiddleware(req: Request, res: Response, next: NextFunction): void {
  const requestId = req.get('X-Request-ID') || 
                   req.get('X-Kong-Request-ID') || 
                   uuidv4();
  req.requestId = requestId;
  res.setHeader('X-Request-ID', requestId);
  next();
}

/**
 * Security headers middleware
 */
function securityHeaders(req: Request, res: Response, next: NextFunction): void {
  // Apply helmet with custom CSP
  helmet({
    contentSecurityPolicy: {
      directives: {
        defaultSrc: ["'self'"],
        scriptSrc: ["'self'", `'nonce-${res.locals.cspNonce}'`],
        styleSrc: ["'self'", "'unsafe-inline'"],
        imgSrc: ["'self'", "data:", "https:"],
        connectSrc: ["'self'"],
        fontSrc: ["'self'"],
        objectSrc: ["'none'"],
        mediaSrc: ["'none'"],
        frameSrc: ["'none'"],
      },
    },
    hsts: {
      maxAge: 31536000,
      includeSubDomains: true,
      preload: true
    }
  })(req, res, next);
}

/**
 * Error handler middleware
 */
function errorHandler(err: Error, req: Request, res: Response, next: NextFunction): void {
  const requestId = req.requestId || uuidv4();
  
  // Log error
  console.error(JSON.stringify({
    timestamp: new Date().toISOString(),
    event_type: 'error',
    service: 'auth-service',
    request_id: requestId,
    error: {
      message: err.message,
      stack: process.env.NODE_ENV === 'development' ? err.stack : undefined
    }
  }));

  // Send error response
  res.status(500).json({
    error: 'Internal Server Error',
    message: process.env.NODE_ENV === 'development' ? err.message : 'An error occurred',
    request_id: requestId
  });
}

/**
 * Global middleware configuration
 */
function configureMiddleware(): void {
  // Trust proxy for accurate IP addresses
  app.set('trust proxy', true);

  // Generate CSP nonce for each request
  app.use(generateCSPNonce);

  // Security headers with Helmet
  app.use(securityHeaders);

  // Request ID assignment
  app.use(requestIdMiddleware);

  // Kong-compatible logging
  app.use(kongLogger);

  // Body parsing
  app.use(express.json({ limit: '10mb' }));
  app.use(express.urlencoded({ extended: true, limit: '10mb' }));
  app.use(cookieParser());

  // Compression
  app.use(compression());

  // CORS whitelist
  const corsWhitelist = (process.env.CORS_WHITELIST || 'http://localhost:3000').split(',');
  const corsOptions: cors.CorsOptions = {
    origin: function (origin: string | undefined, callback: (err: Error | null, allow?: boolean) => void) {
      // Allow requests with no origin (like mobile apps or curl requests)
      if (!origin) return callback(null, true);
      
      if (corsWhitelist.indexOf(origin) !== -1) {
        callback(null, true);
      } else {
        callback(new Error('Not allowed by CORS'));
      }
    },
    credentials: true,
    optionsSuccessStatus: 200
  };
  app.use(cors(corsOptions));

  // Auth0 OpenID Connect configuration
  const enableAuth0 = isAuth0Configured();
  if (enableAuth0) {
    const oidcConfig = {
      authRequired: false,
      auth0Logout: true,
      secret: process.env.AUTH0_SECRET!,
      baseURL: process.env.AUTH0_BASE_URL!,
      clientID: process.env.AUTH0_CLIENT_ID!,
      clientSecret: process.env.AUTH0_CLIENT_SECRET!,
      issuerBaseURL: process.env.AUTH0_ISSUER_BASE_URL!,
      idpLogout: true,
      routes: {
        login: '/auth/login',
        logout: '/auth/logout',
        callback: '/auth/callback',
        postLogoutRedirect: process.env.FRONTEND_URL
      },
      authorizationParams: {
        response_type: 'code',
        audience: process.env.AUTH0_AUDIENCE,
        scope: 'openid profile email offline_access'
      },
      session: {
        name: 'cra_auth_session',
        cookie: {
          httpOnly: true,
          sameSite: 'Strict' as const,
          secure: process.env.NODE_ENV === 'production'
        },
        absoluteDuration: 86400, // 24 hours
        rolling: true,
        rollingDuration: 3600 // 1 hour
      }
    };
    app.use(oidcAuth(oidcConfig));
  }

  // Extract user context for Kong
  app.use(extractUserContext);

  // Global rate limiting
  const limiter = rateLimit({
    windowMs: 15 * 60 * 1000, // 15 minutes
    max: 100, // 100 requests per window
    standardHeaders: true,
    legacyHeaders: false,
    message: {
      error: 'Too Many Requests',
      message: 'Rate limit exceeded. Please try again later.'
    }
  });
  app.use(limiter as any);
}

/**
 * Custom requiresAuth middleware
 */
function requiresAuth() {
  return (req: Request, res: Response, next: NextFunction): void => {
    // If Auth0 is not configured, allow all requests in development
    if (!isAuth0Configured()) {
      if (process.env.NODE_ENV === 'development') {
        // Add mock user for development
        (req as any).oidc = {
          isAuthenticated: () => true,
          user: {
            sub: 'dev-user-123',
            email: 'dev@example.com',
            name: 'Development User'
          }
        };
        next();
        return;
      } else {
        res.status(503).json({
          error: 'Service Unavailable',
          message: 'Authentication service not configured'
        });
        return;
      }
    }

    if (!req.oidc?.isAuthenticated()) {
      res.status(401).json({
        error: 'Unauthorized',
        message: 'Authentication required'
      });
      return;
    }
    next();
  };
}

/**
 * Role-based access control middleware
 */
function requireRole(role: string) {
  return (req: Request, res: Response, next: NextFunction): void => {
    // If Auth0 is not configured, allow all requests in development
    if (!isAuth0Configured()) {
      if (process.env.NODE_ENV === 'development') {
        console.log(`ℹ️ Development mode: Allowing access for role '${role}'`);
        next();
        return;
      } else {
        res.status(503).json({
          error: 'Service Unavailable',
          message: 'Authentication service not configured'
        });
        return;
      }
    }

    if (!req.oidc?.isAuthenticated()) {
      res.status(401).json({
        error: 'Unauthorized',
        message: 'Authentication required'
      });
      return;
    }
    
    const userRoles = req.oidc.user?.['https://api.nexpo.io/roles'] || [];
    if (!userRoles.includes(role)) {
      res.status(403).json({
        error: 'Forbidden',
        message: `Role '${role}' required`
      });
      return;
    }
    
    next();
  };
}

/**
 * API Routes configuration
 */
function configureRoutes(): void {
  // Health check endpoint
  app.get('/health', (req: Request, res: Response) => {
    res.json({
      status: 'healthy',
      service: 'auth-service',
      version: process.env.SERVICE_VERSION || '1.0.0',
      timestamp: new Date().toISOString()
    });
  });

  // Kong readiness check
  app.get('/ready', (req: Request, res: Response) => {
    // Check Auth0 connectivity
    const isReady = !!process.env.AUTH0_ISSUER_BASE_URL;
    
    if (isReady) {
      res.json({
        status: 'ready',
        checks: {
          auth0: 'connected',
          database: 'not_required'
        }
      });
    } else {
      res.status(503).json({
        status: 'not_ready',
        checks: {
          auth0: 'disconnected'
        }
      });
    }
  });

  // API version endpoint
  app.get('/api/version', (req: Request, res: Response) => {
    res.json({
      version: process.env.SERVICE_VERSION || '1.0.0',
      api: 'v1',
      features: {
        oauth: true,
        passkeys: process.env.ENABLE_PASSKEYS === 'true',
        linking: true,
        audit: true
      }
    });
  });

  // Mount route modules
  app.use('/api/auth', authRoutes);
  app.use('/api/user', requiresAuth(), userRoutes);
  app.use('/api/linking', requiresAuth(), linkingRoutes);
  app.use('/api/metrics', requiresAuth(), requireRole('admin'), metricsRoutes);

  // 404 handler
  app.use((req: Request, res: Response) => {
    res.status(404).json({
      error: 'Not Found',
      message: 'The requested resource does not exist',
      path: req.path
    });
  });

  // Global error handler
  app.use(errorHandler);
}

/**
 * Start the server
 */
async function startServer(): Promise<void> {
  try {
    // Validate environment
    validateEnvironment();

    // Configure middleware
    configureMiddleware();

    // Configure routes
    configureRoutes();

    // Start listening
    const server = app.listen(PORT, '0.0.0.0', () => {
      console.log(JSON.stringify({
        timestamp: new Date().toISOString(),
        event_type: 'server_start',
        service: 'auth-service',
        message: `CRA Auth Service running on port ${PORT}`,
        environment: process.env.NODE_ENV || 'development',
        features: {
          kong_integration: true,
          auth0: true,
          rate_limiting: true,
          audit_logging: true
        }
      }));
    });

    // Graceful shutdown
    process.on('SIGTERM', () => {
      console.log(JSON.stringify({
        timestamp: new Date().toISOString(),
        event_type: 'server_shutdown',
        service: 'auth-service',
        message: 'Received SIGTERM, shutting down gracefully'
      }));
      
      server.close(() => {
        console.log(JSON.stringify({
          timestamp: new Date().toISOString(),
          event_type: 'server_stopped',
          service: 'auth-service',
          message: 'Server closed'
        }));
        process.exit(0);
      });
    });

  } catch (error) {
    console.error(JSON.stringify({
      timestamp: new Date().toISOString(),
      event_type: 'server_error',
      service: 'auth-service',
      error: error instanceof Error ? error.message : 'Unknown error',
      stack: error instanceof Error ? error.stack : undefined
    }));
    process.exit(1);
  }
}

// Start the server
startServer();
