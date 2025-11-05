import express from 'express';
import cors from 'cors';
import helmet from 'helmet';
import rateLimit from 'express-rate-limit';
import dotenv from 'dotenv';
import path from 'path';
import fs from 'fs';
import bulkVerificationRouter from './bulk-verification';
import logger from './logger';

// Load environment variables
dotenv.config();

const app = express();
const PORT = process.env.PORT || 7001;

// Create logs directory if it doesn't exist
const logsDir = path.join(__dirname, '../logs');
if (!fs.existsSync(logsDir)) {
  fs.mkdirSync(logsDir, { recursive: true });
}

// Security middleware
app.use(helmet({
  contentSecurityPolicy: {
    directives: {
      defaultSrc: ["'self'"],
      styleSrc: ["'self'", "'unsafe-inline'"],
      scriptSrc: ["'self'"],
      imgSrc: ["'self'", "data:", "https:"],
    },
  },
}));

// CORS configuration
app.use(cors({
  origin: process.env.ALLOWED_ORIGINS?.split(',') || ['http://localhost:3000'],
  credentials: true,
}));

// Rate limiting
const limiter = rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 100, // Limit each IP to 100 requests per windowMs
  message: {
    error: 'Too many requests from this IP, please try again later.',
  },
  standardHeaders: true,
  legacyHeaders: false,
});

app.use(limiter);

// More restrictive rate limiting for verification endpoints
const verificationLimiter = rateLimit({
  windowMs: 60 * 1000, // 1 minute
  max: 10, // Limit verification requests to 10 per minute per IP
  message: {
    error: 'Too many verification requests, please try again later.',
  },
});

// Body parsing middleware
app.use(express.json({ limit: '10mb' }));
app.use(express.urlencoded({ extended: true, limit: '10mb' }));

// Request logging middleware
app.use((req, res, next) => {
  logger.info(`${req.method} ${req.path}`, {
    ip: req.ip,
    userAgent: req.get('User-Agent'),
    timestamp: new Date().toISOString(),
  });
  next();
});

// Health check endpoint
app.get('/health', (req, res) => {
  res.json({
    status: 'healthy',
    service: 'email-verification',
    timestamp: new Date().toISOString(),
    version: process.env.npm_package_version || '1.0.0',
  });
});

// API information endpoint
app.get('/api/info', (req, res) => {
  res.json({
    service: 'Email Verification Microservice',
    description: 'Bulk email verification using Zero Bounce and Brevo integration',
    version: process.env.npm_package_version || '1.0.0',
    endpoints: [
      {
        method: 'POST',
        path: '/api/verify/bulk',
        description: 'Verify multiple emails in bulk',
        rateLimit: '10 requests per minute',
      },
      {
        method: 'POST',
        path: '/api/verify/single',
        description: 'Verify a single email address',
        rateLimit: '10 requests per minute',
      },
      {
        method: 'GET',
        path: '/api/verify/credits',
        description: 'Get remaining verification credits',
        rateLimit: '10 requests per minute',
      },
      {
        method: 'POST',
        path: '/api/verify/clean-lists',
        description: 'Clean Brevo email lists by removing invalid emails',
        rateLimit: '10 requests per minute',
      },
    ],
  });
});

// Apply verification rate limiter to all verification routes
app.use('/api', verificationLimiter);

// Mount verification routes
app.use('/api', bulkVerificationRouter);

// Error handling middleware
app.use((error: Error, req: express.Request, res: express.Response, next: express.NextFunction) => {
  logger.error('Unhandled error:', {
    error: error.message,
    stack: error.stack,
    path: req.path,
    method: req.method,
    ip: req.ip,
  });

  res.status(500).json({
    error: 'Internal server error',
    message: process.env.NODE_ENV === 'development' ? error.message : 'Something went wrong',
  });
});

// 404 handler
app.use('*', (req, res) => {
  res.status(404).json({
    error: 'Not found',
    message: `Route ${req.method} ${req.originalUrl} not found`,
  });
});

// Graceful shutdown handling
process.on('SIGTERM', () => {
  logger.info('SIGTERM received, shutting down gracefully');
  process.exit(0);
});

process.on('SIGINT', () => {
  logger.info('SIGINT received, shutting down gracefully');
  process.exit(0);
});

// Start server
app.listen(PORT, () => {
  logger.info(`Email verification microservice started on port ${PORT}`, {
    port: PORT,
    environment: process.env.NODE_ENV || 'development',
    timestamp: new Date().toISOString(),
  });
});

export default app;
