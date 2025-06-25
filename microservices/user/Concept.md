# User Microservice Implementation Guide

## Overview
The User Microservice is a critical component of the Nexpo platform that handles user profile management, account linking, and user-specific data operations. Based on the Terraform infrastructure, this service integrates with Auth0 for authentication and the Central Rank Authority (CRA) for federated identity management.

## Architecture Components

### 1. Core Technologies
- **Runtime**: Node.js with Express.js framework
- **Language**: TypeScript for type safety
- **API Style**: RESTful API with OpenAPI documentation
- **Authentication**: Auth0 JWT validation through Kong API Gateway
- **Database Options**: 
  - Auth0 User Database (default)
  - Firebase Firestore
  - Supabase PostgreSQL
  - MongoDB

### 2. Service Structure
```
user/
├── src/
│   ├── controllers/        # API endpoint handlers
│   ├── services/          # Business logic
│   ├── models/            # Data models
│   ├── middleware/        # Auth, validation, etc.
│   ├── database/          # Database adapters
│   ├── utils/             # Helper functions
│   └── index.ts           # Application entry point
├── tests/                 # Unit and integration tests
├── docs/                  # API documentation
├── Dockerfile             # Container configuration
└── package.json           # Dependencies
```

## Implementation Details

### 1. Authentication Integration

#### JWT Validation Middleware
```typescript
// src/middleware/auth.ts
import { Request, Response, NextFunction } from 'express';
import jwt from 'jsonwebtoken';
import jwksRsa from 'jwks-rsa';

const jwksClient = jwksRsa({
  jwksUri: `https://${process.env.AUTH0_DOMAIN}/.well-known/jwks.json`
});

export async function validateJWT(req: Request, res: Response, next: NextFunction) {
  const token = req.headers.authorization?.split(' ')[1];
  
  if (!token) {
    return res.status(401).json({ error: 'No token provided' });
  }

  try {
    const decoded = jwt.verify(token, getKey, {
      audience: process.env.AUTH0_AUDIENCE,
      issuer: `https://${process.env.AUTH0_DOMAIN}/`,
      algorithms: ['RS256']
    });
    
    req.user = decoded;
    next();
  } catch (error) {
    res.status(401).json({ error: 'Invalid token' });
  }
}
```

### 2. Database Adapter Pattern

#### Interface Definition
```typescript
// src/database/adapter.interface.ts
export interface UserDatabaseAdapter {
  getUser(userId: string): Promise<UserProfile>;
  updateUser(userId: string, data: Partial<UserProfile>): Promise<UserProfile>;
  createUser(data: UserProfile): Promise<UserProfile>;
  deleteUser(userId: string): Promise<void>;
  linkProvider(userId: string, provider: OAuthProvider): Promise<void>;
  getProviders(userId: string): Promise<OAuthProvider[]>;
}
```

#### Auth0 Implementation
```typescript
// src/database/auth0.adapter.ts
import { ManagementClient } from 'auth0';

export class Auth0Adapter implements UserDatabaseAdapter {
  private client: ManagementClient;

  constructor() {
    this.client = new ManagementClient({
      domain: process.env.AUTH0_DOMAIN,
      clientId: process.env.AUTH0_CLIENT_ID,
      clientSecret: process.env.AUTH0_CLIENT_SECRET,
      scope: 'read:users update:users'
    });
  }

  async getUser(userId: string): Promise<UserProfile> {
    const user = await this.client.getUser({ id: userId });
    return this.mapAuth0UserToProfile(user);
  }

  // Implement other methods...
}
```

### 3. API Endpoints Implementation

#### User Profile Controller
```typescript
// src/controllers/user.controller.ts
import { Request, Response } from 'express';
import { UserService } from '../services/user.service';
import { validateUserUpdate } from '../validators/user.validator';

export class UserController {
  constructor(private userService: UserService) {}

  async getProfile(req: Request, res: Response) {
    try {
      const userId = req.params.id;
      const requesterId = req.user.sub;

      // Ensure user can only access their own profile unless admin
      if (userId !== requesterId && !req.user.permissions?.includes('read:any_user')) {
        return res.status(403).json({ error: 'Forbidden' });
      }

      const profile = await this.userService.getProfile(userId);
      res.json(profile);
    } catch (error) {
      res.status(500).json({ error: 'Internal server error' });
    }
  }

  async updateProfile(req: Request, res: Response) {
    try {
      const userId = req.params.id;
      const requesterId = req.user.sub;

      if (userId !== requesterId) {
        return res.status(403).json({ error: 'Forbidden' });
      }

      const validatedData = validateUserUpdate(req.body);
      const updatedProfile = await this.userService.updateProfile(userId, validatedData);
      res.json(updatedProfile);
    } catch (error) {
      res.status(400).json({ error: error.message });
    }
  }
}
```

### 4. CRA Integration for Account Linking

#### Account Linking Service
```typescript
// src/services/account-linking.service.ts
import axios from 'axios';

export class AccountLinkingService {
  private craEndpoint = process.env.CRA_ENDPOINT;

  async linkAccount(userId: string, provider: string, providerUserId: string) {
    // Call CRA to establish link
    const response = await axios.post(
      `${this.craEndpoint}/link`,
      {
        rootIdentity: userId,
        provider,
        providerUserId
      },
      {
        headers: {
          'Authorization': `Bearer ${await this.getCRAToken()}`,
          'Content-Type': 'application/json'
        }
      }
    );

    return response.data;
  }

  async getLinkedProviders(userId: string): Promise<OAuthProvider[]> {
    const response = await axios.get(
      `${this.craEndpoint}/identities/${userId}/providers`,
      {
        headers: {
          'Authorization': `Bearer ${await this.getCRAToken()}`
        }
      }
    );

    return response.data.providers;
  }

  private async getCRAToken(): Promise<string> {
    // Implement M2M authentication with CRA
    // This would use client credentials flow
  }
}
```

### 5. Security Implementation

#### Rate Limiting
```typescript
// src/middleware/rate-limit.ts
import rateLimit from 'express-rate-limit';
import RedisStore from 'rate-limit-redis';
import Redis from 'ioredis';

const redisClient = new Redis(process.env.REDIS_URL);

export const userApiLimiter = rateLimit({
  store: new RedisStore({
    client: redisClient,
    prefix: 'rl:user:'
  }),
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 100, // Limit each IP to 100 requests per windowMs
  message: 'Too many requests from this IP',
  standardHeaders: true,
  legacyHeaders: false,
});
```

#### Data Encryption
```typescript
// src/utils/encryption.ts
import crypto from 'crypto';

export class EncryptionService {
  private algorithm = 'aes-256-gcm';
  private key: Buffer;

  constructor() {
    this.key = Buffer.from(process.env.ENCRYPTION_KEY, 'hex');
  }

  encrypt(text: string): { encrypted: string; iv: string; tag: string } {
    const iv = crypto.randomBytes(16);
    const cipher = crypto.createCipheriv(this.algorithm, this.key, iv);
    
    let encrypted = cipher.update(text, 'utf8', 'hex');
    encrypted += cipher.final('hex');
    
    const tag = cipher.getAuthTag();
    
    return {
      encrypted,
      iv: iv.toString('hex'),
      tag: tag.toString('hex')
    };
  }

  decrypt(encrypted: string, iv: string, tag: string): string {
    const decipher = crypto.createDecipheriv(
      this.algorithm,
      this.key,
      Buffer.from(iv, 'hex')
    );
    
    decipher.setAuthTag(Buffer.from(tag, 'hex'));
    
    let decrypted = decipher.update(encrypted, 'hex', 'utf8');
    decrypted += decipher.final('utf8');
    
    return decrypted;
  }
}
```

### 6. Environment Configuration

#### Required Environment Variables
```env
# Auth0 Configuration
AUTH0_DOMAIN=your-tenant.auth0.com
AUTH0_AUDIENCE=https://api.nexpo.app
AUTH0_CLIENT_ID=your-client-id
AUTH0_CLIENT_SECRET=your-client-secret

# Database Configuration
DATABASE_TYPE=auth0|firebase|supabase|mongodb
DATABASE_URL=connection-string

# CRA Integration
CRA_ENDPOINT=https://cra.nexpo.app
CRA_CLIENT_ID=user-service-client
CRA_CLIENT_SECRET=secret

# Security
ENCRYPTION_KEY=64-char-hex-key
JWT_PUBLIC_KEY_URL=https://your-tenant.auth0.com/.well-known/jwks.json

# Redis for caching and rate limiting
REDIS_URL=redis://localhost:6379

# Service Configuration
PORT=3001
NODE_ENV=production
LOG_LEVEL=info
```

### 7. Deployment Configuration

#### Dockerfile
```dockerfile
FROM node:18-alpine AS builder

WORKDIR /app
COPY package*.json ./
RUN npm ci --only=production

COPY . .
RUN npm run build

FROM node:18-alpine

WORKDIR /app
COPY --from=builder /app/dist ./dist
COPY --from=builder /app/node_modules ./node_modules
COPY --from=builder /app/package*.json ./

EXPOSE 3001
USER node

CMD ["node", "dist/index.js"]
```

#### Health Check Endpoint
```typescript
// src/routes/health.ts
import { Router } from 'express';
import { databaseHealth, redisHealth } from '../utils/health-checks';

const router = Router();

router.get('/health', async (req, res) => {
  const checks = await Promise.all([
    databaseHealth(),
    redisHealth()
  ]);

  const isHealthy = checks.every(check => check.status === 'healthy');
  
  res.status(isHealthy ? 200 : 503).json({
    status: isHealthy ? 'healthy' : 'unhealthy',
    checks: checks
  });
});
```

### 8. Testing Strategy

#### Unit Test Example
```typescript
// tests/services/user.service.test.ts
import { UserService } from '../../src/services/user.service';
import { Auth0Adapter } from '../../src/database/auth0.adapter';

describe('UserService', () => {
  let userService: UserService;
  let mockAdapter: jest.Mocked<Auth0Adapter>;

  beforeEach(() => {
    mockAdapter = createMockAdapter();
    userService = new UserService(mockAdapter);
  });

  test('should get user profile', async () => {
    const userId = 'auth0|123456';
    const mockProfile = {
      id: userId,
      email: 'test@example.com',
      name: 'Test User'
    };

    mockAdapter.getUser.mockResolvedValue(mockProfile);

    const result = await userService.getProfile(userId);
    
    expect(result).toEqual(mockProfile);
    expect(mockAdapter.getUser).toHaveBeenCalledWith(userId);
  });
});
```

## API Documentation

### OpenAPI Specification
```yaml
openapi: 3.0.0
info:
  title: User Microservice API
  version: 1.0.0
paths:
  /api/v1/users/{id}:
    get:
      summary: Get user profile
      security:
        - bearerAuth: []
      parameters:
        - name: id
          in: path
          required: true
          schema:
            type: string
      responses:
        200:
          description: User profile
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/UserProfile'
    put:
      summary: Update user profile
      security:
        - bearerAuth: []
      parameters:
        - name: id
          in: path
          required: true
          schema:
            type: string
      requestBody:
        content:
          application/json:
            schema:
              $ref: '#/components/schemas/UserUpdateRequest'
      responses:
        200:
          description: Updated profile
```

## Monitoring and Observability

### Logging
```typescript
// src/utils/logger.ts
import winston from 'winston';

export const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  format: winston.format.json(),
  transports: [
    new winston.transports.Console({
      format: winston.format.simple()
    })
  ]
});
```

### Metrics Collection
```typescript
// src/utils/metrics.ts
import { Registry, Counter, Histogram } from 'prom-client';

export const registry = new Registry();

export const httpRequestDuration = new Histogram({
  name: 'http_request_duration_seconds',
  help: 'Duration of HTTP requests in seconds',
  labelNames: ['method', 'route', 'status_code'],
  registers: [registry]
});

export const userOperations = new Counter({
  name: 'user_operations_total',
  help: 'Total number of user operations',
  labelNames: ['operation', 'status'],
  registers: [registry]
});
```

## Next Steps

1. **Set up development environment** with required dependencies
2. **Configure Auth0 application** and obtain credentials
3. **Choose and configure database backend** (Auth0, Firebase, Supabase, or MongoDB)
4. **Implement core features** following the patterns above
5. **Add comprehensive tests** for all endpoints and services
6. **Configure CI/CD pipeline** for automated testing and deployment
7. **Set up monitoring** with Prometheus/Grafana or cloud provider tools
8. **Document API** with Swagger UI for developer experience