import { Request, Response, NextFunction } from 'express';

export interface UserPayload {
  id: string;
  email: string;
  roles: string[];
  [key: string]: any;
}

export interface RequestWithUser extends Request {
  user?: UserPayload;
  id?: string;
  session?: {
    id: string;
    data: any;
    destroy: () => Promise<boolean>;
    touch: () => Promise<boolean>;
    save: () => Promise<boolean>;
  };
}

export interface AuthTokenPayload {
  sub: string;
  email: string;
  email_verified: boolean;
  name?: string;
  picture?: string;
  given_name?: string;
  family_name?: string;
  'https://your-namespace/roles'?: string[];
  [key: string]: any;
}

export interface SessionData {
  userId: string;
  email: string;
  roles: string[];
  createdAt: number;
  lastAccessed: number;
  userAgent?: string;
  ipAddress?: string;
  [key: string]: any;
}

export interface RateLimitConfig {
  windowMs: number;
  max: number;
  message?: string;
  statusCode?: number;
  keyGenerator?: (req: Request) => string;
  skip?: (req: Request, res: Response) => boolean;
  handler?: (req: Request, res: Response, next: NextFunction) => void;
  onLimitReached?: (req: Request, res: Response, options: any) => void;
}

export interface SecurityHeadersConfig {
  csp?: {
    directives: {
      [key: string]: string[] | boolean | string | Function;
    };
  };
  hsts?: boolean | {
    maxAge?: number;
    includeSubDomains?: boolean;
    preload?: boolean;
  };
  xssFilter?: boolean;
  noSniff?: boolean;
  frameguard?: boolean | {
    action?: string;
  };
  hidePoweredBy?: boolean;
  referrerPolicy?: boolean | {
    policy?: string | string[];
  };
  crossOriginEmbedderPolicy?: boolean | {
    policy?: string;
  };
  crossOriginOpenerPolicy?: boolean | {
    policy?: string;
  };
  crossOriginResourcePolicy?: boolean | {
    policy?: string;
  };
  dnsPrefetchControl?: boolean | {
    allow?: boolean;
  };
  ieNoOpen?: boolean;
  permittedCrossDomainPolicies?: boolean | {
    permittedPolicies?: string;
  };
}

export interface LogEntry {
  level: string;
  message: string;
  timestamp?: string;
  requestId?: string;
  userId?: string;
  method?: string;
  url?: string;
  statusCode?: number;
  responseTime?: string;
  contentLength?: string;
  ip?: string;
  userAgent?: string;
  error?: Error;
  stack?: string;
  [key: string]: any;
}

export interface Auth0UserMetadata {
  user_id: string;
  email: string;
  email_verified: boolean;
  name?: string;
  nickname?: string;
  picture?: string;
  created_at: string;
  updated_at: string;
  last_ip?: string;
  last_login?: string;
  logins_count?: number;
  [key: string]: any;
}

export interface Auth0User extends Auth0UserMetadata {
  user_metadata?: {
    [key: string]: any;
  };
  app_metadata?: {
    [key: string]: any;
    authorization?: {
      roles: string[];
      permissions: string[];
    };
  };
  identities?: Array<{
    connection: string;
    user_id: string;
    provider: string;
    isSocial: boolean;
  }>;
}

export interface Auth0TokenResponse {
  access_token: string;
  id_token: string;
  refresh_token?: string;
  expires_in: number;
  token_type: string;
  scope?: string;
}
