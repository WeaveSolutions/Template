import { Request, Response, NextFunction } from 'express';
import { auth } from 'express-openid-connect';
import jwt from 'jsonwebtoken';
import jwksRsa from 'jwks-rsa';

// Extend Express Request type
declare global {
  namespace Express {
    interface Request {
      auth0?: any;
      user?: any;
    }
  }
}

// JWKS client for Auth0 public key retrieval
const jwksClient = jwksRsa({
  cache: true,
  rateLimit: true,
  jwksRequestsPerMinute: 5,
  jwksUri: `https://${process.env.AUTH0_ISSUER_BASE_URL}/.well-known/jwks.json`
});

/**
 * Custom JWT validation middleware compatible with Express v5
 * Validates Auth0 JWTs using RS256 algorithm and cached public keys
 */
export const checkJwt = async (req: Request, res: Response, next: NextFunction): Promise<void> => {
  try {
    const token = req.headers.authorization?.replace('Bearer ', '');
    
    if (!token) {
      res.status(401).json({ error: 'No token provided' });
      return;
    }

    // Decode token to get key ID
    const decoded = jwt.decode(token, { complete: true });
    if (!decoded || typeof decoded === 'string') {
      res.status(401).json({ error: 'Invalid token format' });
      return;
    }

    // Get signing key from JWKS
    const signingKey = await new Promise<string>((resolve, reject) => {
      jwksClient.getSigningKey(decoded.header.kid, (err, key) => {
        if (err) reject(err);
        else resolve(key?.getPublicKey() || '');
      });
    });

    // Verify JWT
    const verified = jwt.verify(token, signingKey, {
      audience: process.env.AUTH0_AUDIENCE,
      issuer: `https://${process.env.AUTH0_ISSUER_BASE_URL}/`,
      algorithms: ['RS256']
    });

    // Add user to request
    req.user = verified;
    next();
  } catch (error) {
    res.status(401).json({ 
      error: 'Invalid token',
      message: error instanceof Error ? error.message : 'Token validation failed'
    });
  }
};

/**
 * Auth middleware wrapper for protected routes
 * Checks if Auth0 is configured before applying authentication
 */
export const authMiddleware = async (req: Request, res: Response, next: NextFunction): Promise<void> => {
  // Check if Auth0 is properly configured
  if (!process.env.AUTH0_ISSUER_BASE_URL || !process.env.AUTH0_CLIENT_ID) {
    res.status(503).json({
      error: 'Service Unavailable',
      message: 'Authentication service is not configured'
    });
    return;
  }

  // Apply JWT validation for API routes
  if (req.path.startsWith('/api/')) {
    return checkJwt(req, res, next);
  }

  // For non-API routes, check session-based auth
  if (req.oidc && req.oidc.isAuthenticated()) {
    next();
  } else {
    res.status(401).json({
      error: 'Unauthorized',
      message: 'Authentication required'
    });
  }
};

/**
 * Extract user context from JWT for Kong integration
 */
export const extractUserContext = (req: Request, res: Response, next: NextFunction): void => {
  if (req.user) {
    // Add user context headers for Kong
    res.setHeader('X-User-ID', req.user.sub || '');
    res.setHeader('X-User-Email', req.user.email || '');
    res.setHeader('X-User-Roles', JSON.stringify(req.user['https://cra.com/roles'] || []));
  }
  next();
};

/**
 * Role-based access control middleware
 */
export const requireRole = (role: string) => {
  return (req: Request, res: Response, next: NextFunction): void => {
    const userRoles = req.user?.['https://cra.com/roles'] || [];
    
    if (!userRoles.includes(role)) {
      res.status(403).json({
        error: 'Forbidden',
        message: `Requires ${role} role`
      });
      return;
    }
    
    next();
  };
};

/**
 * Scope-based access control middleware
 */
export const requireScope = (scope: string) => {
  return (req: Request, res: Response, next: NextFunction): void => {
    const tokenScopes = req.user?.scope?.split(' ') || [];
    
    if (!tokenScopes.includes(scope)) {
      res.status(403).json({
        error: 'Forbidden', 
        message: `Requires ${scope} scope`
      });
      return;
    }
    
    next();
  };
};
