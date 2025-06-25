import { OpenidRequest } from 'express-openid-connect';
import { Session } from 'express-session';

declare global {
  namespace Express {
    interface Request {
      oidc?: OpenidRequest['oidc'];
      requestId?: string;
      session?: Session & { id?: string };
      user?: {
        sub: string;
        email?: string;
        name?: string;
        picture?: string;
        [key: string]: any;
      };
    }
    
    interface Response {
      oidc?: {
        logout: (options?: { returnTo?: string }) => void;
      };
      locals: {
        cspNonce?: string;
        [key: string]: any;
      };
    }
  }
}

export {};
