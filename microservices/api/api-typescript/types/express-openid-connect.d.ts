declare module 'express-openid-connect' {
  import { Request, Response } from 'express';

  export interface AuthorizationParameters {
    response_type?: string;
    audience?: string;
    scope?: string;
    redirect_uri?: string;
  }

  export interface ConfigParams {
    authRequired?: boolean;
    auth0Logout?: boolean;
    secret: string;
    baseURL: string;
    clientID: string;
    issuerBaseURL: string;
    clientSecret: string;
    authorizationParams?: AuthorizationParameters;
    routes?: {
      login?: boolean | string;
      logout?: boolean | string;
      callback?: boolean | string;
    };
  }

  export interface User {
    sub: string;
    name?: string;
    email?: string;
    picture?: string;
    [key: string]: any;
  }

  export interface OpenIdRequest extends Request {
    oidc: {
      isAuthenticated(): boolean;
      user?: User;
      accessToken?: {
        access_token: string;
        scope: string;
        expires_in: number;
        token_type: string;
      };
    };
  }

  export interface OpenIdResponse extends Response {
    oidc: {
      login(options?: {
        returnTo?: string;
        authorizationParams?: AuthorizationParameters;
      }): void;
      logout(options?: {
        returnTo?: string;
      }): void;
      callback(options?: {
        redirectUri?: string;
      }): void;
    };
  }

  export function auth(config: ConfigParams): any;
}

// Extend Express Request and Response interfaces
declare global {
  namespace Express {
    interface Request {
      oidc: {
        isAuthenticated(): boolean;
        user?: import('express-openid-connect').User;
        accessToken?: {
          access_token: string;
          scope: string;
          expires_in: number;
          token_type: string;
        };
      };
    }

    interface Response {
      oidc: {
        login(options?: {
          returnTo?: string;
          authorizationParams?: import('express-openid-connect').AuthorizationParameters;
        }): void;
        logout(options?: {
          returnTo?: string;
        }): void;
        callback(options?: {
          redirectUri?: string;
        }): void;
      };
    }
  }
}
