export const AUTH0_DOMAIN = process.env.EXPO_PUBLIC_AUTH0_DOMAIN || '';
export const CLIENT_ID = process.env.EXPO_PUBLIC_AUTH0_CLIENT_ID || '';
export const AUDIENCE = process.env.EXPO_PUBLIC_AUTH0_AUDIENCE || '';

if (!AUTH0_DOMAIN || !CLIENT_ID || !AUDIENCE) {
  console.warn('Missing required Auth0 environment variables');
}

export const AUTH0_ISSUER = `https://${AUTH0_DOMAIN}`;
export const AUTH0_AUTHORIZE_URL = `${AUTH0_ISSUER}/authorize`;
export const AUTH0_TOKEN_URL = `${AUTH0_ISSUER}/oauth/token`;
// Scopes define the permissions your app is requesting
export const SCOPES = 'openid profile email offline_access';
