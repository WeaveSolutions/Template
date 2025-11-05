import { Router, Request, Response } from 'express';
import axios from 'axios';
import { auditLogger } from '../middleware/logger';

const router: Router = Router();

/**
 * @swagger
 * /api/auth/status:
 *   get:
 *     tags:
 *       - auth
 *     summary: Get authentication status
 *     description: Returns the current authentication status and user information if authenticated
 *     responses:
 *       200:
 *         description: Authentication status retrieved
 *         content:
 *           application/json:
 *             schema:
 *               type: object
 *               properties:
 *                 authenticated:
 *                   type: boolean
 *                   example: true
 *                 user:
 *                   type: object
 *                   properties:
 *                     sub:
 *                       type: string
 *                       example: "auth0|123456"
 *                     email:
 *                       type: string
 *                       example: "user@example.com"
 *                     name:
 *                       type: string
 *                       example: "John Doe"
 *                     picture:
 *                       type: string
 *                       example: "https://example.com/avatar.jpg"
 */
router.get('/status', (req: Request, res: Response): void => {
  const isAuthenticated = req.oidc?.isAuthenticated() || false;
  const user = isAuthenticated ? req.oidc?.user : null;
  
  res.json({
    authenticated: isAuthenticated,
    user: user ? {
      sub: user.sub,
      email: user.email,
      name: user.name,
      picture: user.picture
    } : null
  });
});

/**
 * @swagger
 * /api/auth/token:
 *   get:
 *     tags:
 *       - auth
 *     summary: Get access token
 *     description: Returns the current user's access token for API calls
 *     security:
 *       - cookieAuth: []
 *     responses:
 *       200:
 *         description: Access token retrieved
 *         content:
 *           application/json:
 *             schema:
 *               type: object
 *               properties:
 *                 access_token:
 *                   type: string
 *                   example: "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9..."
 *                 token_type:
 *                   type: string
 *                   example: "Bearer"
 *                 expires_in:
 *                   type: integer
 *                   example: 3600
 *       401:
 *         description: Not authenticated
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/ErrorResponse'
 *       500:
 *         description: Failed to fetch access token
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/ErrorResponse'
 */
router.get('/token', async (req: Request, res: Response): Promise<void> => {
  try {
    if (!req.oidc?.isAuthenticated()) {
      res.status(401).json({ error: 'Not authenticated' });
      return;
    }

    const tokenSet = req.oidc.accessToken;
    
    res.json({
      access_token: tokenSet?.access_token,
      token_type: 'Bearer',
      expires_in: tokenSet?.expires_in
    });
  } catch (error) {
    console.error('Token fetch error:', error);
    res.status(500).json({ error: 'Failed to fetch access token' });
  }
});

/**
 * @swagger
 * /api/auth/providers:
 *   get:
 *     tags:
 *       - auth
 *     summary: List authentication providers
 *     description: Returns list of available OAuth providers and authentication methods
 *     responses:
 *       200:
 *         description: Providers list retrieved
 *         content:
 *           application/json:
 *             schema:
 *               type: object
 *               properties:
 *                 providers:
 *                   type: array
 *                   items:
 *                     type: object
 *                     properties:
 *                       id:
 *                         type: string
 *                         example: "google-oauth2"
 *                       name:
 *                         type: string
 *                         example: "Google"
 *                       icon:
 *                         type: string
 *                         example: "google"
 *                 passwordless:
 *                   type: object
 *                   properties:
 *                     email:
 *                       type: boolean
 *                       example: true
 *                     sms:
 *                       type: boolean
 *                       example: false
 *                 passkeys:
 *                   type: boolean
 *                   example: false
 */
router.get('/providers', (req: Request, res: Response): void => {
  const providers = [];
  
  if (process.env.ENABLE_GOOGLE_OAUTH === 'true') {
    providers.push({ id: 'google-oauth2', name: 'Google', icon: 'google' });
  }
  if (process.env.ENABLE_FACEBOOK_OAUTH === 'true') {
    providers.push({ id: 'facebook', name: 'Facebook', icon: 'facebook' });
  }
  if (process.env.ENABLE_APPLE_OAUTH === 'true') {
    providers.push({ id: 'apple', name: 'Apple', icon: 'apple' });
  }
  if (process.env.ENABLE_GITHUB_OAUTH === 'true') {
    providers.push({ id: 'github', name: 'GitHub', icon: 'github' });
  }
  if (process.env.ENABLE_MICROSOFT_OAUTH === 'true') {
    providers.push({ id: 'microsoft', name: 'Microsoft', icon: 'microsoft' });
  }
  
  res.json({
    providers,
    passwordless: {
      email: process.env.ENABLE_PASSWORDLESS_EMAIL === 'true',
      sms: process.env.ENABLE_PASSWORDLESS_SMS === 'true'
    },
    passkeys: process.env.ENABLE_PASSKEYS === 'true'
  });
});

/**
 * @swagger
 * /api/auth/social/{provider}:
 *   get:
 *     tags:
 *       - auth
 *     summary: Initiate social login
 *     description: Redirects to the specified OAuth provider for authentication
 *     parameters:
 *       - in: path
 *         name: provider
 *         required: true
 *         schema:
 *           type: string
 *           enum: [google-oauth2, facebook, apple, github, microsoft]
 *         description: OAuth provider identifier
 *       - in: query
 *         name: redirect_uri
 *         schema:
 *           type: string
 *         description: URI to redirect to after successful authentication
 *     responses:
 *       302:
 *         description: Redirect to OAuth provider
 *       400:
 *         description: Invalid provider
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/ErrorResponse'
 */
router.get('/social/:provider', (req: Request, res: Response): void => {
  const { provider } = req.params;
  const { redirect_uri } = req.query;
  
  // Validate provider
  const validProviders = ['google-oauth2', 'facebook', 'apple', 'github', 'microsoft'];
  if (!validProviders.includes(provider)) {
    res.status(400).json({ error: 'Invalid provider' });
    return;
  }
  
  // Build Auth0 authorization URL
  const authUrl = new URL(`${process.env.AUTH0_ISSUER_BASE_URL}/authorize`);
  authUrl.searchParams.set('response_type', 'code');
  authUrl.searchParams.set('client_id', process.env.AUTH0_CLIENT_ID!);
  authUrl.searchParams.set('redirect_uri', `${process.env.AUTH0_BASE_URL}/auth/callback`);
  authUrl.searchParams.set('scope', 'openid profile email offline_access');
  authUrl.searchParams.set('connection', provider);
  
  if (redirect_uri && typeof redirect_uri === 'string') {
    authUrl.searchParams.set('state', Buffer.from(JSON.stringify({ redirect_uri })).toString('base64'));
  }
  
  res.redirect(authUrl.toString());
});

/**
 * @swagger
 * /api/auth/login:
 *   get:
 *     tags:
 *       - auth
 *     summary: Initiate login
 *     description: Redirects to Auth0 for authentication
 *     parameters:
 *       - in: query
 *         name: returnTo
 *         schema:
 *           type: string
 *         description: URL to return to after successful login
 *     responses:
 *       302:
 *         description: Redirect to Auth0 login page
 */
router.get('/login', (req: Request, res: Response): void => {
  const { returnTo } = req.query;
  
  const loginUrl = new URL(`${process.env.AUTH0_BASE_URL}/auth/login`);
  if (returnTo && typeof returnTo === 'string') {
    loginUrl.searchParams.set('returnTo', returnTo);
  }
  
  res.redirect(loginUrl.toString());
});

/**
 * @swagger
 * /api/auth/logout:
 *   post:
 *     tags:
 *       - auth
 *     summary: Logout user
 *     description: Logs out the current user and terminates their session
 *     security:
 *       - cookieAuth: []
 *     responses:
 *       200:
 *         description: Successfully logged out
 *       302:
 *         description: Redirect to logout URL
 */
router.post('/logout', async (req: Request, res: Response): Promise<void> => {
  const userId = req.oidc?.user?.sub;
  const sessionId = req.session?.id;
  
  // Audit log the logout
  if (userId) {
    auditLogger('user_logout', userId, {
      session_id: sessionId,
      ip: req.ip,
      user_agent: req.get('user-agent')
    });
  }
  
  // Use oidc logout
  res.oidc?.logout({
    returnTo: process.env.FRONTEND_URL || '/'
  });
});

/**
 * @swagger
 * /api/auth/session:
 *   get:
 *     tags:
 *       - auth
 *     summary: Get session information
 *     description: Returns detailed information about the current user session
 *     security:
 *       - cookieAuth: []
 *     responses:
 *       200:
 *         description: Session information retrieved
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/SessionResponse'
 *       401:
 *         description: Not authenticated
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/ErrorResponse'
 */
router.get('/session', (req: Request, res: Response): void => {
  if (!req.oidc?.isAuthenticated()) {
    res.status(401).json({ error: 'Not authenticated' });
    return;
  }
  
  const session = req.session;
  const oidcContext = req.oidc;
  const maxAge = session?.cookie?.maxAge;
  
  res.json({
    session_id: session?.id,
    created_at: session?.cookie?.originalMaxAge ? 
      new Date(Date.now() - (session.cookie.originalMaxAge - (maxAge || 0))).toISOString() : null,
    expires_at: session?.cookie?.expires?.toISOString() || null,
    last_accessed: new Date().toISOString(),
    user: {
      sub: oidcContext?.user?.sub,
      email: oidcContext?.user?.email
    }
  });
});

export default router;
