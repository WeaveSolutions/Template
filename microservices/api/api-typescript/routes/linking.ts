import { Router, Request, Response } from 'express';
import axios from 'axios';
import { auditLogger, securityLogger } from '../middleware/logger';

// Extend session interface to include linkingContext
declare module 'express-session' {
  interface SessionData {
    linkingContext?: {
      provider: string;
      primary_user_id: string;
      redirect_uri: string;
    };
  }
}

const router: Router = Router();

/**
 * Get Auth0 Management API token
 */
async function getManagementToken(): Promise<string> {
  const response = await axios.post(`${process.env.AUTH0_ISSUER_BASE_URL}/oauth/token`, {
    client_id: process.env.AUTH0_CLIENT_ID,
    client_secret: process.env.AUTH0_CLIENT_SECRET,
    audience: `${process.env.AUTH0_ISSUER_BASE_URL}/api/v2/`,
    grant_type: 'client_credentials'
  });
  
  return response.data.access_token;
}

/**
 * Get linked accounts
 */
router.get('/accounts', async (req: Request, res: Response): Promise<void> => {
  try {
    const userId = req.oidc?.user?.sub;
    if (!userId) {
      res.status(401).json({ error: 'Not authenticated' });
      return;
    }
    
    const token = await getManagementToken();
    
    const response = await axios.get(
      `${process.env.AUTH0_ISSUER_BASE_URL}/api/v2/users/${encodeURIComponent(userId)}`,
      {
        headers: { Authorization: `Bearer ${token}` }
      }
    );
    
    const identities = response.data.identities || [];
    
    res.json({
      primary_identity: {
        provider: identities[0]?.provider,
        user_id: identities[0]?.user_id,
        connection: identities[0]?.connection
      },
      linked_accounts: identities.slice(1).map((identity: any) => ({
        provider: identity.provider,
        user_id: identity.user_id,
        connection: identity.connection,
        is_social: identity.isSocial,
        linked_at: identity.profileData?.linked_at
      })),
      total_linked: identities.length - 1
    });
  } catch (error) {
    console.error('Linked accounts fetch error:', error);
    res.status(500).json({ error: 'Failed to fetch linked accounts' });
  }
});

/**
 * Link a new account
 */
router.post('/link', async (req: Request, res: Response): Promise<void> => {
  try {
    const primaryUserId = req.oidc?.user?.sub;
    if (!primaryUserId) {
      res.status(401).json({ error: 'Not authenticated' });
      return;
    }
    
    const { secondary_jwt } = req.body;
    
    if (!secondary_jwt) {
      res.status(400).json({ error: 'secondary_jwt is required' });
      return;
    }
    
    const token = await getManagementToken();
    
    // Link accounts
    const response = await axios.post(
      `${process.env.AUTH0_ISSUER_BASE_URL}/api/v2/users/${encodeURIComponent(primaryUserId)}/identities`,
      {
        link_with: secondary_jwt
      },
      {
        headers: { Authorization: `Bearer ${token}` }
      }
    );
    
    // Audit log
    auditLogger('account_linked', primaryUserId, {
      secondary_provider: response.data[0]?.provider,
      secondary_user_id: response.data[0]?.user_id,
      ip: req.ip
    });
    
    // Security log
    securityLogger('account_linking', 'low', {
      primary_user: primaryUserId,
      action: 'link',
      success: true
    });
    
    res.json({
      success: true,
      linked_identity: response.data[0]
    });
  } catch (error) {
    console.error('Account linking error:', error);
    
    // Security log for failed attempt
    securityLogger('account_linking_failed', 'medium', {
      primary_user: req.oidc?.user?.sub,
      error: error instanceof Error ? error.message : 'Unknown error'
    });
    
    res.status(500).json({ error: 'Failed to link account' });
  }
});

/**
 * Unlink an account
 */
router.delete('/unlink/:provider/:user_id', async (req: Request, res: Response): Promise<void> => {
  try {
    const primaryUserId = req.oidc?.user?.sub;
    if (!primaryUserId) {
      res.status(401).json({ error: 'Not authenticated' });
      return;
    }
    
    const { provider, user_id } = req.params;
    
    const token = await getManagementToken();
    
    // Unlink account
    await axios.delete(
      `${process.env.AUTH0_ISSUER_BASE_URL}/api/v2/users/${encodeURIComponent(primaryUserId)}/identities/${provider}/${user_id}`,
      {
        headers: { Authorization: `Bearer ${token}` }
      }
    );
    
    // Audit log
    auditLogger('account_unlinked', primaryUserId, {
      unlinked_provider: provider,
      unlinked_user_id: user_id,
      ip: req.ip
    });
    
    res.json({
      success: true,
      message: 'Account unlinked successfully'
    });
  } catch (error) {
    console.error('Account unlinking error:', error);
    res.status(500).json({ error: 'Failed to unlink account' });
  }
});

/**
 * Generate account linking URL
 */
router.get('/link-url/:provider', (req: Request, res: Response): void => {
  const { provider } = req.params;
  const { redirect_uri } = req.query;
  
  // Validate provider
  const validProviders = ['google-oauth2', 'facebook', 'apple', 'github', 'microsoft'];
  if (!validProviders.includes(provider)) {
    res.status(400).json({ error: 'Invalid provider' });
    return;
  }
  
  // Build Auth0 authorization URL for linking
  const authUrl = new URL(`${process.env.AUTH0_ISSUER_BASE_URL}/authorize`);
  authUrl.searchParams.set('response_type', 'code');
  authUrl.searchParams.set('client_id', process.env.AUTH0_CLIENT_ID!);
  authUrl.searchParams.set('redirect_uri', `${process.env.AUTH0_BASE_URL}/api/linking/callback`);
  authUrl.searchParams.set('scope', 'openid');
  authUrl.searchParams.set('connection', provider);
  authUrl.searchParams.set('access_type', 'offline');
  authUrl.searchParams.set('prompt', 'login');
  
  // Store linking context in session
  if (req.session) {
    req.session.linkingContext = {
      provider,
      primary_user_id: req.oidc?.user?.sub || '',
      redirect_uri: redirect_uri as string || process.env.FRONTEND_URL || ''
    };
  }
  
  res.json({
    link_url: authUrl.toString()
  });
});

/**
 * Handle linking callback
 */
router.get('/callback', async (req: Request, res: Response): Promise<void> => {
  try {
    const { code } = req.query;
    const linkingContext = req.session?.linkingContext;
    
    if (!code || !linkingContext) {
      res.status(400).json({ error: 'Invalid linking callback' });
      return;
    }
    
    // Exchange code for token
    const tokenResponse = await axios.post(`${process.env.AUTH0_ISSUER_BASE_URL}/oauth/token`, {
      grant_type: 'authorization_code',
      client_id: process.env.AUTH0_CLIENT_ID,
      client_secret: process.env.AUTH0_CLIENT_SECRET,
      code,
      redirect_uri: `${process.env.AUTH0_BASE_URL}/api/linking/callback`
    });
    
    // Get the secondary JWT
    const secondary_jwt = tokenResponse.data.id_token;
    
    // Link accounts using Management API
    const mgmtToken = await getManagementToken();
    
    await axios.post(
      `${process.env.AUTH0_ISSUER_BASE_URL}/api/v2/users/${encodeURIComponent(linkingContext.primary_user_id)}/identities`,
      {
        link_with: secondary_jwt
      },
      {
        headers: { Authorization: `Bearer ${mgmtToken}` }
      }
    );
    
    // Clear linking context
    if (req.session) {
      delete req.session.linkingContext;
    }
    
    // Redirect to frontend
    res.redirect(linkingContext.redirect_uri);
  } catch (error) {
    console.error('Linking callback error:', error);
    res.status(500).json({ error: 'Failed to complete account linking' });
  }
});

export default router;
