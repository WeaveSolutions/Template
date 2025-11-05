import { Router, Request, Response } from 'express';
import axios from 'axios';
import { auditLogger } from '../middleware/logger';

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
 * Get user profile from Auth0
 */
router.get('/profile', async (req: Request, res: Response): Promise<void> => {
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
    
    res.json({
      user_id: response.data.user_id,
      email: response.data.email,
      name: response.data.name,
      picture: response.data.picture,
      email_verified: response.data.email_verified,
      user_metadata: response.data.user_metadata || {},
      app_metadata: response.data.app_metadata || {},
      created_at: response.data.created_at,
      updated_at: response.data.updated_at,
      last_login: response.data.last_login,
      logins_count: response.data.logins_count
    });
  } catch (error) {
    console.error('Profile fetch error:', error);
    res.status(500).json({ error: 'Failed to fetch user profile' });
  }
});

/**
 * Update user metadata
 */
router.patch('/metadata', async (req: Request, res: Response): Promise<void> => {
  try {
    const userId = req.oidc?.user?.sub;
    if (!userId) {
      res.status(401).json({ error: 'Not authenticated' });
      return;
    }
    
    const { user_metadata } = req.body;
    
    if (!user_metadata || typeof user_metadata !== 'object') {
      res.status(400).json({ error: 'Invalid user_metadata' });
      return;
    }
    
    const token = await getManagementToken();
    
    const response = await axios.patch(
      `${process.env.AUTH0_ISSUER_BASE_URL}/api/v2/users/${encodeURIComponent(userId)}`,
      { user_metadata },
      {
        headers: { Authorization: `Bearer ${token}` }
      }
    );
    
    // Audit log
    auditLogger('user_metadata_update', userId, {
      changes: Object.keys(user_metadata),
      ip: req.ip
    });
    
    res.json({
      user_metadata: response.data.user_metadata
    });
  } catch (error) {
    console.error('Metadata update error:', error);
    res.status(500).json({ error: 'Failed to update user metadata' });
  }
});

/**
 * Get user roles
 */
router.get('/roles', async (req: Request, res: Response): Promise<void> => {
  try {
    const userId = req.oidc?.user?.sub;
    if (!userId) {
      res.status(401).json({ error: 'Not authenticated' });
      return;
    }
    
    const token = await getManagementToken();
    
    const response = await axios.get(
      `${process.env.AUTH0_ISSUER_BASE_URL}/api/v2/users/${encodeURIComponent(userId)}/roles`,
      {
        headers: { Authorization: `Bearer ${token}` }
      }
    );
    
    res.json({
      roles: response.data
    });
  } catch (error) {
    console.error('Roles fetch error:', error);
    res.status(500).json({ error: 'Failed to fetch user roles' });
  }
});

/**
 * Get user permissions
 */
router.get('/permissions', async (req: Request, res: Response): Promise<void> => {
  try {
    const userId = req.oidc?.user?.sub;
    if (!userId) {
      res.status(401).json({ error: 'Not authenticated' });
      return;
    }
    
    const token = await getManagementToken();
    
    const response = await axios.get(
      `${process.env.AUTH0_ISSUER_BASE_URL}/api/v2/users/${encodeURIComponent(userId)}/permissions`,
      {
        headers: { Authorization: `Bearer ${token}` }
      }
    );
    
    res.json({
      permissions: response.data
    });
  } catch (error) {
    console.error('Permissions fetch error:', error);
    res.status(500).json({ error: 'Failed to fetch user permissions' });
  }
});

/**
 * Delete user account (GDPR compliance)
 */
router.delete('/account', async (req: Request, res: Response): Promise<void> => {
  try {
    const userId = req.oidc?.user?.sub;
    if (!userId) {
      res.status(401).json({ error: 'Not authenticated' });
      return;
    }
    
    const { confirmation_code } = req.body;
    
    if (!confirmation_code || confirmation_code !== 'DELETE') {
      res.status(400).json({ 
        error: 'Confirmation required',
        message: 'Please provide confirmation_code: "DELETE"'
      });
      return;
    }
    
    const token = await getManagementToken();
    
    // Audit log before deletion
    auditLogger('account_deletion_request', userId, {
      ip: req.ip,
      user_agent: req.get('user-agent'),
      confirmation: true
    });
    
    // Delete user from Auth0
    await axios.delete(
      `${process.env.AUTH0_ISSUER_BASE_URL}/api/v2/users/${encodeURIComponent(userId)}`,
      {
        headers: { Authorization: `Bearer ${token}` }
      }
    );
    
    // Clear session
    req.session?.destroy(() => {
      res.json({
        success: true,
        message: 'Account successfully deleted'
      });
    });
    
  } catch (error) {
    console.error('Account deletion error:', error);
    res.status(500).json({ error: 'Failed to delete account' });
  }
});

export default router;
