import { NextApiRequest, NextApiResponse } from 'next';

// Placeholder Auth API - Configure Auth0 environment variables to enable
export default function handler(req: NextApiRequest, res: NextApiResponse) {
  const { auth0 } = req.query;
  const route = Array.isArray(auth0) ? auth0[0] : auth0;

  // Check if Auth0 is configured
  const isAuth0Configured = 
    process.env.AUTH0_SECRET &&
    process.env.AUTH0_BASE_URL &&
    process.env.AUTH0_ISSUER_BASE_URL &&
    process.env.AUTH0_CLIENT_ID &&
    process.env.AUTH0_CLIENT_SECRET;

  if (!isAuth0Configured) {
    return res.status(501).json({ 
      error: 'Auth0 not configured',
      message: 'Please configure AUTH0_* environment variables to enable authentication',
      route: route
    });
  }

  // TODO: Import and use handleAuth from '@auth0/nextjs-auth0' when Auth0 is configured
  // For now, return placeholder response
  switch (route) {
    case 'login':
      // Redirect to dashboard for demo purposes
      return res.redirect(302, '/dashboard');
    
    case 'logout':
      return res.redirect(302, '/');
    
    case 'callback':
      return res.redirect(302, '/dashboard');
    
    case 'me':
      return res.json({ user: null });
    
    default:
      return res.status(404).json({ error: 'Not found' });
  }
}
