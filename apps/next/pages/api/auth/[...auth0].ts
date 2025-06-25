import { handleAuth } from '@auth0/nextjs-auth0';
import { NextApiRequest, NextApiResponse } from 'next';

export default handleAuth({
  async login(req: NextApiRequest, res: NextApiResponse) {
    try {
      await handleAuth.login(req, res, {
        authorizationParams: {
          audience: process.env.AUTH0_AUDIENCE,
          scope: 'openid profile email offline_access',
        },
      });
    } catch (error: any) {
      res.status(error.status || 400).end(error.message);
    }
  },
  
  async callback(req: NextApiRequest, res: NextApiResponse) {
    try {
      await handleAuth.callback(req, res, { redirectUri: '/' });
    } catch (error: any) {
      res.status(error.status || 400).end(error.message);
    }
  },
  
  async logout(req: NextApiRequest, res: NextApiResponse) {
    try {
      await handleAuth.logout(req, res, {
        returnTo: '/',
      });
    } catch (error: any) {
      res.status(error.status || 400).end(error.message);
    }
  },
  
  async me(req: NextApiRequest, res: NextApiResponse) {
    try {
      await handleAuth.me(req, res);
    } catch (error: any) {
      res.status(error.status || 400).end(error.message);
    }
  },
});
