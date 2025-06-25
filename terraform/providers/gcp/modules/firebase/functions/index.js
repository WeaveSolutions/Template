const functions = require('@google-cloud/functions-framework');
const admin = require('firebase-admin');
const jwt = require('jsonwebtoken');
const jwksRsa = require('jwks-rsa');

// Initialize Firebase Admin
admin.initializeApp({
  projectId: process.env.FIREBASE_PROJECT_ID,
});

// JWKS client for Auth0 token verification
const client = jwksRsa({
  jwksUri: `https://${process.env.AUTH0_DOMAIN}/.well-known/jwks.json`,
  cache: true,
  rateLimit: true,
});

function getKey(header, callback) {
  client.getSigningKey(header.kid, (err, key) => {
    if (err) {
      callback(err);
    } else {
      const signingKey = key.publicKey || key.rsaPublicKey;
      callback(null, signingKey);
    }
  });
}

/**
 * Exchange Auth0 token for Firebase custom token
 */
functions.http('exchangeToken', async (req, res) => {
  // Enable CORS
  res.set('Access-Control-Allow-Origin', '*');
  res.set('Access-Control-Allow-Methods', 'POST, OPTIONS');
  res.set('Access-Control-Allow-Headers', 'Content-Type, Authorization');
  
  if (req.method === 'OPTIONS') {
    res.status(204).send('');
    return;
  }

  if (req.method !== 'POST') {
    res.status(405).send('Method Not Allowed');
    return;
  }

  try {
    // Extract Auth0 token from Authorization header
    const authHeader = req.headers.authorization;
    if (!authHeader || !authHeader.startsWith('Bearer ')) {
      res.status(401).json({ error: 'Missing or invalid authorization header' });
      return;
    }

    const auth0Token = authHeader.substring(7);

    // Verify Auth0 token
    const decoded = await new Promise((resolve, reject) => {
      jwt.verify(
        auth0Token,
        getKey,
        {
          audience: process.env.AUTH0_API_IDENTIFIER,
          issuer: `https://${process.env.AUTH0_DOMAIN}/`,
          algorithms: ['RS256'],
        },
        (err, decoded) => {
          if (err) reject(err);
          else resolve(decoded);
        }
      );
    });

    // Extract user information and custom claims
    const userId = decoded.sub;
    const email = decoded.email;
    const emailVerified = decoded.email_verified || false;
    const name = decoded.name || decoded.nickname || email;
    const picture = decoded.picture;
    
    // Extract custom claims (roles, permissions)
    const namespace = 'https://auth0.com/';
    const roles = decoded[`${namespace}roles`] || [];
    const permissions = decoded[`${namespace}permissions`] || [];

    // Create or update Firebase user
    let firebaseUser;
    try {
      firebaseUser = await admin.auth().getUser(userId);
      // Update user if needed
      await admin.auth().updateUser(userId, {
        email,
        emailVerified,
        displayName: name,
        photoURL: picture,
      });
    } catch (error) {
      if (error.code === 'auth/user-not-found') {
        // Create new user
        firebaseUser = await admin.auth().createUser({
          uid: userId,
          email,
          emailVerified,
          displayName: name,
          photoURL: picture,
        });
      } else {
        throw error;
      }
    }

    // Create custom claims for Firebase
    const customClaims = {
      auth0Id: userId,
      email,
      emailVerified,
      roles,
      permissions,
      provider: 'auth0',
    };

    // Set custom claims on the user
    await admin.auth().setCustomUserClaims(userId, customClaims);

    // Create custom token
    const firebaseToken = await admin.auth().createCustomToken(userId, customClaims);

    // Sync user data to Firestore
    const userRef = admin.firestore().collection('users').doc(userId);
    await userRef.set({
      uid: userId,
      email,
      emailVerified,
      displayName: name,
      photoURL: picture,
      roles,
      permissions,
      provider: 'auth0',
      lastLogin: admin.firestore.FieldValue.serverTimestamp(),
      updatedAt: admin.firestore.FieldValue.serverTimestamp(),
    }, { merge: true });

    // Return the Firebase token
    res.status(200).json({
      token: firebaseToken,
      expiresIn: 3600, // 1 hour
      user: {
        uid: userId,
        email,
        displayName: name,
        photoURL: picture,
        roles,
        permissions,
      },
    });

  } catch (error) {
    console.error('Token exchange error:', error);
    
    if (error.name === 'TokenExpiredError') {
      res.status(401).json({ error: 'Auth0 token expired' });
    } else if (error.name === 'JsonWebTokenError') {
      res.status(401).json({ error: 'Invalid Auth0 token' });
    } else {
      res.status(500).json({ error: 'Token exchange failed' });
    }
  }
});

/**
 * Webhook to sync Auth0 user updates to Firebase
 */
functions.http('auth0Webhook', async (req, res) => {
  // Verify webhook signature (implement based on Auth0 webhook security)
  // const signature = req.headers['x-auth0-signature'];
  // if (!verifyWebhookSignature(req.body, signature)) {
  //   res.status(401).send('Invalid signature');
  //   return;
  // }

  try {
    const { type, data } = req.body;

    switch (type) {
      case 'user.updated':
      case 'user.created': {
        const { user_id, email, name, picture, app_metadata, user_metadata } = data;
        
        // Update Firebase Auth
        await admin.auth().updateUser(user_id, {
          email,
          displayName: name,
          photoURL: picture,
        });

        // Update custom claims if roles/permissions changed
        if (app_metadata?.roles || app_metadata?.permissions) {
          await admin.auth().setCustomUserClaims(user_id, {
            roles: app_metadata.roles || [],
            permissions: app_metadata.permissions || [],
          });
        }

        // Update Firestore
        await admin.firestore().collection('users').doc(user_id).set({
          email,
          displayName: name,
          photoURL: picture,
          metadata: user_metadata || {},
          updatedAt: admin.firestore.FieldValue.serverTimestamp(),
        }, { merge: true });

        break;
      }

      case 'user.deleted': {
        const { user_id } = data;
        
        // Delete from Firebase Auth
        await admin.auth().deleteUser(user_id);
        
        // Mark as deleted in Firestore (soft delete)
        await admin.firestore().collection('users').doc(user_id).update({
          deleted: true,
          deletedAt: admin.firestore.FieldValue.serverTimestamp(),
        });

        break;
      }
    }

    res.status(200).json({ success: true });
  } catch (error) {
    console.error('Webhook processing error:', error);
    res.status(500).json({ error: 'Webhook processing failed' });
  }
});
