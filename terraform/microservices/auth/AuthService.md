# Auth Service Infrastructure - Auth0 Primary Authentication

This directory contains the Terraform configuration for deploying the Auth Service, which uses **Auth0 as the primary authentication provider** for the Nexpo template.

## Overview
This module manages the authentication infrastructure for the Nexpo platform using Auth0 as the primary identity provider, integrated with the Central Rank Authority (CRA) for federated identity management. It provisions the necessary resources for secure authentication, token management, and API gateway integration with Kong.

## 🔐 Service Overview

The Auth Service provides:
- **Auth0 as primary authentication provider**
- Email/password authentication via Auth0
- Connected logins (Google, GitHub, Facebook, Apple, Discord, X, Slack, Reddit, Amazon, Spotify)
- JWT token generation and validation
- Role-based access control (RBAC) via Auth0
- Session management with Kong API Gateway caching
- User permissions and metadata management
- Multi-tenant support

## 🏗️ Architecture

- **Auth0**: Primary authentication provider supporting multiple OAuth providers (Google, Discord, GitHub, X, Slack, Reddit, Amazon, Spotify, etc.) with MFA and RBAC.
- **Central Rank Authority (CRA)**: Backend service for root identity management, account linking, scope control, and token issuance.
- **Kong API Gateway**: Validates Auth0 JWTs, enforces rate limiting, and routes requests to backend microservices.
- **Vault**: Secure storage for OAuth tokens, service credentials, and signing keys.
- **Temporal**: Workflow orchestration for token refresh and provider connection processes.

```
┌─────────────────┐     ┌──────────────────┐     ┌─────────────┐
│  Mobile/Web App │────▶│ Kong API Gateway │────▶│ Auth Service│
└─────────────────┘     └──────────────────┘     └─────────────┘
                                │                        │
                                │                        │
                                ▼                        ▼
                        ┌─────────────┐          ┌─────────────┐
                        │ Kong Cache  │          │    Auth0    │
                        │ (Proxy/JWT) │          │   (Primary) │
                        └─────────────┘          └─────────────┘
```

## 📁 Directory Structure

```
auth/
├── main.tf              # Main service configuration
├── variables.tf         # Service variables
├── outputs.tf          # Service outputs
├── provider-specific/  # Cloud-specific configs
│   ├── aws.tf         # AWS-specific resources
│   ├── gcp.tf         # GCP-specific resources
│   └── azure.tf       # Azure-specific resources
├── auth0/             # Auth0 specific configs
│   ├── rules.js       # Auth0 rules
│   ├── actions.js     # Auth0 actions
│   └── connections.tf # Auth0 connections
└── README.md          # This file
```

## 🚀 Deployment

### Prerequisites

1. **Auth0 Setup**
   - Create Auth0 tenant at [auth0.com](https://auth0.com)
   - Create a Regular Web Application
   - Create an API for your services
   - Configure allowed callbacks and origins

2. **Kong Configuration**
   - Enable Kong API Gateway
   - Configure Kong caching and JWT validation

### Standalone Deployment

```bash
cd microservices/auth

# Initialize and deploy
terraform init
terraform apply \
  -var="cloud_provider=aws" \
  -var="environment=dev" \
  -var="auth0_domain=your-domain.auth0.com" \
  -var="auth0_client_id=your-client-id" \
  -var="auth0_client_secret=your-secret"
```

### Integrated Deployment

The auth service can be deployed as part of the main infrastructure:

```hcl
# In your main terraform configuration
module "auth_service" {
  source = "./microservices/auth"
  
  cloud_provider = var.enable_aws ? "aws" : var.enable_gcp ? "gcp" : "azure"
  environment    = var.environment
  project_name   = var.project_name
  
  # Networking
  vpc_id         = module.networking.vpc_id
  subnet_ids     = module.networking.private_subnet_ids
  
  # Auth0 Configuration (Primary Auth)
  auth0_domain        = var.auth0_domain
  auth0_client_id     = var.auth0_client_id
  auth0_client_secret = var.auth0_client_secret
  auth0_audience      = var.auth0_audience
  
  # Service Configuration
  enable_social_logins = true
  enable_mfa          = var.environment == "prod"
  enable_rbac         = true
}
```

## 🔧 Configuration

### Required Variables

| Variable | Description | Example |
|----------|-------------|---------|
| `auth0_domain` | Auth0 tenant domain | `myapp.auth0.com` |
| `auth0_client_id` | Auth0 application ID | `abc123...` |
| `auth0_client_secret` | Auth0 application secret | `xyz789...` |
| `auth0_audience` | Auth0 API identifier | `https://api.myapp.com` |

### Optional Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `container_cpu` | CPU allocation | `0.5` |
| `container_memory` | Memory allocation | `1Gi` |
| `min_replicas` | Minimum instances | `2` |
| `max_replicas` | Maximum instances | `20` |
| `cache_ttl_seconds` | Token cache TTL | `300` |
| `enable_social_logins` | Enable social auth | `true` |
| `enable_mfa` | Enable multi-factor auth | `false` |
| `enable_rbac` | Enable role-based access | `true` |

## 📡 API Endpoints

### Primary Authentication (Auth0)

```
POST /auth/login
Body: { "email": "user@example.com", "password": "..." }
Response: { 
  "access_token": "...", 
  "id_token": "...",
  "refresh_token": "...", 
  "expires_in": 3600,
  "user": {
    "id": "auth0|...",
    "email": "...",
    "roles": ["user"],
    "permissions": ["read:profile"]
  }
}

POST /auth/signup
Body: { 
  "email": "user@example.com", 
  "password": "...",
  "metadata": { "name": "John Doe" }
}
Response: { "success": true, "user_id": "auth0|..." }

POST /auth/logout
Headers: { "Authorization": "Bearer <token>" }
Response: { "success": true, "logout_url": "..." }

POST /auth/refresh
Body: { "refresh_token": "..." }
Response: { "access_token": "...", "expires_in": 3600 }

POST /auth/forgot-password
Body: { "email": "user@example.com" }
Response: { "success": true, "message": "Password reset email sent" }
```

### Token Management

```
POST /auth/validate
Headers: { "Authorization": "Bearer <token>" }
Response: { 
  "valid": true, 
  "user_id": "auth0|...", 
  "roles": ["user", "admin"],
  "permissions": ["read:all", "write:all"]
}

GET /auth/me
Headers: { "Authorization": "Bearer <token>" }
Response: { 
  "id": "auth0|...", 
  "email": "...", 
  "email_verified": true,
  "roles": [...], 
  "permissions": [...],
  "metadata": { ... }
}

PATCH /auth/me
Headers: { "Authorization": "Bearer <token>" }
Body: { "metadata": { "name": "Jane Doe" } }
Response: { "success": true, "user": { ... } }
```

### Social Authentication

```
GET /auth/social/{provider}
Providers: google, github, facebook, apple, linkedin, microsoft
Query params: ?redirect_uri=https://app.com/callback
Redirects to Auth0 OAuth flow

GET /auth/callback/{provider}
Handles OAuth callback from Auth0
Returns to redirect_uri with tokens
```

### Role & Permission Management

```
GET /auth/roles
Headers: { "Authorization": "Bearer <admin-token>" }
Response: { "roles": [{ "id": "...", "name": "admin", "permissions": [...] }] }

POST /auth/users/{userId}/roles
Headers: { "Authorization": "Bearer <admin-token>" }
Body: { "roles": ["admin", "moderator"] }
Response: { "success": true }

GET /auth/users/{userId}/permissions
Headers: { "Authorization": "Bearer <admin-token>" }
Response: { "permissions": ["read:all", "write:posts", ...] }
```

## 🔒 Security Features

1. **Auth0 Security**
   - Enterprise-grade authentication
   - Brute force protection
   - Anomaly detection
   - Bot detection
   - Breached password detection

2. **Token Security**
   - RS256 signed JWTs
   - Short-lived access tokens (1 hour)
   - Rotating refresh tokens
   - Secure token storage

3. **Advanced Features**
   - Multi-factor authentication (MFA)
   - Passwordless authentication
   - Single Sign-On (SSO)
   - Universal Login

4. **Rate Limiting**
   - Auth0 built-in rate limits
   - Additional service-level limits
   - DDoS protection

## 📊 Monitoring

The service includes:
- Health check endpoint: `/health`
- Metrics endpoint: `/metrics`
- Auth0 logs integration
- Custom dashboards for each cloud provider

### Key Metrics

- Authentication success/failure rates
- Social login provider usage
- Token validation latency
- Active users count
- Failed login attempts
- MFA adoption rate

## 🧪 Testing

```bash
# Health check
curl https://auth.your-domain.com/health

# Login with Auth0
curl -X POST https://auth.your-domain.com/auth/login \
  -H "Content-Type: application/json" \
  -d '{"email":"test@example.com","password":"Test123!"}'

# Validate token
curl https://auth.your-domain.com/auth/validate \
  -H "Authorization: Bearer YOUR_ACCESS_TOKEN"

# Get user profile
curl https://auth.your-domain.com/auth/me \
  -H "Authorization: Bearer YOUR_ACCESS_TOKEN"
```

## 🔄 Integration Examples

### Next.js Integration

```typescript
// pages/api/auth/[...auth0].ts
import { handleAuth, handleCallback } from '@auth0/nextjs-auth0';

export default handleAuth({
  async callback(req, res) {
    try {
      await handleCallback(req, res, {
        redirectUri: process.env.AUTH0_BASE_URL
      });
    } catch (error) {
      res.status(error.status || 500).end(error.message);
    }
  }
});
```

### React Native / Expo Integration

```typescript
// auth/useAuth.ts
import * as AuthSession from 'expo-auth-session';
import { Platform } from 'react-native';

const useAuth0 = () => {
  const redirectUri = AuthSession.makeRedirectUri({
    scheme: 'com.yourapp.auth'
  });

  const [request, response, promptAsync] = AuthSession.useAuthRequest(
    {
      clientId: process.env.AUTH0_CLIENT_ID,
      scopes: ['openid', 'profile', 'email', 'offline_access'],
      redirectUri,
      responseType: AuthSession.ResponseType.Code,
      codeChallenge: request?.codeChallenge,
    },
    {
      authorizationEndpoint: `https://${process.env.AUTH0_DOMAIN}/authorize`,
      tokenEndpoint: `https://${process.env.AUTH0_DOMAIN}/oauth/token`,
    }
  );

  return { request, response, promptAsync };
};
```

### API Gateway Integration

```javascript
// middleware/auth.js
const jwt = require('jsonwebtoken');
const jwksRsa = require('jwks-rsa');

const client = jwksRsa({
  jwksUri: `https://${process.env.AUTH0_DOMAIN}/.well-known/jwks.json`
});

const verifyToken = (token) => {
  return new Promise((resolve, reject) => {
    jwt.verify(token, getKey, {
      audience: process.env.AUTH0_AUDIENCE,
      issuer: `https://${process.env.AUTH0_DOMAIN}/`,
      algorithms: ['RS256']
    }, (err, decoded) => {
      if (err) reject(err);
      else resolve(decoded);
    });
  });
};
```

## 🚨 Troubleshooting

### Common Issues

1. **"Invalid token" errors**
   - Verify Auth0 domain and audience match
   - Check token hasn't expired
   - Ensure RS256 algorithm is used

2. **Auth0 login failures**
   - Check callback URLs in Auth0 dashboard
   - Verify client ID and secret
   - Review Auth0 logs for details

3. **Social login not working**
   - Ensure social connections are enabled in Auth0
   - Verify redirect URIs for each provider
   - Check provider-specific settings

4. **Rate limit errors**
   - Review Auth0 rate limit settings
   - Implement exponential backoff
   - Consider upgrading Auth0 plan

### Debug Mode

Enable debug logging:
```bash
terraform apply -var="debug_mode=true" -var="log_level=debug"
```

Check Auth0 logs:
- Go to Auth0 Dashboard → Monitoring → Logs
- Filter by type, user, or connection
- Export logs for analysis

## 📈 Scaling Considerations

- **High Availability**: Deploy across multiple regions
- **Caching**: Use Kong API Gateway caching
- **CDN**: Put Universal Login behind CDN
- **Database**: Use Auth0's custom database for large user bases
- **Enterprise**: Consider Auth0 Private Cloud deployment

## 🔐 Auth0 Configuration Checklist

1. **Applications**
   - [ ] Regular Web Application created
   - [ ] Allowed Callback URLs configured
   - [ ] Allowed Logout URLs configured
   - [ ] Allowed Web Origins set
   - [ ] CORS settings configured

2. **APIs**
   - [ ] API created with unique identifier
   - [ ] Scopes defined
   - [ ] RBAC enabled
   - [ ] Permissions created

3. **Connections**
   - [ ] Database connection configured
   - [ ] Social connections enabled
   - [ ] Connection settings optimized

4. **Security**
   - [ ] Brute force protection enabled
   - [ ] Suspicious IP throttling on
   - [ ] Breached password detection active
   - [ ] MFA configured (production)

5. **Customization**
   - [ ] Universal Login customized
   - [ ] Email templates configured
   - [ ] Custom domains set up (production)
   - [ ] Tenant settings optimized

**Key Changes from Redis to Kong:**
- ✅ **Kong Proxy Cache**: Replaces Redis for response caching
- ✅ **JWT Validation**: Kong handles Auth0 JWT validation at gateway level
- ✅ **Rate Limiting**: Built-in Kong plugin for API protection
- ✅ **Session Storage**: Managed via Kong's memory cache and JWT tokens
- ✅ **Performance**: Improved latency with gateway-level caching
