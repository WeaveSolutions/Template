# Nexpo TypeScript API with Express

A production-ready TypeScript API backend implementing the Nexpo API specification using Express.js framework with CRA (Central Rank Authority) authentication integration.

## Features

- 🔐 **Auth0 JWT Authentication**: Secure API endpoints with JWT token validation
- 🎖️ **CRA Integration**: Central Rank Authority aligned authentication patterns
- 📊 **MindsDB Integration**: Query proxy for unified data access
- 📚 **Swagger Documentation**: Auto-generated API documentation at `/swagger`
- 🔄 **CORS Support**: Configurable cross-origin resource sharing
- 🛡️ **Security First**: Helmet CSP, rate limiting, CORS whitelisting
- 📝 **Structured Logging**: Winston logger with Kong-compatible formats
- 🐳 **Docker Ready**: Multi-stage Dockerfile for optimized containers
- ✅ **Health Checks**: Built-in health monitoring endpoints
- 🔗 **Account Linking**: Secure identity federation with audit trails

## Architecture Overview

This TypeScript API backend implements:

- **Auth0 Integration**: Federated identity management with JWT-based sessions
- **Kong API Gateway**: Compatible logging and request routing
- **Audit Compliance**: 10-year retention policy for security events
- **OAuth Providers**: Google, Facebook, Apple, GitHub, Microsoft
- **CRA Authentication**: Patterns documented in the [CRA Authentication Documentation](../../../docs/Authentication/CRA.md)

## Quick Start

```bash
# Install dependencies
pnpm install

# Copy environment variables
cp .env.example .env

# Configure Auth0 credentials in .env
# Run development server
pnpm dev
```

## API Endpoints

### Authentication
- `GET /auth/login` - Initiate login flow
- `GET /auth/logout` - Logout with session cleanup
- `GET /auth/callback` - OAuth callback handler
- `GET /api/auth/status` - Check authentication status
- `GET /api/auth/token` - Get access token
- `GET /api/auth/providers` - List enabled OAuth providers
- `GET /api/auth/social/:provider` - Initiate social login

### User Management
- `GET /api/user/profile` - Get user profile
- `PATCH /api/user/metadata` - Update user metadata
- `GET /api/user/roles` - Get user roles
- `GET /api/user/permissions` - Get user permissions
- `DELETE /api/user/account` - GDPR-compliant account deletion

### Account Linking
- `GET /api/linking/accounts` - Get linked accounts
- `POST /api/linking/link` - Link new identity
- `DELETE /api/linking/unlink/:provider/:user_id` - Unlink identity
- `GET /api/linking/link-url/:provider` - Get linking URL

### Monitoring
- `GET /health` - Service health check
- `GET /ready` - Kong readiness probe
- `GET /api/version` - API version and features
- `GET /api/metrics/current` - Current service metrics
- `GET /api/metrics/prometheus` - Prometheus format metrics

## Environment Variables

See [.env.example](.env.example) for all configuration options. Key variables:

- `AUTH0_*` - Auth0 tenant configuration
- `KONG_*` - Kong API Gateway settings
- `ENABLE_*` - Feature flags for OAuth providers
- `AUDIT_RETENTION_DAYS` - Compliance retention (default: 3653)

## Security Features

### Middleware Stack
1. **CSP Nonce Generation**: Dynamic Content Security Policy
2. **Helmet Security Headers**: HSTS, X-Frame-Options, etc.
3. **Request ID Assignment**: Distributed tracing support
4. **Kong-Compatible Logging**: JSON structured logs
5. **Rate Limiting**: Global and per-endpoint limits
6. **CORS Whitelisting**: Strict origin validation

### JWT Validation
- RS256 algorithm with cached public keys
- Audience and issuer verification
- Token refresh with audit logging

### Audit Logging
- 10-year retention for compliance
- User actions, security events, access logs
- Kong-compatible JSON format

## Development

```bash
# Run with hot reload
pnpm dev

# Build TypeScript
pnpm build

# Run production build
pnpm start

# Type checking
pnpm lint
```

## Kong Integration

The service logs in Kong-compatible JSON format:

```json
{
  "timestamp": "2024-01-01T00:00:00.000Z",
  "service": {
    "name": "auth-service",
    "version": "1.0.0"
  },
  "request": {
    "id": "auth-1234567890-abc123",
    "method": "GET",
    "uri": "/api/user/profile"
  },
  "response": {
    "status": 200,
    "size": "1234"
  },
  "latencies": {
    "request": 45
  }
}
```

## Deployment

1. Configure environment variables
2. Set up Auth0 tenant and applications
3. Configure Kong routes and plugins
4. Deploy with health checks enabled
5. Monitor metrics and audit logs

## Architecture Decisions

- **Single Entry Point**: `cra-server.ts` consolidates all middleware
- **Modular Routes**: Separated by domain (auth, user, linking, metrics)
- **TypeScript Strict Mode**: Type safety throughout
- **Express v5**: Latest features and performance
- **No Database Dependency**: Stateless with Auth0 as source of truth

## License

Apache 2.0 - See [LICENSE](../../../../LICENSE)
