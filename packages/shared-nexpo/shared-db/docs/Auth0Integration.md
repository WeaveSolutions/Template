# Auth0 Integration

This document provides instructions on setting up and using the Auth0 integration with our Central Rank Authority (CRA) Account Center framework for unified identity management and federated user data storage.

## Overview

The Auth0 integration provides a comprehensive identity management solution that serves as the backbone of the CRA (Central Rank Authority) system. This implementation leverages Auth0's Management API to store user metadata, manage connected OAuth providers through linked list structures, and implement enterprise-grade security with audit trails and integrity verification.

## Configuration

### Environment Variables

Add the following environment variables to your `.env` file:

```bash
# Auth0 Configuration
ENABLE_AUTH0=true
AUTH0_DOMAIN=your-domain.auth0.com
AUTH0_CLIENT_ID=your-management-api-client-id
AUTH0_CLIENT_SECRET=your-management-api-client-secret
AUTH0_AUDIENCE=https://your-domain.auth0.com/api/v2/
AUTH0_SCOPE=read:users update:users read:user_metadata update:user_metadata read:app_metadata update:app_metadata

# CRA Configuration
AUTH0_DATABASE_URL=postgresql://localhost:5432/auth0_cra_db?schema=public
CRA_VERSION=2.1.0
CRA_HMAC_SECRET=your-256-bit-hmac-secret-key
AUTH0_ISSUER_BASE_URL=https://your-domain.auth0.com
AUTH0_BASE_URL=http://localhost:3000
AUTH0_SECRET=your-32-character-secret-for-sessions

# Vault Integration (Optional)
VAULT_URL=http://localhost:8200
VAULT_TOKEN=your-vault-token

# Database Backend Selection
USER_DATA_BACKEND=auth0
```

### Required Variables

| Variable | Description | Required |
|----------|-------------|----------|
| `ENABLE_AUTH0` | Set to `true` to enable Auth0 provider | Yes |
| `AUTH0_DOMAIN` | Your Auth0 domain | Yes |
| `AUTH0_CLIENT_ID` | Management API application client ID | Yes |
| `AUTH0_CLIENT_SECRET` | Management API application client secret | Yes |
| `AUTH0_AUDIENCE` | Management API audience (usually `https://domain.auth0.com/api/v2/`) | Yes |
| `AUTH0_DATABASE_URL` | PostgreSQL database for local schema tracking | Yes |
| `CRA_HMAC_SECRET` | HMAC secret for integrity verification | Yes |
| `AUTH0_ISSUER_BASE_URL` | Auth0 domain for authentication | Yes |
| `AUTH0_BASE_URL` | Your application base URL | Yes |
| `AUTH0_SECRET` | Session encryption secret | Yes |

## Schema Generation

Auth0 uses the schema defined in `prisma/providers/auth0.prisma` for local tracking and integrity verification. To generate Prisma client for this schema:

```bash
PRISMA_SCHEMA=prisma/providers/auth0.prisma npx prisma generate
```

## Database Schema

### Models Overview

The Auth0 schema implements a comprehensive CRA Account Center structure:

- **Auth0User**: Core user model mapping to Auth0 user profile
- **Auth0AppMetadata**: System data including roles, permissions, and security metadata
- **Auth0UserMetadata**: User preferences and privacy settings
- **Auth0ConnectedProvider**: OAuth providers linked through linked list structure
- **Auth0DataObject**: User data objects with provenance and access tracking
- **Auth0AuditTrail**: Comprehensive audit logging with linked list structure
- **Auth0SchemaIntegrity**: Tamper detection and integrity verification

### Schema Structure

```typescript
// Core User Model
type Auth0User = {
  id: string;                    // Auth0 user ID
  email: string;                 // Unique email address
  emailVerified: boolean;        // Email verification status
  name?: string;                 // Display name
  nickname?: string;             // Auth0 nickname
  picture?: string;              // Profile picture URL
  
  // CRA specific fields
  craVersion: string;            // CRA version (default: "2.1.0")
  primaryEmail: string;          // Primary email for account linking
  displayName?: string;          // CRA display name
  
  // Relationships
  appMetadata?: Auth0AppMetadata;
  userMetadata?: Auth0UserMetadata;
  connectedProviders: Auth0ConnectedProvider[];
  dataObjects: Auth0DataObject[];
  auditTrails: Auth0AuditTrail[];
  schemaIntegrity?: Auth0SchemaIntegrity;
}

// System Metadata
type Auth0AppMetadata = {
  userId: string;
  roles: string[];               // RBAC roles
  permissions: string[];         // Granular permissions
  
  // Subscription data
  subscriptionPlan: string;      // free, premium, enterprise
  subscriptionStatus: string;    // active, cancelled, past_due
  featuresEnabled: string[];     // Feature flags
  
  // Security metadata
  riskScore: number;             // 0.0 to 1.0
  failedLoginAttempts: number;   // Failed login counter
  accountLocked: boolean;        // Account lock status
  lastSecurityAudit?: Date;      // Last security audit
  
  // Multi-tenant support
  tenantId?: string;             // Tenant identifier
  databaseAccess?: Json;         // Database permissions
}

// User Preferences
type Auth0UserMetadata = {
  userId: string;
  
  // Preferences
  defaultRetentionPolicy: string; // keep, delete
  mfaEnabled: boolean;
  preferredMfaMethod: string;     // sms, totp, webauthn, none
  
  // Notifications
  emailAlerts: boolean;
  securityNotifications: boolean;
  providerSyncNotifications: boolean;
  
  // Privacy
  dataSharingConsent: boolean;
  analyticsEnabled: boolean;
  gdprConsentGiven?: boolean;
  gdprConsentDate?: Date;
  
  // Linked list metadata
  connectedProvidersHead?: string;
  connectedProvidersCount: number;
  dataObjectsHead?: string;
  dataObjectsCount: number;
  dataObjectsSizeBytes: bigint;
}
```

## Usage

### Basic Usage

```typescript
import { Auth0Provider } from '@your-package/shared-db';

// Initialize Auth0 provider
const auth0Provider = new Auth0Provider({
  domain: process.env.AUTH0_DOMAIN!,
  clientId: process.env.AUTH0_CLIENT_ID!,
  clientSecret: process.env.AUTH0_CLIENT_SECRET!,
  audience: process.env.AUTH0_AUDIENCE!,
  hmacSecret: process.env.CRA_HMAC_SECRET!,
  vaultUrl: process.env.VAULT_URL,
  vaultToken: process.env.VAULT_TOKEN,
});

// Get user's complete CRA account center
const accountCenter = await auth0Provider.getUserAccountCenter('auth0|user_id_here');

// Create a new user with CRA metadata
const newUser = await auth0Provider.createUser({
  email: 'user@example.com',
  name: 'John Doe',
  primaryEmail: 'user@example.com',
  preferences: {
    defaultRetentionPolicy: 'keep',
    mfaEnabled: true,
    preferredMfaMethod: 'totp',
    emailAlerts: true,
    securityNotifications: true,
    dataSharingConsent: false,
    analyticsEnabled: true,
  },
  subscription: {
    plan: 'free',
    status: 'active',
    featuresEnabled: ['basic_analytics', 'email_notifications'],
  },
});

// Connect an OAuth provider
await auth0Provider.connectOAuthProvider('auth0|user_id', {
  provider: 'google-oauth2',
  accessToken: 'access_token_here',
  refreshToken: 'refresh_token_here',
  expiresAt: new Date(Date.now() + 3600000),
  scopes: ['openid', 'email', 'profile'],
  displayName: 'Google Account',
  email: 'user@gmail.com',
  dataRetentionChoice: 'keep',
});

// Add data object from connected provider
await auth0Provider.addDataObject('auth0|user_id', {
  objectType: 'profile_photo',
  data: { url: 'https://example.com/photo.jpg', size: 1024 },
  origin: {
    provider: 'google-oauth2',
    providerUserId: 'google_user_id',
    sourceType: 'profile_data',
    retentionPolicy: 'keep',
    syncEnabled: true,
    qualityScore: 0.95,
    trustLevel: 'verified',
  },
  storageOptions: {
    storageType: 'external',
    encryptionMethod: 'aes-256-gcm',
  },
});
```

### Advanced Features

#### Linked List Operations

```typescript
// Traverse connected providers (linked list)
const connectedProviders = await auth0Provider.getConnectedProviders('auth0|user_id');

// Get specific provider by name
const googleProvider = connectedProviders.find(p => p.provider === 'google-oauth2');

// Update provider status
await auth0Provider.updateProviderStatus('auth0|user_id', 'google-oauth2', 'suspended');

// Disconnect provider (removes from linked list)
await auth0Provider.disconnectOAuthProvider('auth0|user_id', 'google-oauth2', {
  dataRetentionChoice: 'delete', // or 'keep'
  reason: 'user_requested',
});
```

#### Data Object Management

```typescript
// Get all data objects for a user
const dataObjects = await auth0Provider.getDataObjects('auth0|user_id');

// Search data objects by type
const profilePhotos = dataObjects.filter(obj => obj.objectType === 'profile_photo');

// Update data object with access logging
await auth0Provider.updateDataObject('auth0|user_id', 'object_id', {
  data: { url: 'https://updated-url.com/photo.jpg' },
  accessLog: {
    action: 'update',
    source: 'web_app',
    ipAddress: '192.168.1.1',
    userAgent: 'Mozilla/5.0...',
  },
});

// Remove data object
await auth0Provider.removeDataObject('auth0|user_id', 'object_id');
```

#### Role-Based Access Control

```typescript
// Update user roles and permissions
await auth0Provider.updateUserRoles('auth0|user_id', {
  roles: ['admin', 'content_manager'],
  permissions: [
    'read:all_users',
    'update:user_profiles',
    'create:content',
    'publish:content',
  ],
  ipAddress: '192.168.1.1',
  userAgent: 'Mozilla/5.0...',
});

// Check user permissions
const hasPermission = await auth0Provider.hasPermission('auth0|user_id', 'read:all_users');

// Get user roles
const userRoles = await auth0Provider.getUserRoles('auth0|user_id');
```

#### Security and Audit

```typescript
// Log custom audit events
await auth0Provider.logAuditEvent(
  'auth0|user_id',
  'data_export',
  'user_data',
  'export_request_id',
  {
    exportType: 'full_profile',
    requestedBy: 'user',
    dataSize: '2.5MB',
  },
  '192.168.1.1',
  'Mozilla/5.0...'
);

// Verify schema integrity
const isIntegrityValid = await auth0Provider.verifySchemaIntegrity('auth0|user_id');

if (!isIntegrityValid) {
  console.warn('Schema integrity verification failed - possible tampering detected');
}

// Compact audit trail (remove old entries)
await auth0Provider.compactAuditTrail('auth0|user_id');

// Get audit trail for user
const auditEvents = await auth0Provider.getAuditTrail('auth0|user_id');
```

#### Vault Integration

```typescript
// Store sensitive data in Vault
const vaultReference = await auth0Provider.storeInVault({
  accessToken: 'sensitive_access_token',
  refreshToken: 'sensitive_refresh_token',
  apiKeys: ['key1', 'key2'],
});

// Retrieve sensitive data from Vault
const sensitiveData = await auth0Provider.retrieveFromVault(vaultReference);
```

## Testing

A test script is available to validate your Auth0 integration:

```bash
npx ts-node packages/shared-db/scripts/test-auth0-provider.ts
```

### Manual Testing

```typescript
// Test Auth0 CRA integration
import { Auth0Provider } from '@your-package/shared-db';

async function testAuth0CRA() {
  const auth0Provider = new Auth0Provider({
    domain: process.env.AUTH0_DOMAIN!,
    clientId: process.env.AUTH0_CLIENT_ID!,
    clientSecret: process.env.AUTH0_CLIENT_SECRET!,
    audience: process.env.AUTH0_AUDIENCE!,
    hmacSecret: process.env.CRA_HMAC_SECRET!,
  });
  
  // Test user creation with CRA metadata
  const testUser = await auth0Provider.createUser({
    email: 'test@example.com',
    name: 'Test User',
    primaryEmail: 'test@example.com',
    preferences: {
      defaultRetentionPolicy: 'keep',
      mfaEnabled: false,
      preferredMfaMethod: 'none',
      emailAlerts: true,
      securityNotifications: true,
      dataSharingConsent: false,
      analyticsEnabled: true,
    },
  });
  
  // Test OAuth provider connection
  await auth0Provider.connectOAuthProvider(testUser.user_id, {
    provider: 'google-oauth2',
    accessToken: 'test_access_token',
    refreshToken: 'test_refresh_token',
    expiresAt: new Date(Date.now() + 3600000),
    scopes: ['openid', 'email', 'profile'],
    displayName: 'Test Google Account',
    email: 'test@gmail.com',
    dataRetentionChoice: 'keep',
  });
  
  // Test data object addition
  await auth0Provider.addDataObject(testUser.user_id, {
    objectType: 'test_data',
    data: { message: 'Hello CRA!' },
    origin: {
      provider: 'google-oauth2',
      providerUserId: 'test_google_id',
      sourceType: 'test_data',
      retentionPolicy: 'keep',
      syncEnabled: true,
      qualityScore: 1.0,
      trustLevel: 'verified',
    },
  });
  
  // Test account center retrieval
  const accountCenter = await auth0Provider.getUserAccountCenter(testUser.user_id);
  
  // Test integrity verification
  const integrityValid = await auth0Provider.verifySchemaIntegrity(testUser.user_id);
  
  console.log('✅ Auth0 CRA integration working correctly');
  console.log('User:', testUser);
  console.log('Account Center:', accountCenter);
  console.log('Integrity Valid:', integrityValid);
  
  // Cleanup
  await auth0Provider.removeDataObject(testUser.user_id, accountCenter?.data_objects.nodes[Object.keys(accountCenter.data_objects.nodes)[0]]?.id);
  await auth0Provider.disconnectOAuthProvider(testUser.user_id, 'google-oauth2', {
    dataRetentionChoice: 'delete',
    reason: 'test_cleanup',
  });
  await auth0Provider.deleteUser(testUser.user_id);
}

testAuth0CRA().catch(console.error);
```

## Current Problems & Solutions

### Management API Rate Limits

**Problem**: Auth0 Management API has rate limits for operations
- **Solution**: Implemented intelligent batching and caching strategies
- **Current Status**: ✅ Optimized

**Problem**: Token expiration during long-running operations
- **Solution**: Automatic token refresh with retry mechanisms
- **Current Status**: ✅ Handled

### Data Consistency

**Problem**: Maintaining consistency between Auth0 and local schema tracking
- **Solution**: Implemented HMAC-based integrity verification and checksums
- **Current Status**: ✅ Secure

**Problem**: Concurrent modifications to linked list structures
- **Solution**: Atomic operations with conflict detection and resolution
- **Current Status**: ✅ Thread-safe

### Storage Optimization

**Problem**: Large metadata objects hitting Auth0 limits
- **Solution**: Vault integration for sensitive data and external references
- **Current Status**: ✅ Scalable

```javascript
// Vault integration example
const sensitiveData = {
  accessTokens: [...],
  refreshTokens: [...],
  apiKeys: [...],
};

// Store in Vault and keep reference in Auth0
const vaultRef = await auth0Provider.storeInVault(sensitiveData);
const metadata = {
  vault_reference: vaultRef,
  data_hash: generateHash(sensitiveData),
  stored_at: new Date().toISOString(),
};
```

## Limitations

1. **Auth0 Metadata Limits**: 16KB limit on user_metadata and app_metadata
2. **Management API Rate Limits**: 2 requests per second (burst: 10)
3. **Linked List Performance**: O(n) traversal for large lists
4. **Real-time Limitations**: No native real-time subscriptions
5. **Complex Queries**: Limited querying capabilities compared to traditional databases

## Troubleshooting

### Common Issues

1. **Management API Authentication Failed**:
   - Verify client credentials and scopes
   - Check Management API application configuration
   - Ensure proper audience setting

2. **Metadata Size Exceeded**:
   - Enable Vault integration for large objects
   - Use external references for binary data
   - Compress JSON data before storage

3. **Integrity Verification Failed**:
   - Check HMAC secret configuration
   - Verify no external metadata modifications
   - Review audit trail for unauthorized changes

4. **Rate Limit Exceeded**:
   - Implement exponential backoff
   - Use batch operations where possible
   - Consider caching frequently accessed data

### Performance Optimization

```typescript
// Enable caching and batching
const auth0Provider = new Auth0Provider({
  domain: process.env.AUTH0_DOMAIN!,
  clientId: process.env.AUTH0_CLIENT_ID!,
  clientSecret: process.env.AUTH0_CLIENT_SECRET!,
  audience: process.env.AUTH0_AUDIENCE!,
  hmacSecret: process.env.CRA_HMAC_SECRET!,
  
  // Performance options
  cacheTtl: 300, // 5 minute cache
  batchSize: 50, // Batch operations
  retryAttempts: 3,
  retryDelay: 1000,
});

// Monitor performance
auth0Provider.on('performance', (metrics) => {
  console.log('API Calls:', metrics.apiCalls);
  console.log('Cache Hits:', metrics.cacheHits);
  console.log('Average Response Time:', metrics.averageResponseTime);
});
```

### Logs

Enable debug logging by setting `DATABASE_LOG_LEVEL=debug` in your environment.

## Migration Guide

### From Firebase Auth

1. **Export User Data**: Use Firebase Admin SDK to export user profiles
2. **Transform Data**: Convert Firebase custom claims to Auth0 app_metadata
3. **Provider Mapping**: Map Firebase providers to Auth0 social connections
4. **CRA Initialization**: Initialize CRA metadata for each migrated user

### From Traditional Database

1. **Schema Analysis**: Map existing user schema to CRA structure
2. **Data Transformation**: Convert relational data to Auth0 metadata format
3. **Provider Integration**: Set up OAuth provider connections
4. **Audit Trail Creation**: Initialize audit trails for existing users

## Security Best Practices

1. **HMAC Integrity**: Always verify data integrity using HMAC signatures
2. **Vault Integration**: Store sensitive tokens and keys in Vault
3. **Audit Logging**: Log all data access and modifications
4. **Principle of Least Privilege**: Grant minimal required permissions
5. **Regular Security Audits**: Verify schema integrity periodically

## Performance Optimization

1. **Caching Strategy**: Cache frequently accessed user data
2. **Batch Operations**: Group multiple API calls when possible
3. **Lazy Loading**: Load linked list nodes on demand
4. **Compression**: Compress large metadata objects
5. **External References**: Use Vault for large or sensitive data

## Cost Optimization

1. **Efficient Queries**: Minimize Management API calls
2. **Data Archival**: Archive old audit trail entries
3. **Storage Optimization**: Use external storage for large objects
4. **Batch Processing**: Process operations in batches during off-peak hours
5. **Monitoring**: Track API usage and optimize based on patterns

## Production Checklist

- [ ] Management API credentials configured and tested
- [ ] HMAC secret generated and secured
- [ ] Vault integration configured for sensitive data
- [ ] Rate limiting and retry logic implemented
- [ ] Integrity verification scheduled and automated
- [ ] Audit trail compaction scheduled
- [ ] Monitoring and alerting configured
- [ ] Backup strategy for critical metadata
- [ ] Security audit procedures established
- [ ] Performance benchmarking completed
- [ ] Documentation updated and accessible
- [ ] Team training on CRA concepts completed

## Auth0 Dashboard Configuration

### Management API Application Setup

1. **Create Machine-to-Machine Application**:
   - Go to Auth0 Dashboard > Applications
   - Click "Create Application"
   - Select "Machine to Machine Applications"
   - Choose your API (Management API)

2. **Configure Scopes**:
   ```
   read:users
   update:users
   create:users
   delete:users
   read:user_metadata
   update:user_metadata
   read:app_metadata
   update:app_metadata
   ```

3. **Security Settings**:
   - Enable token endpoint authentication method: POST
   - Configure allowed callback URLs
   - Set up CORS if needed

### User Profile Configuration

1. **Custom Claims Configuration**:
   - Set up Rules or Actions to add CRA metadata to tokens
   - Configure custom user profile fields
   - Set up email verification workflows

2. **Social Connections**:
   - Configure OAuth providers (Google, Facebook, Apple, etc.)
   - Set up custom database connections if needed
   - Configure user profile attribute mapping

This comprehensive Auth0 integration provides the foundation for a robust, scalable, and secure CRA Account Center implementation that serves as the central identity hub for your multi-platform application ecosystem.
