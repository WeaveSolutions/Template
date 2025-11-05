# Supabase Integration

This document provides instructions on setting up and using the Supabase integration with our database framework, specifically optimized for Auth0 + CRA (Central Rank Authority) architecture.

## Overview

The Supabase integration provides a PostgreSQL-based database solution with real-time capabilities, auth integration, and edge functions. This implementation is specifically designed for Auth0 authentication workflows and centralizes user data management through the CRA pattern.

## Configuration

### Environment Variables

Add the following environment variables to your `.env` file:

```bash
# Supabase Configuration
ENABLE_SUPABASE=true
SUPABASE_DATABASE_URL=postgresql://postgres:password@host:port/database
SUPABASE_DIRECT_URL=postgresql://postgres:password@host:port/database
SUPABASE_URL=https://your-project.supabase.co
SUPABASE_ANON_KEY=your-anon-key
SUPABASE_SERVICE_ROLE_KEY=your-service-role-key
SUPABASE_JWT_SECRET=your-jwt-secret
```

### Required Variables

| Variable | Description | Required |
|----------|-------------|----------|
| `ENABLE_SUPABASE` | Set to `true` to enable Supabase provider | Yes |
| `SUPABASE_DATABASE_URL` | Connection pooling URL for Supabase database | Yes |
| `SUPABASE_DIRECT_URL` | Direct connection URL (bypasses pooling) | Yes |
| `SUPABASE_URL` | Project URL from Supabase dashboard | Yes |
| `SUPABASE_ANON_KEY` | Anonymous/public key for client-side access | Yes |
| `SUPABASE_SERVICE_ROLE_KEY` | Service role key for server-side operations | Yes |
| `SUPABASE_JWT_SECRET` | JWT secret for token verification | Yes |

## Schema Generation

Supabase uses the schema defined in `prisma/providers/supabase.prisma`. To generate Prisma client for this schema:

```bash
PRISMA_SCHEMA=prisma/providers/supabase.prisma npx prisma generate
```

## Auth0 Integration Architecture

### User Model Structure

The Supabase schema implements the CRA pattern with Auth0-specific models:

- **User Model**: Central rank authority user with Auth0 metadata
- **Account Model**: OAuth provider connections (Google, GitHub, Discord, etc.)
- **Multi-provider Support**: Links multiple OAuth accounts to single user

### Authentication Flow

```typescript
// 1. Auth0 authentication
const auth0User = await auth0.getUser();

// 2. CRA user creation/retrieval
const craUser = await db.prisma.user.upsert({
  where: { email: auth0User.email },
  update: { rank: calculateRank(auth0User) },
  create: {
    email: auth0User.email,
    displayName: auth0User.name,
    rank: 1
  }
});

// 3. Link OAuth account
await db.prisma.account.create({
  data: {
    userId: craUser.id,
    provider: 'auth0',
    providerAccountId: auth0User.sub,
    accessToken: tokenData.access_token,
    refreshToken: tokenData.refresh_token,
    expiresAt: new Date(tokenData.expires_at * 1000)
  }
});
```

## Usage

### Basic Usage

```typescript
import { DatabaseClient } from '@your-package/shared-db';

// Initialize with Supabase provider
const db = await DatabaseClient.getInstance('supabase');

// Get user with linked accounts
const user = await db.prisma.user.findUnique({
  where: { email: 'user@example.com' },
  include: {
    accounts: {
      select: {
        provider: true,
        providerAccountId: true,
        scope: true,
        createdAt: true
      }
    }
  }
});

// Raw query execution
const result = await db.executeRaw(`
  SELECT u.*, COUNT(a.id) as account_count 
  FROM users u 
  LEFT JOIN accounts a ON u.id = a.user_id 
  GROUP BY u.id
`);

// Disconnect when done
await db.disconnect();
```

### Advanced Features

#### Real-time Subscriptions

```typescript
import { createClient } from '@supabase/supabase-js';

const supabase = createClient(
  process.env.SUPABASE_URL!,
  process.env.SUPABASE_ANON_KEY!
);

// Subscribe to user changes
const subscription = supabase
  .channel('user-changes')
  .on('postgres_changes', {
    event: '*',
    schema: 'public',
    table: 'users'
  }, (payload) => {
    console.log('User changed:', payload);
  })
  .subscribe();
```

#### Row Level Security (RLS)

```sql
-- Enable RLS on users table
ALTER TABLE users ENABLE ROW LEVEL SECURITY;

-- Policy: Users can only access their own data
CREATE POLICY "Users can view own data" ON users
FOR SELECT USING (auth.uid()::text = id);

-- Policy: Service role can access all data
CREATE POLICY "Service role full access" ON users
FOR ALL USING (current_setting('role') = 'service_role');
```

#### Edge Functions Integration

```typescript
// Call Supabase Edge Function
const { data, error } = await supabase.functions.invoke('sync-auth0-user', {
  body: {
    auth0UserId: user.sub,
    userData: {
      email: user.email,
      name: user.name,
      picture: user.picture
    }
  }
});
```

## Testing

A test script is available to validate your Supabase connection:

```bash
npx ts-node packages/shared-db/scripts/test-supabase-provider.ts
```

### Manual Testing

```typescript
// Test connection and schema
import { DatabaseClient } from '@your-package/shared-db';

async function testSupabase() {
  const db = await DatabaseClient.getInstance('supabase');
  
  // Test user creation
  const testUser = await db.prisma.user.create({
    data: {
      email: 'test@example.com',
      displayName: 'Test User',
      rank: 1
    }
  });
  
  // Test account linking
  const testAccount = await db.prisma.account.create({
    data: {
      userId: testUser.id,
      provider: 'google',
      providerAccountId: 'google-123456789',
      accessToken: 'access-token-example'
    }
  });
  
  console.log('✅ Supabase integration working correctly');
  
  // Cleanup
  await db.prisma.account.delete({ where: { id: testAccount.id } });
  await db.prisma.user.delete({ where: { id: testUser.id } });
  await db.disconnect();
}

testSupabase().catch(console.error);
```

## Current Problems & Solutions

### Schema Issues

**Problem**: Multi-schema support with PostgreSQL connector
- **Solution**: Using `@@schema("public")` directive for proper schema mapping
- **Current Status**: ✅ Resolved

**Problem**: UUID type compatibility
- **Solution**: Using `@supabasedb.Uuid` type mapping for proper UUID handling
- **Current Status**: ✅ Resolved

### Authentication Integration

**Problem**: Auth0 to Supabase user mapping
- **Solution**: Implemented Account model for OAuth provider linking
- **Current Status**: ✅ Implemented

**Problem**: Token refresh handling
- **Solution**: Storing refresh tokens with expiration tracking
- **Current Status**: ✅ Implemented

### Performance Optimization

**Problem**: Inefficient queries for user lookups
- **Solution**: Added comprehensive indexing strategy
- **Current Status**: ✅ Optimized

```sql
-- Implemented indexes
CREATE INDEX user_email_idx ON users(email);
CREATE INDEX user_rank_idx ON users(rank);
CREATE INDEX user_created_at_idx ON users(created_at);
CREATE INDEX account_user_id_idx ON accounts(user_id);
CREATE INDEX account_provider_idx ON accounts(provider);
```

## Limitations

1. **PostgreSQL Only**: Supabase only supports PostgreSQL databases
2. **Real-time Limitations**: Complex queries may not work with real-time subscriptions
3. **RLS Complexity**: Row Level Security requires careful policy design
4. **Edge Function Cold Starts**: Initial function invocation may have latency

## Troubleshooting

### Common Issues

1. **Connection Failed**: 
   - Verify `SUPABASE_DATABASE_URL` format
   - Check project settings in Supabase dashboard
   - Ensure database is not paused

2. **Authentication Failed**: 
   - Verify API keys in Supabase dashboard
   - Check JWT secret configuration
   - Ensure service role key has correct permissions

3. **Schema Generation Failed**:
   - Verify Prisma schema syntax
   - Check environment variable availability
   - Ensure multiSchema preview feature is enabled

4. **RLS Policy Errors**:
   - Check policy syntax and conditions
   - Verify user authentication context
   - Test with service role key

### Logs

Enable debug logging by setting `DATABASE_LOG_LEVEL=debug` in your environment.

```typescript
// Enable Prisma query logging
const db = await DatabaseClient.getInstance('supabase', {
  log: ['query', 'info', 'warn', 'error']
});
```

## Migration Guide

### From Firebase

1. Export Firebase data using Admin SDK
2. Transform user structure to match Supabase schema
3. Import data using Supabase SQL editor or API
4. Update application connection strings

### From PostgreSQL

1. Dump existing PostgreSQL schema
2. Modify schema to match Supabase requirements
3. Import using `pg_restore` or SQL scripts
4. Configure RLS policies and user management

## Security Best Practices

1. **Always use RLS**: Enable Row Level Security on all tables
2. **Minimize Key Exposure**: Use anon key for client-side, service role for server-side
3. **Token Rotation**: Implement refresh token rotation for OAuth accounts
4. **Audit Logging**: Enable and monitor database access logs
5. **Network Security**: Use SSL connections and firewall rules

## Cost Optimization

1. **Connection Pooling**: Always use `SUPABASE_DATABASE_URL` for pooled connections
2. **Index Optimization**: Monitor query performance and optimize indexes
3. **Data Archival**: Implement soft deletes and data retention policies
4. **Real-time Usage**: Monitor real-time connection usage to avoid overage

## Production Checklist

- [ ] RLS policies configured and tested
- [ ] API keys rotated and secured
- [ ] Backup strategy implemented
- [ ] Monitoring and alerting configured
- [ ] Connection pooling enabled
- [ ] Edge functions deployed and tested
- [ ] Auth0 integration verified
- [ ] Performance benchmarking completed
