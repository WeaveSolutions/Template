# Supabase Production Guide for Nexpo

This guide provides instructions for setting up and managing a Supabase project in a production environment.

## Overview

Supabase is an open-source Firebase alternative that provides backend services like authentication, database, and storage. Moving from a local or development setup to production requires careful configuration to ensure security, scalability, and reliability.

## Prerequisites

- A Supabase account ([https://app.supabase.com/](https://app.supabase.com/))
- A Nexpo project with Supabase already integrated

## Setting Up Supabase for Production

### 1. Create a Production Project

1. Log in to the [Supabase Dashboard](https://app.supabase.com/).
2. Click on "New Project" and follow the prompts to create a new project for production.
   - Choose a meaningful name (e.g., "MyApp-Production").
   - Set a strong database password.
   - Select a region close to your users for lower latency.
3. Once created, navigate to "Settings" > "API" to find your production Project URL and Anonymous Key.

### 2. Update Environment Variables for Production

Update your environment variables in your deployment environment (e.g., Vercel for Next.js, or EAS for Expo builds) with the production Supabase credentials. Do NOT commit these to version control.

For local testing with production credentials (if needed), update `.env` with separate production variables:

```
# Production
EXPO_PUBLIC_SUPABASE_URL_PROD=your_production_supabase_project_url
EXPO_PUBLIC_SUPABASE_ANON_KEY_PROD=your_production_supabase_anon_key
NEXT_PUBLIC_SUPABASE_URL_PROD=your_production_supabase_project_url
NEXT_PUBLIC_SUPABASE_ANON_KEY_PROD=your_production_supabase_anon_key
```

In your app, conditionally use production or development credentials based on the environment:

```typescript
// In shared-ui/src/utils/supabase.ts
const env = process.env.NODE_ENV === 'production' ? '_PROD' : '';
const SUPABASE_URL = process.env[`NEXT_PUBLIC_SUPABASE_URL${env}`] || process.env[`EXPO_PUBLIC_SUPABASE_URL${env}`] || '';
const SUPABASE_ANON_KEY = process.env[`NEXT_PUBLIC_SUPABASE_ANON_KEY${env}`] || process.env[`EXPO_PUBLIC_SUPABASE_ANON_KEY${env}`] || '';
```

### 3. Database Configuration for Production

#### Database Settings
- **Connection Pooling**: Supabase uses PgBouncer for connection pooling by default. In the Supabase Dashboard under "Database" > "Connection Pooling", adjust the pool size based on your expected traffic (e.g., increase to 50-100 for high-traffic apps).
- **Timeouts**: Set appropriate connection timeouts to handle peak loads. Default settings are usually sufficient for small to medium apps.

#### Backups
- Enable automated backups in "Database" > "Backups". Supabase offers daily backups for paid plans. For critical apps, consider additional manual backups or third-party solutions.
- Download backups periodically from the dashboard for offsite storage if compliance requires it.

#### Performance
- Monitor database performance in "Reports" > "Database" for slow queries or high resource usage.
- Use indexes on frequently queried columns to improve performance. Add indexes via SQL in the dashboard:
  ```sql
  CREATE INDEX idx_users_email ON users(email);
  ```

### 4. Security Configurations

#### Row-Level Security (RLS)
- Enable RLS on tables to control access based on user authentication. In the Supabase Dashboard, go to "Authentication" > "Policies" or use SQL:
  ```sql
  ALTER TABLE profiles ENABLE ROW LEVEL SECURITY;
  CREATE POLICY "Users can only view their own profile" ON profiles
    FOR SELECT USING (auth.uid() = id);
  ```

#### API Keys
- Use the Anonymous Key for client-side operations, but for server-side operations (e.g., Next.js API routes), create a Service Role Key with elevated permissions (found in "Settings" > "API"). Store it securely in environment variables.

#### SSL and Network
- Supabase enforces SSL connections by default. Ensure your app connects via HTTPS.
- Restrict database access to specific IP ranges if needed under "Database" > "Network".

### 5. Authentication in Production

#### Email Templates
- Customize email templates for sign-up, password reset, etc., in "Authentication" > "Email Templates" to match your app's branding.

#### Rate Limiting
- Supabase applies rate limits to prevent abuse. Monitor "Reports" > "Auth" for rate limit issues and adjust in "Authentication" > "Rate Limits" if on a paid plan.

#### OAuth Providers
- Set up OAuth providers (Google, GitHub, etc.) in "Authentication" > "Providers" for social logins. Ensure you register your app with the provider and add the client ID and secret to Supabase.

### 6. Storage for Production

- Use Supabase Storage for user-uploaded files. Set appropriate file size limits and allowed file types in "Storage" > "Settings".
- Secure buckets with policies, e.g., only authenticated users can upload:
  ```sql
  CREATE POLICY "Only authenticated users can upload" ON storage.objects
    FOR INSERT TO authenticated WITH CHECK (true);
  ```

### 7. Edge Functions for Production

- Deploy serverless functions with Supabase Edge Functions for custom backend logic. Ensure functions are optimized for cold starts and have proper error handling.
- Deploy functions from your local setup to production:
  ```bash
  supabase functions deploy my-function --project-ref your-production-project-id
  ```

### 8. Monitoring and Scaling

- Use Supabase's built-in "Reports" for monitoring API usage, database health, and authentication events.
- For high-traffic apps, consider upgrading your Supabase plan for more resources ("Settings" > "Billing").
- Set up alerts for unusual activity or resource limits in "Settings" > "Alerts".

### 9. Migrating Data to Production

If you've been using a local or staging Supabase instance:
1. Export data from your development environment using `pg_dump` or Supabase's export feature.
2. Import data into production using `psql` or the dashboard's SQL editor.
   - Example with `pg_dump`:
     ```bash
     pg_dump -h localhost -p 54322 -U postgres -d postgres > dev_dump.sql
     psql -h your-production-host -p 5432 -U postgres -d postgres < dev_dump.sql
     ```
3. Alternatively, use Supabase's migration tools or manually apply schema changes via SQL.

### 10. Domain Configuration (Optional)

- Add a custom domain for your Supabase project in "Settings" > "General" to use a branded URL instead of the default `supabase.co` domain.

## Best Practices

- **Environment Separation**: Maintain separate Supabase projects for development, staging, and production to prevent accidental data overwrites.
- **Secrets Management**: Store sensitive keys (like Service Role Key) securely in environment variables or a secrets manager, not in code.
- **Regular Backups**: Schedule and verify regular backups to avoid data loss.
- **Testing**: Test authentication flows, database queries, and edge functions in a staging environment before deploying to production.
- **Documentation**: Keep production configurations and access details documented securely for your team.

## Troubleshooting

- **Connection Issues**: Verify your Supabase URL and keys are correct in production environment variables. Check "Reports" for connection limit errors.
- **Performance Slowdowns**: Look for slow queries in "Reports" > "Database" and optimize with indexes or caching.
- **Authentication Failures**: Ensure RLS policies aren't overly restrictive. Check logs in "Authentication" > "Logs" for failed logins.
- **Deployment Errors**: If edge functions fail to deploy, check for syntax errors or dependency issues in the function code.

For more detailed information, refer to the official Supabase documentation at [https://supabase.com/docs/guides/production](https://supabase.com/docs/guides/production).
