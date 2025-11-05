# Prisma VS Code Extension Setup Guide

Complete setup guide for the Prisma VS Code Extension with Nexpo template integration, including Prisma Accelerate support and multi-schema configuration.

## Table of Contents
1. [Extension Installation](#extension-installation)
2. [Prisma Account Setup](#prisma-account-setup)
3. [Database Configuration](#database-configuration)
4. [Prisma Accelerate Setup](#prisma-accelerate-setup)
5. [Multi-Schema Configuration](#multi-schema-configuration)
6. [Environment Variables](#environment-variables)
7. [Schema Generation](#schema-generation)
8. [Troubleshooting](#troubleshooting)

---

## Extension Installation

### 1. Install Prisma Extension
```bash
# Install via VS Code Extensions marketplace
# Search for: "Prisma" by Prisma
# Extension ID: Prisma.prisma
```

### 2. Extension Features
- **Syntax Highlighting**: Full Prisma schema syntax support
- **IntelliSense**: Auto-completion for models, fields, and attributes
- **Error Detection**: Real-time validation and error highlighting
- **Formatting**: Auto-format Prisma schemas
- **Database Introspection**: Generate schemas from existing databases
- **Schema Validation**: Multi-schema and relation validation

---

## Prisma Account Setup

### 1. Create Prisma Account
1. Visit [Prisma Data Platform](https://cloud.prisma.io/)
2. Sign up with GitHub/Google or email
3. Verify your email address

### 2. Login via VS Code Extension
1. Open VS Code
2. Press `Ctrl+Shift+P` (Windows) or `Cmd+Shift+P` (Mac)
3. Type "Prisma: Login"
4. Follow authentication flow in browser
5. Confirm login in VS Code

### 3. Account Benefits
- **Prisma Accelerate**: Connection pooling and global caching
- **Prisma Pulse**: Real-time database events
- **Database Browser**: Visual database exploration
- **Performance Insights**: Query analytics and optimization

---

## Database Configuration

### 1. Supported Providers in Nexpo
- **PostgreSQL** (Primary with Accelerate support)
- **MongoDB** (Standalone schema)
- **Azure CosmosDB** (MongoDB API)
- **SQL Server** (Enterprise support)
- **Supabase** (PostgreSQL-based)
- **Auth0** (User metadata storage)

### 2. Provider Feature Flags
```bash
# Enable/disable providers in .env
ENABLE_POSTGRES=true
ENABLE_MONGODB=true
ENABLE_COSMOSDB=false
ENABLE_SQLSERVER=false
ENABLE_SUPABASE=false
```

---

## Prisma Accelerate Setup

### 1. Create Accelerate Database
1. In VS Code, open Prisma extension panel
2. Click "Create Database" or use Command Palette
3. Select "PostgreSQL with Accelerate"
4. Choose region (closest to your users)
5. Configure database settings
6. Copy the generated connection string

### 2. Example Connection String
```
prisma+postgres://accelerate.prisma-data.net/?api_key=eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...
```

### 3. Accelerate Benefits
- **⚡ Connection Pooling**: Reduces connection overhead
- **🌍 Global Caching**: Sub-100ms query responses
- **📊 Analytics**: Built-in performance monitoring
- **🔒 Security**: Encrypted connections with API key auth
- **🚀 Auto-scaling**: Handles traffic spikes automatically

---

## Multi-Schema Configuration

### 1. Schema File Structure
```
packages/shared-db/prisma/providers/
├── auth0.prisma          # Auth0 user metadata
├── postgres.prisma       # PostgreSQL with Accelerate
├── mongodb.prisma        # MongoDB standalone
├── cosmosdb.prisma       # Azure CosmosDB
├── sqlserver.prisma      # SQL Server
└── supabase.prisma       # Supabase (PostgreSQL)
```

### 2. Independent Schema Usage
Each schema is designed to work independently:

```bash
# Generate specific provider client
PRISMA_SCHEMA=prisma/providers/postgres.prisma npx prisma generate
PRISMA_SCHEMA=prisma/providers/mongodb.prisma npx prisma generate
PRISMA_SCHEMA=prisma/providers/cosmosdb.prisma npx prisma generate
```

### 3. Schema Validation
The Prisma extension may show validation errors when multiple schemas are open. This is expected behavior due to Prisma's multi-schema limitations.

**Solution**: Work with one schema at a time or use independent generation commands.

---

## Environment Variables

### 1. Create Environment File
Copy `.env.example` to `.env.local`:
```bash
cp .env.example .env.local
```

### 2. PostgreSQL Configuration
```bash
# Local Development
POSTGRES_PRISMA_URL=postgresql://user:password@localhost:5432/yourdb?schema=public

# Prisma Accelerate (Production)
POSTGRES_ACCELERATE_URL=prisma+postgres://accelerate.prisma-data.net/?api_key=your-api-key

# Feature Flags
ENABLE_POSTGRES_ACCELERATE=true
POSTGRES_ACCELERATE_CACHE_TTL=300
POSTGRES_ACCELERATE_GLOBAL_TTL=86400
```

### 3. Multi-Provider Configuration
```bash
# Database Providers
DEFAULT_DATABASE_PROVIDER=postgres
ENABLE_POSTGRES=true
ENABLE_MONGODB=true
ENABLE_COSMOSDB=false

# Auth0 Integration
AUTH0_ISSUER_BASE_URL=your-tenant.auth0.com
AUTH0_CLIENT_ID=your-auth0-client-id
AUTH0_CLIENT_SECRET=your-auth0-client-secret
```

---

## Schema Generation

### 1. Generate All Clients
```bash
# Navigate to shared-db package
cd packages/shared-db

# Generate all enabled provider clients
pnpm run prisma:generate
```

### 2. Generate Specific Provider
```bash
# PostgreSQL with Accelerate
PRISMA_SCHEMA=prisma/providers/postgres.prisma npx prisma generate

# MongoDB standalone
PRISMA_SCHEMA=prisma/providers/mongodb.prisma npx prisma generate

# CosmosDB (MongoDB API)
PRISMA_SCHEMA=prisma/providers/cosmosdb.prisma npx prisma generate
```

### 3. Database Migration
```bash
# PostgreSQL migrations
PRISMA_SCHEMA=prisma/providers/postgres.prisma npx prisma migrate dev

# MongoDB (no migrations needed)
PRISMA_SCHEMA=prisma/providers/mongodb.prisma npx prisma db push
```

---

## Troubleshooting

### 1. Extension Not Working
**Issue**: Prisma extension not providing IntelliSense
**Solutions**:
- Restart VS Code
- Reload window (`Ctrl+Shift+P` → "Developer: Reload Window")
- Check if schema files have `.prisma` extension
- Verify workspace folder contains prisma schemas

### 2. Multi-Schema Validation Errors
**Issue**: Red underlines in schema files
**Solutions**:
- Work with one schema file at a time
- Use independent generation commands
- Ignore false positive errors from multi-schema validation

### 3. Accelerate Connection Issues
**Issue**: Cannot connect to Prisma Accelerate
**Solutions**:
- Verify API key is correct
- Check environment variable naming
- Ensure network connectivity
- Try regenerating API key

### 4. Authentication Problems
**Issue**: Extension login fails
**Solutions**:
- Clear VS Code extension cache
- Try manual authentication in browser
- Check firewall/proxy settings
- Use incognito/private browser mode

---

## Best Practices

### 1. Development Workflow
1. **Local Development**: Use standard PostgreSQL connection
2. **Testing**: Use Accelerate for staging environment
3. **Production**: Always use Accelerate for performance

### 2. Schema Management
1. Keep schemas independent for multi-provider support
2. Use consistent naming conventions across providers
3. Document provider-specific limitations
4. Test schema changes across all enabled providers

### 3. Security
1. Never commit API keys to version control
2. Use environment variables for all connections
3. Rotate API keys regularly
4. Monitor usage via Prisma dashboard

---

## Additional Resources

- [Prisma Documentation](https://prisma.io/docs)
- [Prisma Accelerate Guide](https://prisma.io/docs/accelerate)
- [VS Code Extension](https://marketplace.visualstudio.com/items?itemName=Prisma.prisma)
- [Nexpo Database Architecture](./AUTH0_INTEGRATION.md)

---

*This documentation is part of the Nexpo template's comprehensive database integration system.*