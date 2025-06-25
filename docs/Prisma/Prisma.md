# Prisma Multi-Provider Documentation

## Overview

This project uses **Prisma ORM** with a **multi-provider architecture** that supports 7 different database providers. Each provider has its own schema file and generates independent Prisma clients, enabling flexible database selection per environment or tenant.

## Supported Database Providers

| Provider | Schema File | Client Output | Use Case |
|----------|-------------|---------------|----------|
| **PostgreSQL** | `providers/postgres.prisma` | `client-postgres` | Primary production database |
| **MongoDB** | `providers/mongodb.prisma` | `client-mongodb` | Document-based storage |
| **CosmosDB** | `providers/cosmosdb.prisma` | `client-cosmosdb` | Azure cloud-native |
| **SQL Server** | `providers/sqlserver.prisma` | `client-sqlserver` | Enterprise databases |
| **IBM Cloud DB2** | `providers/ibmcloud.prisma` | `client-ibmcloud` | Enterprise cloud |

## Architecture Design

### Multi-Provider Strategy
- **Independent Schemas**: Each provider has its own `.prisma` file
- **Isolated Clients**: Generated clients don't interfere with each other
- **Feature Flags**: Enable/disable providers via environment variables
- **Consistent Models**: Same data models across all providers
- **Provider-Specific Types**: Native types optimized for each database

### Schema Structure
```
packages/shared-db/prisma/
├── schema.prisma           # Main schema (placeholder)
├── providers/
│   ├── postgres.prisma     # PostgreSQL schema
│   ├── mongodb.prisma      # MongoDB schema
│   ├── cosmosdb.prisma     # CosmosDB schema
│   ├── sqlserver.prisma    # SQL Server schema
│   └── ibmcloud.prisma     # IBM Cloud DB2 schema
└── migrations/
    └── [provider]/         # Provider-specific migrations
```

## Quick Start

### 1. Environment Setup

Copy and configure environment variables:
```bash
cp .env.example .env
```

Enable your desired providers in `.env`:
```bash
# Database Provider Feature Flags
ENABLE_POSTGRESQL=true
ENABLE_MONGODB=false
ENABLE_COSMOSDB=false
ENABLE_SQLSERVER=false
ENABLE_IBM=false

# Database URLs
POSTGRES_PRISMA_URL=postgresql://user:password@localhost:5432/yourdb
MONGODB_URI=mongodb://localhost:27017/yourdb
COSMOSDB_MONGODB_URI=mongodb://[YOUR-COSMOSDB-NAME]:[PASSWORD]@[HOST]:10255/
SQLSERVER_URL=sqlserver://localhost:1433;database=yourdb;user=sa;password=password
DB2_URL=postgresql://user:password@hostname:port/database  # DB2 via PostgreSQL connector
```

### 2. Generate Prisma Clients

Generate clients for all providers:
```bash
# From project root directory
npm run prisma:generate:all
```

Or generate individual provider clients:
```bash
# PostgreSQL
npx prisma generate --schema=packages/shared-db/prisma/providers/postgres.prisma

# MongoDB
npx prisma generate --schema=packages/shared-db/prisma/providers/mongodb.prisma

# CosmosDB
npx prisma generate --schema=packages/shared-db/prisma/providers/cosmosdb.prisma

# SQL Server
npx prisma generate --schema=packages/shared-db/prisma/providers/sqlserver.prisma

# IBM Cloud DB2
npx prisma generate --schema=packages/shared-db/prisma/providers/ibmcloud.prisma
```

### 3. Database Setup

Run migrations for enabled providers:
```bash
# PostgreSQL migrations
npx prisma migrate dev --schema=packages/shared-db/prisma/providers/postgres.prisma

# For other providers, push schema changes
npx prisma db push --schema=packages/shared-db/prisma/providers/mongodb.prisma
npx prisma db push --schema=packages/shared-db/prisma/providers/cosmosdb.prisma
npx prisma db push --schema=packages/shared-db/prisma/providers/sqlserver.prisma
npx prisma db push --schema=packages/shared-db/prisma/providers/ibmcloud.prisma
```

## Usage in Code

### Importing Provider-Specific Clients

```typescript
// PostgreSQL client
import { PrismaClient as PostgresClient } from '../../../node_modules/.prisma/client-postgres';

// MongoDB client
import { PrismaClient as MongoClient } from '../../../node_modules/.prisma/client-mongodb';

// CosmosDB client
import { PrismaClient as CosmosClient } from '../../../node_modules/.prisma/client-cosmosdb';

// SQL Server client
import { PrismaClient as SqlServerClient } from '../../../node_modules/.prisma/client-sqlserver';

// IBM Cloud client
import { PrismaClient as IbmCloudClient } from '../../../node_modules/.prisma/client-ibmcloud';
```

### Database Client Factory

Use the centralized database client factory:
```typescript
import { DatabaseClient } from '@packages/shared-db';

// Get client for active provider
const db = await DatabaseClient.getInstance();

// Get specific provider client
const postgresDb = await DatabaseClient.getInstance('postgres');
const mongoDb = await DatabaseClient.getInstance('mongodb');
const cosmosDb = await DatabaseClient.getInstance('cosmosdb');
const sqlServerDb = await DatabaseClient.getInstance('sqlserver');
const ibmCloudDb = await DatabaseClient.getInstance('ibmcloud');
```

### Dynamic Provider Selection

```typescript
import { getActiveProvider, DatabaseClient } from '@packages/shared-db';

async function getUserData() {
  const provider = getActiveProvider(); // Returns active provider from env
  const db = await DatabaseClient.getInstance(provider);
  
  const users = await db.user.findMany();
  return users;
}
```

## Mock Data and Testing

For comprehensive mock data management, seeding, and testing documentation, see:

📚 **[Mock Data Documentation](./Mock.md)**

The mock data system provides:
- Centralized data generation using Faker.js
- Feature flag-based provider enablement
- Provider-specific data adapters
- TypeScript scripts for seeding and cleanup
- Individual test files per database provider
- Comprehensive test scenarios and integration examples

## NPM Scripts

Add these scripts to your `package.json`:

```json
{
  "scripts": {
    "prisma:generate:all": "node scripts/generate-all-prisma-clients.js",
    "prisma:generate:postgres": "npx prisma generate --schema=packages/shared-db/prisma/providers/postgres.prisma",
    "prisma:generate:mongodb": "npx prisma generate --schema=packages/shared-db/prisma/providers/mongodb.prisma",
    "prisma:generate:cosmosdb": "npx prisma generate --schema=packages/shared-db/prisma/providers/cosmosdb.prisma",
    "prisma:generate:sqlserver": "npx prisma generate --schema=packages/shared-db/prisma/providers/sqlserver.prisma",
    "prisma:generate:ibmcloud": "npx prisma generate --schema=packages/shared-db/prisma/providers/ibmcloud.prisma",
    
    "seed:all": "node packages/shared-db/scripts/seed-all-providers.js",
    "seed:postgres": "node packages/shared-db/scripts/seed-provider.js postgres",
    "seed:mongodb": "node packages/shared-db/scripts/seed-provider.js mongodb",
    "seed:cosmosdb": "node packages/shared-db/scripts/seed-provider.js cosmosdb",
    "seed:sqlserver": "node packages/shared-db/scripts/seed-provider.js sqlserver",
    "seed:ibmcloud": "node packages/shared-db/scripts/seed-provider.js ibmcloud",
    
    "seed:clear:all": "node packages/shared-db/scripts/clear-all-providers.js",
    "seed:clear:postgres": "node packages/shared-db/scripts/clear-provider.js postgres",
    
    "test:all-providers": "jest --testPathPattern=multi-provider",
    "test:provider:postgres": "DB_PROVIDER=postgres jest",
    "test:provider:mongodb": "DB_PROVIDER=mongodb jest",
    "test:provider:cosmosdb": "DB_PROVIDER=cosmosdb jest",
    "test:provider:sqlserver": "DB_PROVIDER=sqlserver jest",
    "test:provider:ibmcloud": "DB_PROVIDER=ibmcloud jest"
  }
}
```

## Troubleshooting

### Common Issues

#### 1. IDE Validation Errors
**Problem**: Prisma extensions show cross-schema validation errors

**Solution**: These are **expected false positives**. Prisma validates all schemas together but each works independently:
```bash
# Each schema generates correctly:
npx prisma generate --schema=packages/shared-db/prisma/providers/postgres.prisma ✅
npx prisma generate --schema=packages/shared-db/prisma/providers/mongodb.prisma ✅
```

#### 2. Environment Variable Errors
**Problem**: `Environment variable not found` errors

**Solution**: Ensure `.env` file has all required variables:
```bash
# Check required variables
cat .env.example | grep -E "(POSTGRES_|MONGODB_|COSMOSDB_|SQLSERVER_|DB2_)"
```

#### 3. Client Import Errors
**Problem**: Cannot import generated Prisma clients

**Solution**: Regenerate clients and check output paths:
```bash
# Regenerate all clients
npm run prisma:generate:all

# Check generated clients
ls -la packages/node_modules/.prisma/
```

#### 4. Native Type Errors
**Problem**: Invalid native type prefix errors

**Solution**: Ensure native types match datasource names:
- PostgreSQL: `@pgdb.VarChar(255)`
- MongoDB: `@mongodb.ObjectId`
- CosmosDB: `@cosmosdb.ObjectId`
- SQL Server: `@sqlserverdb.DateTime2`
- IBM Cloud: `@ibmcloud.Uuid`

### Performance Optimization

#### Connection Pooling
```typescript
// Configure connection pooling per provider
const db = new PrismaClient({
  datasources: {
    db: {
      url: process.env.POSTGRES_PRISMA_URL,
    },
  },
  log: ['query', 'info', 'warn', 'error'],
});

// Enable connection pooling
await db.$connect();
```

#### Query Optimization
```typescript
// Use select to reduce data transfer
const users = await db.user.findMany({
  select: {
    id: true,
    email: true,
    displayName: true,
    // Don't select large fields unless needed
  },
  where: {
    rank: { gte: 50 }
  },
  take: 10 // Limit results
});
```

## Advanced Usage

### Multi-Tenant Architecture

```typescript
// Tenant-specific database selection
async function getTenantDatabase(tenantId: string) {
  const tenantConfig = await getTenantConfig(tenantId);
  return DatabaseClient.getInstance(tenantConfig.provider);
}

// Usage
const tenantDb = await getTenantDatabase('tenant-123');
const userData = await tenantDb.user.findMany();
```

### Provider Failover

```typescript
// Implement provider failover
async function findUserWithFailover(userId: string) {
  const providers = ['postgres', 'mongodb', 'cosmosdb'];
  
  for (const provider of providers) {
    try {
      const db = await DatabaseClient.getInstance(provider);
      const user = await db.user.findUnique({ where: { id: userId } });
      if (user) return user;
    } catch (error) {
      console.warn(`Provider ${provider} failed:`, error);
      continue;
    }
  }
  
  throw new Error('All providers failed');
}
```

### Data Migration Between Providers

```typescript
// Migrate data between providers
async function migrateData(fromProvider: string, toProvider: string) {
  const sourceDb = await DatabaseClient.getInstance(fromProvider);
  const targetDb = await DatabaseClient.getInstance(toProvider);
  
  const users = await sourceDb.user.findMany();
  
  for (const user of users) {
    await targetDb.user.upsert({
      where: { id: user.id },
      update: user,
      create: user,
    });
  }
}
```

## Best Practices

1. **Provider Selection**: Use feature flags to enable/disable providers
2. **Error Handling**: Implement proper error handling for each provider
3. **Connection Management**: Always disconnect clients in cleanup
4. **Type Safety**: Use TypeScript for type-safe database operations
5. **Testing**: Use mock data for consistent testing across providers
6. **Performance**: Implement connection pooling and query optimization
7. **Monitoring**: Add logging and metrics for database operations

## Conclusion

The multi-provider Prisma setup enables flexible, scalable database architecture that can adapt to different requirements, environments, and cloud providers while maintaining type safety and developer experience.

For additional examples and advanced usage patterns, see the `packages/shared-db/examples/` directory.
