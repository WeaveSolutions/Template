# IBM Cloud DB2 Integration

This document provides instructions on setting up and using the IBM Cloud DB2 integration with our database framework.

## Overview

The IBM Cloud DB2 integration allows seamless connection to IBM Cloud Db2 databases using Prisma with PostgreSQL connector, since Prisma does not have native support for DB2. This implementation handles the connection management, query execution, and other database operations specific to IBM Cloud DB2.

## Configuration

### Environment Variables

Add the following environment variables to your `.env` file:

```
# IBM Cloud DB2
ENABLE_IBM=true
DB2_URL=postgresql://user:password@hostname:port/dbname
IBM_APIKEY=your-api-key
IBM_RESOURCE_INSTANCE_ID=your-resource-instance-id
```

### Required Variables

| Variable | Description | Required |
|----------|-------------|----------|
| `ENABLE_IBM` | Set to `true` to enable IBM Cloud DB2 provider | Yes |
| `DB2_URL` | PostgreSQL-compatible connection string for DB2 | Yes |
| `IBM_APIKEY` | API Key for IBM Cloud services | Optional |
| `IBM_RESOURCE_INSTANCE_ID` | Resource instance ID for DB2 database | Optional |

## Schema Generation

IBM Cloud DB2 uses the schema defined in `prisma/providers/ibmcloud.prisma`. To generate Prisma client for this schema:

```bash
PRISMA_SCHEMA=prisma/providers/ibmcloud.prisma npx prisma generate
```

## Usage

### Basic Usage

```typescript
import { DatabaseClient } from '@your-package/shared-db';

// Initialize with IBM Cloud provider
const db = await DatabaseClient.getInstance('ibmcloud');

// Execute a query
const users = await db.prisma.ibmcloudUser.findMany();

// Raw query execution
const result = await db.executeRaw('SELECT * FROM "IbmcloudUser"');

// Disconnect when done
await db.disconnect();
```

### Advanced Features

#### Connection Pooling

The IBM Cloud provider supports connection pooling by default when using the `DB2_URL` variable.

#### Transactions

```typescript
const result = await db.prisma.$transaction(async (tx) => {
  const user = await tx.ibmcloudUser.create({
    data: { name: "Test User", email: "test@example.com" }
  });
  
  const post = await tx.ibmcloudPost.create({
    data: { 
      title: "Test Post", 
      content: "Content", 
      authorId: user.id 
    }
  });
  
  return { user, post };
});
```

## Testing

A test script is available to validate your IBM Cloud DB2 connection:

```bash
npx ts-node packages/shared-db/scripts/test-ibmcloud-provider.ts
```

## Limitations

1. Prisma does not have native DB2 support, so we're using PostgreSQL connector
2. Some DB2-specific features might not be available through Prisma
3. IBM Cloud-specific authentication methods require additional configuration

## Troubleshooting

### Common Issues

1. **Connection Failed**: Verify your `DB2_URL` format and check network access
2. **Authentication Failed**: Ensure your IBM Cloud API key is valid
3. **Schema Issues**: Make sure your DB2 database schema matches the Prisma schema

### Logs

Enable debug logging by setting `DATABASE_LOG_LEVEL=debug` in your environment.
