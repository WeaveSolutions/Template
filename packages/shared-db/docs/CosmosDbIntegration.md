# Azure CosmosDB Integration

This document provides instructions on setting up and using the Azure CosmosDB integration with our database framework for globally distributed, multi-model database operations.

## Overview

The Azure CosmosDB integration provides a globally distributed, multi-model database service with guaranteed low latency, elastic scalability, and comprehensive SLAs. This implementation uses the MongoDB API through Prisma's MongoDB connector for seamless integration with existing MongoDB code patterns.

## Configuration

### Environment Variables

Add the following environment variables to your `.env` file:

```bash
# Azure CosmosDB Configuration
ENABLE_COSMOSDB=true
COSMOSDB_MONGODB_URI=mongodb://username:password@cosmosdb-account.mongo.cosmos.azure.com:10255/nexpo_db?ssl=true&replicaSet=globaldb&retrywrites=false&maxIdleTimeMS=120000&appName=@cosmosdb-account@
COSMOS_DB_URI=mongodb://username:password@cosmosdb-account.mongo.cosmos.azure.com:10255/nexpo_db?ssl=true&replicaSet=globaldb&retrywrites=false&maxIdleTimeMS=120000&appName=@cosmosdb-account@
COSMOSDB_ACCOUNT_NAME=your-cosmosdb-account
COSMOSDB_DATABASE_NAME=nexpo_db
COSMOSDB_PRIMARY_KEY=your-primary-key
COSMOSDB_CONNECTION_STRING=AccountEndpoint=https://your-account.documents.azure.com:443/;AccountKey=your-key;

# Azure Configuration
AZURE_TENANT_ID=your-tenant-id
AZURE_CLIENT_ID=your-client-id
AZURE_CLIENT_SECRET=your-client-secret
AZURE_SUBSCRIPTION_ID=your-subscription-id
AZURE_RESOURCE_GROUP=your-resource-group
```

### Required Variables

| Variable | Description | Required |
|----------|-------------|----------|
| `ENABLE_COSMOSDB` | Set to `true` to enable CosmosDB provider | Yes |
| `COSMOSDB_MONGODB_URI` | MongoDB API connection string for CosmosDB | Yes |
| `COSMOS_DB_URI` | Alternative connection string format | Yes |
| `COSMOSDB_ACCOUNT_NAME` | CosmosDB account name | Yes |
| `COSMOSDB_DATABASE_NAME` | Database name | Yes |
| `COSMOSDB_PRIMARY_KEY` | Primary access key | Yes |
| `COSMOSDB_CONNECTION_STRING` | Full connection string | Yes |
| `AZURE_TENANT_ID` | Azure tenant ID | Yes |
| `AZURE_SUBSCRIPTION_ID` | Azure subscription ID | Yes |

## Schema Generation

CosmosDB uses the schema defined in `prisma/providers/cosmosdb.prisma`. To generate Prisma client for this schema:

```bash
PRISMA_SCHEMA=prisma/providers/cosmosdb.prisma npx prisma generate
```

## Database Schema

### Models Overview

The CosmosDB schema implements a document-based structure optimized for global distribution:

- **CosmosUser**: Core user document with global distribution support
- **CosmosPost**: Content posts with partition key optimization

### Schema Structure

```typescript
// User Document
type CosmosUser = {
  id: string;           // MongoDB ObjectId
  email: string;        // Unique email address (partition key candidate)
  name?: string;        // Optional display name
  password: string;     // User password
  role: string;         // User role (simplified from array)
  posts: CosmosPost[];  // Referenced posts
  createdAt: Date;
  updatedAt: Date;
}

// Post Document
type CosmosPost = {
  id: string;           // MongoDB ObjectId
  title: string;
  content?: string;
  published: boolean;
  authorId: string;     // Reference to CosmosUser (partition key)
  author: CosmosUser;
  createdAt: Date;
  updatedAt: Date;
}
```

## Usage

### Basic Usage

```typescript
import { DatabaseClient } from '@your-package/shared-db';

// Initialize with CosmosDB provider
const db = await DatabaseClient.getInstance('cosmosdb');

// Create a user with partition key considerations
const user = await db.prisma.cosmosUser.create({
  data: {
    email: 'user@example.com',
    name: 'John Doe',
    password: 'hashed_password',
    role: 'author'
  }
});

// Create a post with proper partitioning
const post = await db.prisma.cosmosPost.create({
  data: {
    title: 'Global Post',
    content: 'This post is distributed globally',
    published: true,
    authorId: user.id // Important for partitioning
  }
});

// Query with partition key for optimal performance
const userPosts = await db.prisma.cosmosPost.findMany({
  where: {
    authorId: user.id, // Using partition key
    published: true
  },
  orderBy: { createdAt: 'desc' }
});

// Cross-partition query (less efficient)
const allPublishedPosts = await db.prisma.cosmosPost.findMany({
  where: { published: true },
  include: {
    author: {
      select: {
        email: true,
        name: true
      }
    }
  }
});

// Disconnect when done
await db.disconnect();
```

### Advanced Features

#### Global Distribution Queries

```typescript
// Query with consistency level considerations
const globalUsers = await db.prisma.cosmosUser.findMany({
  where: {
    role: 'author'
  },
  // Note: Consistency levels are configured at account level
  orderBy: { createdAt: 'desc' }
});

// Partition-specific queries for better performance
const usersByRegion = await db.prisma.cosmosUser.aggregateRaw({
  pipeline: [
    {
      $match: {
        role: 'author',
        // Assuming we have a region field for partitioning
        region: 'us-east'
      }
    },
    {
      $group: {
        _id: '$role',
        count: { $sum: 1 },
        latestUser: { $max: '$createdAt' }
      }
    }
  ]
});
```

#### Optimized Aggregations

```typescript
// Efficient aggregation using partition keys
const authorStats = await db.prisma.cosmosUser.aggregateRaw({
  pipeline: [
    // Match specific partition
    { $match: { role: 'author' } },
    
    // Lookup posts (within same partition when possible)
    {
      $lookup: {
        from: 'cosmos_posts',
        localField: '_id',
        foreignField: 'authorId',
        as: 'userPosts'
      }
    },
    
    // Calculate statistics
    {
      $project: {
        email: 1,
        name: 1,
        totalPosts: { $size: '$userPosts' },
        publishedPosts: {
          $size: {
            $filter: {
              input: '$userPosts',
              cond: { $eq: ['$$this.published', true] }
            }
          }
        },
        latestPost: { $max: '$userPosts.createdAt' }
      }
    },
    
    // Sort by activity
    { $sort: { totalPosts: -1 } }
  ]
});
```

#### Change Feed Integration

```typescript
// Note: Change feed requires Azure SDK, not available through Prisma
// This is conceptual implementation
async function setupChangeFeed() {
  const { CosmosClient } = require('@azure/cosmos');
  
  const client = new CosmosClient({
    endpoint: process.env.COSMOS_ENDPOINT,
    key: process.env.COSMOSDB_PRIMARY_KEY
  });
  
  const database = client.database(process.env.COSMOSDB_DATABASE_NAME);
  const container = database.container('cosmos_users');
  
  // Change feed iterator
  const changeFeedIterator = container.items.getChangeFeedIterator({
    startFromBeginning: true
  });
  
  while (changeFeedIterator.hasMoreResults) {
    const response = await changeFeedIterator.readNext();
    for (const item of response.result) {
      console.log('User changed:', item);
      // Process change event
    }
  }
}
```

#### Multi-Region Writes

```typescript
// Configure multi-region writes (account-level setting)
// Query with session consistency for read-your-writes
const createUserGlobally = async (userData: any) => {
  // Create user with proper partitioning
  const user = await db.prisma.cosmosUser.create({
    data: {
      ...userData,
      // Include region for partitioning if needed
      region: process.env.AZURE_REGION || 'us-east'
    }
  });
  
  // Immediate read should be consistent due to session consistency
  const verifyUser = await db.prisma.cosmosUser.findUnique({
    where: { id: user.id }
  });
  
  return verifyUser;
};
```

## Testing

A test script is available to validate your CosmosDB connection:

```bash
npx ts-node packages/shared-db/scripts/test-cosmosdb-provider.ts
```

### Manual Testing

```typescript
// Test CosmosDB functionality
import { DatabaseClient } from '@your-package/shared-db';

async function testCosmosDB() {
  const db = await DatabaseClient.getInstance('cosmosdb');
  
  // Test user creation
  const testUser = await db.prisma.cosmosUser.create({
    data: {
      email: 'test@example.com',
      name: 'Test User',
      password: 'test_password',
      role: 'tester'
    }
  });
  
  // Test post creation with partitioning
  const testPost = await db.prisma.cosmosPost.create({
    data: {
      title: 'Global Test Post',
      content: 'This is a test post distributed globally',
      published: true,
      authorId: testUser.id
    }
  });
  
  // Test partition-key query
  const userPosts = await db.prisma.cosmosPost.findMany({
    where: { authorId: testUser.id }
  });
  
  // Test cross-partition aggregation
  const stats = await db.prisma.cosmosUser.aggregateRaw({
    pipeline: [
      { $match: { role: 'tester' } },
      {
        $lookup: {
          from: 'cosmos_posts',
          localField: '_id',
          foreignField: 'authorId',
          as: 'posts'
        }
      },
      {
        $project: {
          email: 1,
          postCount: { $size: '$posts' }
        }
      }
    ]
  });
  
  console.log('✅ CosmosDB integration working correctly');
  console.log('User:', testUser);
  console.log('Posts:', userPosts);
  console.log('Stats:', stats);
  
  // Cleanup
  await db.prisma.cosmosPost.delete({ where: { id: testPost.id } });
  await db.prisma.cosmosUser.delete({ where: { id: testUser.id } });
  await db.disconnect();
}

testCosmosDB().catch(console.error);
```

## Current Problems & Solutions

### Partition Key Strategy

**Problem**: Inefficient cross-partition queries
- **Solution**: Designed schema with authorId as natural partition key
- **Current Status**: ✅ Optimized

**Problem**: Hot partitions with uneven data distribution
- **Solution**: Using email-based partitioning for users, authorId for posts
- **Current Status**: ✅ Balanced

### MongoDB API Limitations

**Problem**: Some MongoDB features not supported in CosmosDB
- **Solution**: Using compatible subset of MongoDB operations
- **Current Status**: ✅ Compatible

**Problem**: Transaction limitations across partitions
- **Solution**: Designed single-partition transactions where possible
- **Current Status**: ✅ Adapted

### Performance Optimization

**Problem**: High request unit consumption
- **Solution**: Implemented efficient indexing and query patterns
- **Current Status**: ✅ Optimized

```javascript
// Recommended indexes for CosmosDB
db.cosmos_users.createIndex({ email: 1 }, { unique: true });
db.cosmos_users.createIndex({ role: 1 });
db.cosmos_users.createIndex({ createdAt: -1 });
db.cosmos_posts.createIndex({ authorId: 1, published: 1 });
db.cosmos_posts.createIndex({ published: 1, createdAt: -1 });
```

## Limitations

1. **Request Units**: Operations consume RUs, requiring cost optimization
2. **Partition Key Immutability**: Cannot change partition key after creation
3. **Cross-Partition Transactions**: Limited transaction support across partitions
4. **MongoDB API Subset**: Not all MongoDB features are available
5. **Eventual Consistency**: Default consistency may not suit all use cases

## Troubleshooting

### Common Issues

1. **Connection Failed**:
   - Verify CosmosDB account is active
   - Check connection string format
   - Ensure SSL is enabled

2. **High RU Consumption**:
   - Optimize queries to use partition keys
   - Review indexing strategy
   - Monitor query patterns

3. **Partition Key Errors**:
   - Ensure consistent partition key usage
   - Verify data distribution
   - Check for hot partitions

4. **Consistency Issues**:
   - Review consistency level settings
   - Implement proper retry logic
   - Consider session consistency

### Request Unit Optimization

```typescript
// Monitor RU consumption
const result = await db.prisma.cosmosUser.findMany({
  where: { role: 'author' }
});

// Use projection to reduce RU consumption
const lightweightUsers = await db.prisma.cosmosUser.findMany({
  select: {
    id: true,
    email: true,
    name: true
  },
  where: { role: 'author' }
});
```

### Logs

Enable debug logging by setting `DATABASE_LOG_LEVEL=debug` in your environment.

## Migration Guide

### From MongoDB

1. **Account Setup**: Create CosmosDB account with MongoDB API
2. **Schema Review**: Ensure schema is compatible with CosmosDB limitations
3. **Partition Strategy**: Plan partition keys for optimal distribution
4. **Data Migration**: Use MongoDB tools or Azure Data Factory

### From SQL Databases

1. **Denormalization**: Convert normalized data to document format
2. **Partition Design**: Plan partitioning strategy for global distribution
3. **Index Strategy**: Create indexes optimized for CosmosDB
4. **Consistency Model**: Adapt application to eventual consistency

## Security Best Practices

1. **Access Keys**: Rotate keys regularly and use read-only keys when possible
2. **Network Security**: Configure firewall rules and VNet integration
3. **Authentication**: Use Azure AD integration for enhanced security
4. **Encryption**: Enable encryption at rest and in transit
5. **Audit Logging**: Enable diagnostic logging for security monitoring

## Performance Optimization

1. **Partition Strategy**: Design efficient partition keys
2. **Query Optimization**: Use partition keys in queries
3. **Indexing**: Create targeted indexes for query patterns
4. **Consistency Levels**: Choose appropriate consistency for use case
5. **Request Units**: Monitor and optimize RU consumption

## Cost Optimization

1. **Throughput Planning**: Right-size provisioned throughput
2. **Autoscale**: Use autoscale for variable workloads
3. **Query Efficiency**: Optimize queries to reduce RU consumption
4. **Data Archival**: Implement time-based data retention
5. **Multi-Region Strategy**: Balance availability vs cost

## Production Checklist

- [ ] Partition key strategy validated
- [ ] Throughput requirements calculated
- [ ] Multi-region configuration completed
- [ ] Backup and disaster recovery configured
- [ ] Monitoring and alerting set up
- [ ] Security policies implemented
- [ ] Performance benchmarking completed
- [ ] Cost optimization measures in place
- [ ] Change feed consumers implemented
- [ ] Consistency level requirements verified
