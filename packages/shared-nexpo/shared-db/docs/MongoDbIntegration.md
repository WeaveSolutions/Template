# MongoDB Integration

This document provides instructions on setting up and using the MongoDB integration with our database framework for document-based data storage and NoSQL operations.

## Overview

The MongoDB integration provides a flexible, document-oriented database solution with high performance, horizontal scaling, and rich query capabilities. This implementation uses Prisma's MongoDB connector with independent schema design for optimal NoSQL patterns.

## Configuration

### Environment Variables

Add the following environment variables to your `.env` file:

```bash
# MongoDB Configuration
ENABLE_MONGODB=true
MONGODB_URI=mongodb://username:password@localhost:27017/nexpo_db?authSource=admin
MONGODB_DATABASE=nexpo_db
MONGODB_USERNAME=nexpo_user
MONGODB_PASSWORD=secure_password
MONGODB_HOST=localhost
MONGODB_PORT=27017
MONGODB_AUTH_SOURCE=admin

# MongoDB Atlas (Cloud)
MONGODB_ATLAS_URI=mongodb+srv://username:password@cluster.mongodb.net/nexpo_db?retryWrites=true&w=majority
MONGODB_ATLAS_CLUSTER=cluster0
```

### Required Variables

| Variable | Description | Required |
|----------|-------------|----------|
| `ENABLE_MONGODB` | Set to `true` to enable MongoDB provider | Yes |
| `MONGODB_URI` | Complete MongoDB connection string | Yes |
| `MONGODB_DATABASE` | Database name | Yes |
| `MONGODB_USERNAME` | Database username | Yes |
| `MONGODB_PASSWORD` | Database password | Yes |
| `MONGODB_HOST` | Database host | Yes |
| `MONGODB_PORT` | Database port (default: 27017) | Yes |
| `MONGODB_AUTH_SOURCE` | Authentication database | Optional |

## Schema Generation

MongoDB uses the schema defined in `prisma/providers/mongodb.prisma`. To generate Prisma client for this schema:

```bash
PRISMA_SCHEMA=prisma/providers/mongodb.prisma npx prisma generate
```

## Database Schema

### Models Overview

The MongoDB schema implements a document-based structure optimized for NoSQL patterns:

- **MongodbUser**: Core user document with embedded fields
- **MongodbPost**: Content posts with author references

### Schema Structure

```typescript
// User Document
type MongodbUser = {
  id: string;           // MongoDB ObjectId
  email: string;        // Unique email address
  name?: string;        // Optional display name
  password: string;     // User password
  role: string;         // User role (single value)
  posts: MongodbPost[]; // Referenced posts
  createdAt: Date;
  updatedAt: Date;
}

// Post Document
type MongodbPost = {
  id: string;           // MongoDB ObjectId
  title: string;
  content?: string;
  published: boolean;
  authorId: string;     // Reference to MongodbUser
  author: MongodbUser;
  createdAt: Date;
  updatedAt: Date;
}
```

## Usage

### Basic Usage

```typescript
import { DatabaseClient } from '@your-package/shared-db';

// Initialize with MongoDB provider
const db = await DatabaseClient.getInstance('mongodb');

// Create a user with embedded data
const user = await db.prisma.mongodbUser.create({
  data: {
    email: 'user@example.com',
    name: 'John Doe',
    password: 'hashed_password',
    role: 'author'
  }
});

// Create a post referencing the user
const post = await db.prisma.mongodbPost.create({
  data: {
    title: 'My First Post',
    content: 'This is the content of my first post',
    published: true,
    authorId: user.id
  }
});

// Query with relations
const userWithPosts = await db.prisma.mongodbUser.findUnique({
  where: { id: user.id },
  include: {
    posts: {
      where: { published: true },
      orderBy: { createdAt: 'desc' }
    }
  }
});

// Find users by role
const authors = await db.prisma.mongodbUser.findMany({
  where: { role: 'author' },
  include: {
    posts: {
      where: { published: true }
    }
  }
});

// Raw aggregation queries
const userStats = await db.prisma.mongodbUser.aggregateRaw({
  pipeline: [
    {
      $lookup: {
        from: 'mongodb_posts',
        localField: '_id',
        foreignField: 'authorId',
        as: 'posts'
      }
    },
    {
      $addFields: {
        postCount: { $size: '$posts' },
        publishedPostCount: {
          $size: {
            $filter: {
              input: '$posts',
              cond: { $eq: ['$$this.published', true] }
            }
          }
        }
      }
    },
    {
      $project: {
        email: 1,
        name: 1,
        role: 1,
        postCount: 1,
        publishedPostCount: 1
      }
    }
  ]
});

// Disconnect when done
await db.disconnect();
```

### Advanced Features

#### Aggregation Pipelines

```typescript
// Complex aggregation for analytics
const authorAnalytics = await db.prisma.mongodbUser.aggregateRaw({
  pipeline: [
    // Match only authors
    { $match: { role: 'author' } },
    
    // Lookup posts
    {
      $lookup: {
        from: 'mongodb_posts',
        localField: '_id',
        foreignField: 'authorId',
        as: 'posts'
      }
    },
    
    // Calculate statistics
    {
      $addFields: {
        totalPosts: { $size: '$posts' },
        publishedPosts: {
          $size: {
            $filter: {
              input: '$posts',
              cond: { $eq: ['$$this.published', true] }
            }
          }
        },
        latestPost: {
          $max: '$posts.createdAt'
        }
      }
    },
    
    // Group by role for summary
    {
      $group: {
        _id: '$role',
        totalAuthors: { $sum: 1 },
        averagePosts: { $avg: '$totalPosts' },
        totalPublishedPosts: { $sum: '$publishedPosts' },
        mostRecentPost: { $max: '$latestPost' }
      }
    }
  ]
});
```

#### Text Search

```typescript
// Create text index (run once)
await db.prisma.mongodbPost.createIndex({
  title: 'text',
  content: 'text'
});

// Search posts
const searchResults = await db.prisma.mongodbPost.findRaw({
  filter: {
    $text: { $search: 'programming tutorial' }
  },
  options: {
    projection: {
      title: 1,
      content: 1,
      score: { $meta: 'textScore' }
    },
    sort: { score: { $meta: 'textScore' } }
  }
});
```

#### Geospatial Queries

```typescript
// Add location field to user schema
const userWithLocation = await db.prisma.mongodbUser.update({
  where: { id: user.id },
  data: {
    // Note: This requires schema modification
    location: {
      type: 'Point',
      coordinates: [-73.856077, 40.848447] // [longitude, latitude]
    }
  }
});

// Find users near a location
const nearbyUsers = await db.prisma.mongodbUser.findRaw({
  filter: {
    location: {
      $near: {
        $geometry: {
          type: 'Point',
          coordinates: [-73.856077, 40.848447]
        },
        $maxDistance: 1000 // meters
      }
    }
  }
});
```

#### Bulk Operations

```typescript
// Bulk insert users
const bulkUsers = await db.prisma.mongodbUser.createMany({
  data: [
    {
      email: 'user1@example.com',
      name: 'User One',
      password: 'password1',
      role: 'reader'
    },
    {
      email: 'user2@example.com',
      name: 'User Two',
      password: 'password2',
      role: 'author'
    }
  ]
});

// Bulk update
await db.prisma.mongodbPost.updateMany({
  where: { published: false },
  data: { published: true }
});
```

## Testing

A test script is available to validate your MongoDB connection:

```bash
npx ts-node packages/shared-db/scripts/test-mongodb-provider.ts
```

### Manual Testing

```typescript
// Test MongoDB functionality
import { DatabaseClient } from '@your-package/shared-db';

async function testMongoDB() {
  const db = await DatabaseClient.getInstance('mongodb');
  
  // Test user creation
  const testUser = await db.prisma.mongodbUser.create({
    data: {
      email: 'test@example.com',
      name: 'Test User',
      password: 'test_password',
      role: 'tester'
    }
  });
  
  // Test post creation
  const testPost = await db.prisma.mongodbPost.create({
    data: {
      title: 'Test Post',
      content: 'This is a test post for MongoDB',
      published: true,
      authorId: testUser.id
    }
  });
  
  // Test aggregation
  const userStats = await db.prisma.mongodbUser.aggregateRaw({
    pipeline: [
      { $match: { role: 'tester' } },
      {
        $lookup: {
          from: 'mongodb_posts',
          localField: '_id',
          foreignField: 'authorId',
          as: 'posts'
        }
      },
      {
        $addFields: {
          postCount: { $size: '$posts' }
        }
      }
    ]
  });
  
  console.log('✅ MongoDB integration working correctly');
  console.log('User:', testUser);
  console.log('Post:', testPost);
  console.log('Stats:', userStats);
  
  // Cleanup
  await db.prisma.mongodbPost.delete({ where: { id: testPost.id } });
  await db.prisma.mongodbUser.delete({ where: { id: testUser.id } });
  await db.disconnect();
}

testMongoDB().catch(console.error);
```

## Current Problems & Solutions

### Schema Design Issues

**Problem**: Prisma limitations with MongoDB schema flexibility
- **Solution**: Using independent schema design with minimal constraints
- **Current Status**: ✅ Resolved

**Problem**: Complex nested document support
- **Solution**: Using aggregation pipelines for complex queries
- **Current Status**: ✅ Implemented

### Performance Optimization

**Problem**: Inefficient queries without proper indexing
- **Solution**: Implemented strategic indexing for common queries
- **Current Status**: ✅ Optimized

```javascript
// Recommended indexes
db.mongodb_users.createIndex({ email: 1 }, { unique: true });
db.mongodb_users.createIndex({ role: 1 });
db.mongodb_users.createIndex({ createdAt: -1 });
db.mongodb_posts.createIndex({ authorId: 1 });
db.mongodb_posts.createIndex({ published: 1 });
db.mongodb_posts.createIndex({ published: 1, createdAt: -1 });
db.mongodb_posts.createIndex({ title: "text", content: "text" });
```

### Connection Management

**Problem**: Connection pool exhaustion under high load
- **Solution**: Implemented connection pooling configuration
- **Current Status**: ✅ Configured

```typescript
// Connection pool configuration
const connectionString = `${process.env.MONGODB_URI}?maxPoolSize=20&minPoolSize=5&maxIdleTimeMS=30000&serverSelectionTimeoutMS=5000`;
```

## Limitations

1. **Schema Flexibility**: Prisma adds some constraints to MongoDB's schema-less nature
2. **Enum Support**: MongoDB/Prisma doesn't support enums in the same way as SQL databases
3. **Transactions**: Limited transaction support compared to ACID databases
4. **Complex Joins**: No SQL-style JOINs; requires aggregation pipelines

## Troubleshooting

### Common Issues

1. **Connection Failed**:
   - Verify MongoDB server is running
   - Check connection string format
   - Ensure network connectivity

2. **Authentication Failed**:
   - Verify username and password
   - Check authentication database
   - Ensure user has required permissions

3. **Schema Generation Failed**:
   - Verify Prisma schema syntax
   - Check MongoDB provider configuration
   - Ensure independent schema usage

4. **Query Performance Issues**:
   - Check index usage with `explain()`
   - Monitor MongoDB logs
   - Optimize aggregation pipelines

### Debugging Queries

```typescript
// Enable MongoDB query logging
const db = await DatabaseClient.getInstance('mongodb', {
  log: ['query', 'info', 'warn', 'error']
});

// Use explain for query analysis
const explainResult = await db.prisma.mongodbUser.findMany({
  where: { role: 'author' }
}).explain();
```

### Logs

Enable debug logging by setting `DATABASE_LOG_LEVEL=debug` in your environment.

## Migration Guide

### From SQL Databases

1. **Denormalize Data**: Convert normalized tables to embedded documents
2. **Reference vs Embed**: Decide on referencing vs embedding strategy
3. **Index Strategy**: Plan indexes for MongoDB query patterns
4. **Data Types**: Convert SQL types to MongoDB equivalents

### From Other NoSQL

1. **Document Structure**: Adapt document schemas to Prisma requirements
2. **Query Patterns**: Convert queries to Prisma syntax or aggregation pipelines
3. **Index Migration**: Recreate indexes appropriate for new schema

## Security Best Practices

1. **Authentication**: Always use authentication in production
2. **Authorization**: Implement role-based access control
3. **Network Security**: Use SSL/TLS for connections
4. **Input Validation**: Validate all input data
5. **Query Injection**: Use Prisma's built-in protection

## Performance Optimization

1. **Indexing Strategy**: Create indexes for frequently queried fields
2. **Document Design**: Optimize for read patterns
3. **Aggregation**: Use efficient aggregation pipelines
4. **Connection Pooling**: Configure appropriate pool sizes
5. **Sharding**: Plan for horizontal scaling

## Production Checklist

- [ ] Authentication configured and tested
- [ ] Indexes created for all query patterns
- [ ] Connection pooling optimized
- [ ] Backup strategy implemented
- [ ] Monitoring and alerting configured
- [ ] SSL/TLS encryption enabled
- [ ] Performance benchmarking completed
- [ ] Security policies implemented
