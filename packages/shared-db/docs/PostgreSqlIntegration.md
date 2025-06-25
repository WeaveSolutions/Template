# PostgreSQL Integration

This document provides instructions on setting up and using the PostgreSQL integration with our database framework for standard relational database operations.

## Overview

The PostgreSQL integration provides a robust, ACID-compliant relational database solution with advanced features like JSON support, full-text search, and comprehensive indexing. This implementation supports multi-schema configurations and provides a standard User/Post/Role model structure.

## Configuration

### Environment Variables

Add the following environment variables to your `.env` file:

```bash
# PostgreSQL Configuration
ENABLE_POSTGRES=true
POSTGRES_PRISMA_URL=postgresql://user:password@localhost:5432/nexpo_db?pgbouncer=true&connect_timeout=15
POSTGRES_URL_NON_POOLING=postgresql://user:password@localhost:5432/nexpo_db
POSTGRES_USER=nexpo_user
POSTGRES_PASSWORD=secure_password
POSTGRES_DATABASE=nexpo_db
POSTGRES_HOST=localhost
POSTGRES_PORT=5432
```

### Required Variables

| Variable | Description | Required |
|----------|-------------|----------|
| `ENABLE_POSTGRES` | Set to `true` to enable PostgreSQL provider | Yes |
| `POSTGRES_PRISMA_URL` | Connection pooling URL with PgBouncer support | Yes |
| `POSTGRES_URL_NON_POOLING` | Direct connection URL (no pooling) | Yes |
| `POSTGRES_USER` | Database username | Yes |
| `POSTGRES_PASSWORD` | Database password | Yes |
| `POSTGRES_DATABASE` | Database name | Yes |
| `POSTGRES_HOST` | Database host | Yes |
| `POSTGRES_PORT` | Database port (default: 5432) | Yes |

## Schema Generation

PostgreSQL uses the schema defined in `prisma/providers/postgres.prisma`. To generate Prisma client for this schema:

```bash
PRISMA_SCHEMA=prisma/providers/postgres.prisma npx prisma generate
```

## Database Schema

### Models Overview

The PostgreSQL schema implements a standard content management structure:

- **PostgresUser**: Core user model with email, name, and timestamps
- **PostgresPost**: Content posts linked to users with publishing status
- **PostgresRole**: User role management with descriptions

### Schema Structure

```typescript
// User Model
type PostgresUser = {
  id: string;          // CUID identifier
  email: string;       // Unique email address
  name?: string;       // Optional display name
  posts: PostgresPost[]; // One-to-many relationship
  roles: PostgresRole[]; // One-to-many relationship
  createdAt: Date;
  updatedAt: Date;
  deletedAt?: Date;    // Soft delete support
}

// Post Model
type PostgresPost = {
  id: string;
  title: string;
  content?: string;
  published: boolean;
  authorId: string;
  author: PostgresUser;
  createdAt: Date;
  updatedAt: Date;
  deletedAt?: Date;
}

// Role Model
type PostgresRole = {
  id: string;
  name: string;        // Unique role name
  description?: string;
  userId: string;
  user: PostgresUser;
  createdAt: Date;
}
```

## Usage

### Basic Usage

```typescript
import { DatabaseClient } from '@your-package/shared-db';

// Initialize with PostgreSQL provider
const db = await DatabaseClient.getInstance('postgres');

// Create a user with posts and roles
const user = await db.prisma.postgresUser.create({
  data: {
    email: 'user@example.com',
    name: 'John Doe',
    posts: {
      create: [
        {
          title: 'First Post',
          content: 'Hello World!',
          published: true
        }
      ]
    },
    roles: {
      create: [
        {
          name: 'author',
          description: 'Content author role'
        }
      ]
    }
  },
  include: {
    posts: true,
    roles: true
  }
});

// Query with relationships
const usersWithPosts = await db.prisma.postgresUser.findMany({
  include: {
    posts: {
      where: { published: true },
      orderBy: { createdAt: 'desc' }
    },
    roles: true
  }
});

// Raw SQL queries
const result = await db.executeRaw(`
  SELECT 
    u.id,
    u.email,
    u.name,
    COUNT(p.id) as post_count,
    STRING_AGG(r.name, ', ') as roles
  FROM pg_users u
  LEFT JOIN pg_posts p ON u.id = p."authorId"
  LEFT JOIN pg_roles r ON u.id = r."userId"
  GROUP BY u.id, u.email, u.name
  ORDER BY post_count DESC
`);

// Disconnect when done
await db.disconnect();
```

### Advanced Features

#### Transactions

```typescript
const result = await db.prisma.$transaction(async (tx) => {
  // Create user
  const user = await tx.postgresUser.create({
    data: {
      email: 'author@example.com',
      name: 'Content Author'
    }
  });
  
  // Create multiple posts atomically
  const posts = await Promise.all([
    tx.postgresPost.create({
      data: {
        title: 'Post 1',
        content: 'Content 1',
        authorId: user.id,
        published: true
      }
    }),
    tx.postgresPost.create({
      data: {
        title: 'Post 2',
        content: 'Content 2',
        authorId: user.id,
        published: false
      }
    })
  ]);
  
  // Create role
  const role = await tx.postgresRole.create({
    data: {
      name: 'content-creator',
      description: 'Creates and manages content',
      userId: user.id
    }
  });
  
  return { user, posts, role };
});
```

#### Full-Text Search

```typescript
// Search posts by content
const searchResults = await db.executeRaw(`
  SELECT 
    p.*,
    u.name as author_name,
    ts_rank(to_tsvector('english', p.title || ' ' || COALESCE(p.content, '')), plainto_tsquery('english', $1)) as rank
  FROM pg_posts p
  JOIN pg_users u ON p."authorId" = u.id
  WHERE to_tsvector('english', p.title || ' ' || COALESCE(p.content, '')) @@ plainto_tsquery('english', $1)
  AND p.published = true
  ORDER BY rank DESC
`, ['search term']);
```

#### JSON Operations

```typescript
// Add JSON metadata to posts
await db.executeRaw(`
  ALTER TABLE pg_posts ADD COLUMN metadata JSONB DEFAULT '{}'::jsonb
`);

// Query JSON data
const postsWithMetadata = await db.executeRaw(`
  SELECT * FROM pg_posts 
  WHERE metadata->>'category' = 'tutorial'
  AND (metadata->>'tags')::jsonb ? 'programming'
`);
```

#### Connection Pooling

```typescript
// Configure connection pool
const db = await DatabaseClient.getInstance('postgres', {
  datasources: {
    pgdb: {
      url: process.env.POSTGRES_PRISMA_URL
    }
  },
  log: ['query', 'info', 'warn', 'error']
});
```

## Testing

A test script is available to validate your PostgreSQL connection:

```bash
npx ts-node packages/shared-db/scripts/test-postgres-provider.ts
```

### Manual Testing

```typescript
// Test PostgreSQL functionality
import { DatabaseClient } from '@your-package/shared-db';

async function testPostgreSQL() {
  const db = await DatabaseClient.getInstance('postgres');
  
  // Test user CRUD
  const testUser = await db.prisma.postgresUser.create({
    data: {
      email: 'test@example.com',
      name: 'Test User'
    }
  });
  
  // Test post creation
  const testPost = await db.prisma.postgresPost.create({
    data: {
      title: 'Test Post',
      content: 'This is a test post',
      authorId: testUser.id,
      published: true
    }
  });
  
  // Test role assignment
  const testRole = await db.prisma.postgresRole.create({
    data: {
      name: 'tester',
      description: 'Test role',
      userId: testUser.id
    }
  });
  
  // Test complex query
  const userWithRelations = await db.prisma.postgresUser.findUnique({
    where: { id: testUser.id },
    include: {
      posts: true,
      roles: true
    }
  });
  
  console.log('✅ PostgreSQL integration working correctly');
  console.log('User:', userWithRelations);
  
  // Cleanup
  await db.prisma.postgresRole.delete({ where: { id: testRole.id } });
  await db.prisma.postgresPost.delete({ where: { id: testPost.id } });
  await db.prisma.postgresUser.delete({ where: { id: testUser.id } });
  await db.disconnect();
}

testPostgreSQL().catch(console.error);
```

## Current Problems & Solutions

### Connection Issues

**Problem**: Connection timeout with high load
- **Solution**: Implemented connection pooling with PgBouncer
- **Current Status**: ✅ Resolved

**Problem**: SSL certificate validation
- **Solution**: Added SSL configuration options
- **Current Status**: ✅ Configured

```typescript
// SSL configuration
const connectionString = `${process.env.POSTGRES_PRISMA_URL}?sslmode=require&sslcert=client-cert.pem&sslkey=client-key.pem&sslrootcert=ca-cert.pem`;
```

### Performance Optimization

**Problem**: Slow queries on large datasets
- **Solution**: Implemented comprehensive indexing strategy
- **Current Status**: ✅ Optimized

```sql
-- Implemented indexes
CREATE INDEX pg_users_email_idx ON pg_users(email);
CREATE INDEX pg_users_created_at_idx ON pg_users(created_at);
CREATE INDEX pg_posts_author_id_idx ON pg_posts("authorId");
CREATE INDEX pg_posts_published_idx ON pg_posts(published);
CREATE INDEX pg_roles_name_idx ON pg_roles(name);
CREATE INDEX pg_roles_user_id_idx ON pg_roles("userId");

-- Composite indexes for common queries
CREATE INDEX pg_posts_author_published_idx ON pg_posts("authorId", published);
CREATE INDEX pg_posts_published_created_idx ON pg_posts(published, created_at DESC);
```

### Schema Migration

**Problem**: Complex schema migrations in production
- **Solution**: Implemented safe migration patterns
- **Current Status**: ✅ Documented

```typescript
// Safe migration example
await db.executeRaw(`
  BEGIN;
  
  -- Add new column with default
  ALTER TABLE pg_users ADD COLUMN profile_image VARCHAR(500) DEFAULT NULL;
  
  -- Create index concurrently
  CREATE INDEX CONCURRENTLY pg_users_profile_image_idx ON pg_users(profile_image) WHERE profile_image IS NOT NULL;
  
  COMMIT;
`);
```

## Limitations

1. **Multi-Schema Complexity**: Requires careful namespace management
2. **Connection Limits**: PostgreSQL has connection limits that require pooling
3. **JSON Performance**: JSONB operations can be slower than dedicated NoSQL
4. **Full-Text Search**: Limited compared to dedicated search engines

## Troubleshooting

### Common Issues

1. **Connection Failed**:
   - Verify connection string format
   - Check PostgreSQL server status
   - Ensure database exists and user has permissions

2. **Permission Denied**:
   - Verify user credentials
   - Check database and table permissions
   - Ensure user has required privileges

3. **Schema Sync Issues**:
   - Run `npx prisma db push` to sync schema
   - Check for migration conflicts
   - Verify schema generation completed

4. **Performance Issues**:
   - Monitor query execution plans
   - Check index usage
   - Analyze connection pool settings

### Debugging Queries

```typescript
// Enable query logging
const db = await DatabaseClient.getInstance('postgres', {
  log: [
    {
      emit: 'event',
      level: 'query',
    },
  ],
});

db.prisma.$on('query', (e) => {
  console.log('Query:', e.query);
  console.log('Params:', e.params);
  console.log('Duration:', e.duration + 'ms');
});
```

### Logs

Enable debug logging by setting `DATABASE_LOG_LEVEL=debug` in your environment.

## Migration Guide

### From MySQL

1. Use `pg_loader` or similar tools for data migration
2. Convert MySQL-specific syntax (AUTO_INCREMENT → SERIAL)
3. Update data types (TEXT → VARCHAR with length)
4. Modify stored procedures to PostgreSQL functions

### From SQLite

1. Export SQLite data using `.dump` command
2. Convert SQLite types to PostgreSQL equivalents
3. Handle autoincrement and foreign key differences
4. Test constraints and indexes

## Security Best Practices

1. **Connection Security**: Always use SSL in production
2. **User Permissions**: Follow principle of least privilege
3. **Query Parameterization**: Use Prisma's built-in parameterization
4. **Row Level Security**: Implement RLS for multi-tenant applications
5. **Audit Logging**: Enable PostgreSQL logging for security monitoring

## Performance Optimization

1. **Connection Pooling**: Use PgBouncer or similar
2. **Index Strategy**: Monitor and optimize indexes regularly
3. **Query Analysis**: Use EXPLAIN ANALYZE for optimization
4. **Vacuum Strategy**: Configure autovacuum appropriately
5. **Memory Configuration**: Tune shared_buffers and work_mem

## Production Checklist

- [ ] Connection pooling configured
- [ ] SSL certificates installed and verified
- [ ] Backup strategy implemented
- [ ] Monitoring and alerting configured
- [ ] Indexes optimized for workload
- [ ] Security policies implemented
- [ ] Performance benchmarking completed
- [ ] Migration rollback plan prepared
