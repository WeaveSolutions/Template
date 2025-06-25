# Microsoft SQL Server Integration

This document provides instructions on setting up and using the Microsoft SQL Server integration with our database framework for enterprise-grade relational database operations.

## Overview

The Microsoft SQL Server integration provides a robust, enterprise-class relational database solution with advanced features like stored procedures, triggers, full-text search, and comprehensive security. This implementation supports multi-schema configurations and provides enterprise-grade User/Post/Role management with proper normalization.

## Configuration

### Environment Variables

Add the following environment variables to your `.env` file:

```bash
# SQL Server Configuration
ENABLE_SQLSERVER=true
SQLSERVER_URL=sqlserver://localhost:1433;database=nexpo_db;user=sa;password=YourPassword123;trustServerCertificate=true;encrypt=true
SQLSERVER_HOST=localhost
SQLSERVER_PORT=1433
SQLSERVER_DATABASE=nexpo_db
SQLSERVER_USERNAME=nexpo_user
SQLSERVER_PASSWORD=secure_password
SQLSERVER_INSTANCE=MSSQLSERVER
SQLSERVER_ENCRYPT=true
SQLSERVER_TRUST_SERVER_CERTIFICATE=true

# Azure SQL Database
AZURE_SQL_SERVER=your-server.database.windows.net
AZURE_SQL_DATABASE=nexpo_db
AZURE_SQL_USERNAME=nexpo_admin
AZURE_SQL_PASSWORD=secure_password
AZURE_SQL_CONNECTION_STRING=Server=tcp:your-server.database.windows.net,1433;Initial Catalog=nexpo_db;Persist Security Info=False;User ID=nexpo_admin;Password=secure_password;MultipleActiveResultSets=False;Encrypt=True;TrustServerCertificate=False;Connection Timeout=30;
```

### Required Variables

| Variable | Description | Required |
|----------|-------------|----------|
| `ENABLE_SQLSERVER` | Set to `true` to enable SQL Server provider | Yes |
| `SQLSERVER_URL` | Complete SQL Server connection string | Yes |
| `SQLSERVER_HOST` | SQL Server host | Yes |
| `SQLSERVER_PORT` | SQL Server port (default: 1433) | Yes |
| `SQLSERVER_DATABASE` | Database name | Yes |
| `SQLSERVER_USERNAME` | Database username | Yes |
| `SQLSERVER_PASSWORD` | Database password | Yes |
| `SQLSERVER_ENCRYPT` | Enable encryption (true/false) | Yes |
| `SQLSERVER_TRUST_SERVER_CERTIFICATE` | Trust server certificate | Yes |

## Schema Generation

SQL Server uses the schema defined in `prisma/providers/sqlserver.prisma`. To generate Prisma client for this schema:

```bash
PRISMA_SCHEMA=prisma/providers/sqlserver.prisma npx prisma generate
```

## Database Schema

### Models Overview

The SQL Server schema implements a comprehensive enterprise structure:

- **SqlServerUser**: Core user model with enterprise features
- **SqlServerPost**: Content management with publishing workflow
- **SqlServerRole**: Role-based access control with descriptions

### Schema Structure

```typescript
// User Model
type SqlServerUser = {
  id: string;              // CUID identifier
  email: string;           // Unique email address
  name?: string;           // Optional display name
  posts: SqlServerPost[];  // One-to-many relationship
  roles: SqlServerRole[];  // One-to-many relationship
  createdAt: Date;         // Creation timestamp
  updatedAt: Date;         // Last update timestamp
  deletedAt?: Date;        // Soft delete timestamp
}

// Post Model
type SqlServerPost = {
  id: string;              // CUID identifier
  title: string;           // Post title
  content?: string;        // Post content
  published: boolean;      // Publication status
  authorId: string;        // Foreign key to SqlServerUser
  author: SqlServerUser;   // Author relationship
  createdAt: Date;
  updatedAt: Date;
  deletedAt?: Date;        // Soft delete support
}

// Role Model
type SqlServerRole = {
  id: string;              // CUID identifier
  name: string;            // Unique role name
  description?: string;    // Role description
  userId: string;          // Foreign key to SqlServerUser
  user: SqlServerUser;     // User relationship
  createdAt: Date;
}
```

## Usage

### Basic Usage

```typescript
import { DatabaseClient } from '@your-package/shared-db';

// Initialize with SQL Server provider
const db = await DatabaseClient.getInstance('sqlserver');

// Create a user with related data
const user = await db.prisma.sqlServerUser.create({
  data: {
    email: 'user@example.com',
    name: 'John Doe',
    posts: {
      create: [
        {
          title: 'Enterprise Post',
          content: 'Content for enterprise application',
          published: true
        },
        {
          title: 'Draft Post',
          content: 'This is still a draft',
          published: false
        }
      ]
    },
    roles: {
      create: [
        {
          name: 'content-manager',
          description: 'Manages content creation and publishing'
        },
        {
          name: 'author',
          description: 'Creates written content'
        }
      ]
    }
  },
  include: {
    posts: true,
    roles: true
  }
});

// Complex queries with joins
const usersWithPublishedPosts = await db.prisma.sqlServerUser.findMany({
  where: {
    posts: {
      some: {
        published: true,
        deletedAt: null
      }
    }
  },
  include: {
    posts: {
      where: {
        published: true,
        deletedAt: null
      },
      orderBy: { createdAt: 'desc' }
    },
    roles: true
  }
});

// Advanced SQL queries
const userStatistics = await db.executeRaw(`
  SELECT 
    u.id,
    u.email,
    u.name,
    COUNT(p.id) as total_posts,
    SUM(CASE WHEN p.published = 1 THEN 1 ELSE 0 END) as published_posts,
    STRING_AGG(r.name, ', ') as roles,
    MAX(p.createdAt) as latest_post_date
  FROM sqlserver_users u
  LEFT JOIN sqlserver_posts p ON u.id = p.authorId AND p.deletedAt IS NULL
  LEFT JOIN sqlserver_roles r ON u.id = r.userId
  WHERE u.deletedAt IS NULL
  GROUP BY u.id, u.email, u.name
  ORDER BY published_posts DESC, latest_post_date DESC
`);

// Disconnect when done
await db.disconnect();
```

### Advanced Features

#### Stored Procedures

```typescript
// Create stored procedure (run once during setup)
await db.executeRaw(`
  CREATE OR ALTER PROCEDURE GetUserContentSummary
    @UserId NVARCHAR(30)
  AS
  BEGIN
    SET NOCOUNT ON;
    
    SELECT 
      u.id,
      u.email,
      u.name,
      COUNT(p.id) as TotalPosts,
      SUM(CASE WHEN p.published = 1 THEN 1 ELSE 0 END) as PublishedPosts,
      MAX(p.createdAt) as LatestPostDate,
      STRING_AGG(r.name, ', ') as UserRoles
    FROM sqlserver_users u
    LEFT JOIN sqlserver_posts p ON u.id = p.authorId AND p.deletedAt IS NULL
    LEFT JOIN sqlserver_roles r ON u.id = r.userId
    WHERE u.id = @UserId AND u.deletedAt IS NULL
    GROUP BY u.id, u.email, u.name;
  END
`);

// Execute stored procedure
const userSummary = await db.executeRaw(`
  EXEC GetUserContentSummary @UserId = $1
`, [user.id]);
```

#### Transactions with Isolation Levels

```typescript
// Transaction with specific isolation level
const result = await db.prisma.$transaction(async (tx) => {
  // Set isolation level for this transaction
  await tx.$executeRaw`SET TRANSACTION ISOLATION LEVEL READ COMMITTED`;
  
  // Create user
  const user = await tx.sqlServerUser.create({
    data: {
      email: 'enterprise@example.com',
      name: 'Enterprise User'
    }
  });
  
  // Create posts atomically
  const posts = await Promise.all([
    tx.sqlServerPost.create({
      data: {
        title: 'Post 1',
        content: 'Content 1',
        authorId: user.id,
        published: true
      }
    }),
    tx.sqlServerPost.create({
      data: {
        title: 'Post 2',
        content: 'Content 2',
        authorId: user.id,
        published: false
      }
    })
  ]);
  
  // Create role with validation
  const role = await tx.sqlServerRole.create({
    data: {
      name: 'enterprise-user',
      description: 'Enterprise system user',
      userId: user.id
    }
  });
  
  return { user, posts, role };
}, {
  isolationLevel: 'ReadCommitted',
  timeout: 30000
});
```

#### Full-Text Search

```typescript
// Create full-text index (run once during setup)
await db.executeRaw(`
  -- Create full-text catalog
  IF NOT EXISTS (SELECT * FROM sys.fulltext_catalogs WHERE name = 'PostSearchCatalog')
    CREATE FULLTEXT CATALOG PostSearchCatalog;
  
  -- Create full-text index on posts
  IF NOT EXISTS (SELECT * FROM sys.fulltext_indexes WHERE object_id = OBJECT_ID('sqlserver_posts'))
    CREATE FULLTEXT INDEX ON sqlserver_posts(title, content) 
    KEY INDEX PK_sqlserver_posts_id
    ON PostSearchCatalog;
`);

// Full-text search queries
const searchResults = await db.executeRaw(`
  SELECT 
    p.*,
    u.name as author_name,
    KEY_TBL.RANK as search_rank
  FROM sqlserver_posts p
  INNER JOIN CONTAINSTABLE(sqlserver_posts, (title, content), $1) AS KEY_TBL
    ON p.id = KEY_TBL.[KEY]
  INNER JOIN sqlserver_users u ON p.authorId = u.id
  WHERE p.published = 1 AND p.deletedAt IS NULL
  ORDER BY KEY_TBL.RANK DESC
`, ['"enterprise development" OR "SQL Server"']);
```

#### Common Table Expressions (CTEs)

```typescript
// Hierarchical queries with CTEs
const hierarchicalData = await db.executeRaw(`
  WITH UserHierarchy AS (
    -- Base case: users with specific roles
    SELECT 
      u.id,
      u.email,
      u.name,
      r.name as role_name,
      0 as level
    FROM sqlserver_users u
    INNER JOIN sqlserver_roles r ON u.id = r.userId
    WHERE r.name = 'admin'
    
    UNION ALL
    
    -- Recursive case: users managed by other users
    SELECT 
      u.id,
      u.email,
      u.name,
      r.name as role_name,
      uh.level + 1
    FROM sqlserver_users u
    INNER JOIN sqlserver_roles r ON u.id = r.userId
    INNER JOIN UserHierarchy uh ON r.description LIKE '%managed by%'
    WHERE uh.level < 5  -- Prevent infinite recursion
  )
  SELECT * FROM UserHierarchy
  ORDER BY level, email;
`);
```

#### Window Functions

```typescript
// Advanced analytics with window functions
const postAnalytics = await db.executeRaw(`
  SELECT 
    p.id,
    p.title,
    p.createdAt,
    u.name as author_name,
    COUNT(*) OVER (PARTITION BY p.authorId) as author_post_count,
    ROW_NUMBER() OVER (PARTITION BY p.authorId ORDER BY p.createdAt DESC) as post_rank,
    LAG(p.createdAt) OVER (PARTITION BY p.authorId ORDER BY p.createdAt) as previous_post_date,
    DATEDIFF(day, LAG(p.createdAt) OVER (PARTITION BY p.authorId ORDER BY p.createdAt), p.createdAt) as days_since_last_post
  FROM sqlserver_posts p
  INNER JOIN sqlserver_users u ON p.authorId = u.id
  WHERE p.published = 1 AND p.deletedAt IS NULL
  ORDER BY u.name, p.createdAt DESC;
`);
```

## Testing

A test script is available to validate your SQL Server connection:

```bash
npx ts-node packages/shared-db/scripts/test-sqlserver-provider.ts
```

### Manual Testing

```typescript
// Test SQL Server functionality
import { DatabaseClient } from '@your-package/shared-db';

async function testSQLServer() {
  const db = await DatabaseClient.getInstance('sqlserver');
  
  // Test user CRUD operations
  const testUser = await db.prisma.sqlServerUser.create({
    data: {
      email: 'test@example.com',
      name: 'Test User'
    }
  });
  
  // Test post creation with foreign key
  const testPost = await db.prisma.sqlServerPost.create({
    data: {
      title: 'Enterprise Test Post',
      content: 'This is a test post for SQL Server',
      authorId: testUser.id,
      published: true
    }
  });
  
  // Test role creation and assignment
  const testRole = await db.prisma.sqlServerRole.create({
    data: {
      name: 'tester',
      description: 'Test role for validation',
      userId: testUser.id
    }
  });
  
  // Test complex join query
  const userWithRelations = await db.prisma.sqlServerUser.findUnique({
    where: { id: testUser.id },
    include: {
      posts: true,
      roles: true
    }
  });
  
  // Test transaction
  const transactionResult = await db.prisma.$transaction(async (tx) => {
    const updatedPost = await tx.sqlServerPost.update({
      where: { id: testPost.id },
      data: { title: 'Updated Title' }
    });
    
    const newRole = await tx.sqlServerRole.create({
      data: {
        name: 'editor',
        description: 'Content editor role',
        userId: testUser.id
      }
    });
    
    return { updatedPost, newRole };
  });
  
  console.log('✅ SQL Server integration working correctly');
  console.log('User:', userWithRelations);
  console.log('Transaction result:', transactionResult);
  
  // Cleanup
  await db.prisma.sqlServerRole.deleteMany({ where: { userId: testUser.id } });
  await db.prisma.sqlServerPost.delete({ where: { id: testPost.id } });
  await db.prisma.sqlServerUser.delete({ where: { id: testUser.id } });
  await db.disconnect();
}

testSQLServer().catch(console.error);
```

## Current Problems & Solutions

### Connection Management

**Problem**: Connection timeout under high load
- **Solution**: Implemented connection pooling with proper timeout settings
- **Current Status**: ✅ Resolved

**Problem**: SSL/TLS certificate validation issues
- **Solution**: Added trustServerCertificate option for development
- **Current Status**: ✅ Configured

### Performance Optimization

**Problem**: Slow queries on large datasets
- **Solution**: Implemented comprehensive indexing strategy
- **Current Status**: ✅ Optimized

```sql
-- Implemented indexes
CREATE INDEX IX_sqlserver_users_email ON sqlserver_users(email);
CREATE INDEX IX_sqlserver_users_created_at ON sqlserver_users(createdAt);
CREATE INDEX IX_sqlserver_posts_author_id ON sqlserver_posts(authorId);
CREATE INDEX IX_sqlserver_posts_published ON sqlserver_posts(published);
CREATE INDEX IX_sqlserver_posts_author_published ON sqlserver_posts(authorId, published);
CREATE INDEX IX_sqlserver_roles_name ON sqlserver_roles(name);
CREATE INDEX IX_sqlserver_roles_user_id ON sqlserver_roles(userId);

-- Composite indexes for common queries
CREATE INDEX IX_sqlserver_posts_published_created ON sqlserver_posts(published, createdAt DESC) WHERE deletedAt IS NULL;
```

### Schema Management

**Problem**: Complex schema migrations in production
- **Solution**: Implemented safe migration patterns with backup/rollback
- **Current Status**: ✅ Documented

```sql
-- Safe schema migration example
BEGIN TRANSACTION;

-- Add new column with default
ALTER TABLE sqlserver_users ADD profile_url NVARCHAR(500) NULL;

-- Create index
CREATE INDEX IX_sqlserver_users_profile_url ON sqlserver_users(profile_url) WHERE profile_url IS NOT NULL;

-- Verify migration
IF @@ERROR = 0
  COMMIT TRANSACTION;
ELSE
  ROLLBACK TRANSACTION;
```

## Limitations

1. **Connection Limits**: SQL Server has connection limits requiring pooling
2. **Case Sensitivity**: Collation settings affect string comparisons
3. **Data Type Mapping**: Some Prisma types may not map perfectly to SQL Server
4. **Licensing Costs**: Enterprise features require appropriate licensing
5. **Platform Dependency**: Windows-centric but supports Linux containers

## Troubleshooting

### Common Issues

1. **Connection Failed**:
   - Verify SQL Server is running and accessible
   - Check connection string format
   - Ensure network connectivity and firewall rules

2. **Authentication Failed**:
   - Verify username and password
   - Check SQL Server authentication mode
   - Ensure user has database access permissions

3. **Schema Sync Issues**:
   - Run `npx prisma db push` to sync schema
   - Check for migration conflicts
   - Verify user has DDL permissions

4. **Performance Issues**:
   - Use SQL Server Management Studio to analyze execution plans
   - Check index usage and fragmentation
   - Monitor blocking and deadlocks

### Query Optimization

```typescript
// Enable query logging and analysis
const db = await DatabaseClient.getInstance('sqlserver', {
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
  
  // Log slow queries for optimization
  if (e.duration > 1000) {
    console.warn('Slow query detected:', e.query);
  }
});
```

### Logs

Enable debug logging by setting `DATABASE_LOG_LEVEL=debug` in your environment.

## Migration Guide

### From PostgreSQL

1. **Data Type Conversion**: Map PostgreSQL types to SQL Server equivalents
2. **Function Translation**: Convert PostgreSQL functions to T-SQL
3. **Schema Migration**: Use SQL Server Migration Assistant (SSMA)
4. **Index Strategy**: Recreate indexes optimized for SQL Server

### From MySQL

1. **Engine Differences**: Adapt to SQL Server's transaction model
2. **Data Type Mapping**: Convert MySQL types to SQL Server types
3. **Stored Procedure Conversion**: Rewrite MySQL procedures in T-SQL
4. **Character Set Handling**: Configure proper collation settings

## Security Best Practices

1. **Authentication**: Use Windows Authentication when possible
2. **Encryption**: Enable TLS encryption for all connections
3. **Permissions**: Follow principle of least privilege
4. **Audit Logging**: Enable SQL Server audit features
5. **Backup Security**: Encrypt backup files and secure storage

## Performance Optimization

1. **Indexing Strategy**: Create targeted indexes for query patterns
2. **Query Optimization**: Use execution plans to optimize queries
3. **Connection Pooling**: Configure appropriate pool sizes
4. **Memory Management**: Tune SQL Server memory settings
5. **Statistics Maintenance**: Keep query statistics up to date

## Production Checklist

- [ ] Connection pooling configured and tested
- [ ] SSL/TLS encryption enabled and verified
- [ ] Backup strategy implemented and tested
- [ ] Monitoring and alerting configured
- [ ] Indexes optimized for workload
- [ ] Security policies implemented and audited
- [ ] Performance benchmarking completed
- [ ] Disaster recovery plan prepared and tested
- [ ] Maintenance plans configured
- [ ] Documentation updated and accessible
