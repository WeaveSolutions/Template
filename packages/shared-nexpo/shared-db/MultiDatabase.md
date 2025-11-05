# Shared Database Package

This package provides a unified database access layer for the application, supporting multiple database providers with Prisma using a provider-based architecture. It implements fine-grained access control through Auth0's role-based access control (RBAC) system and the Auth0 Management API.

## Features

- **Provider-based architecture** for clean separation of database implementations
- **Multi-database support**: PostgreSQL, MongoDB, Supabase, CosmosDB, SQL Server, DB2, and more
- **Type-safe database access** with Prisma
- **Isolated Prisma clients** to prevent cross-database relations
- **Connection pooling** for optimal performance
- **Transaction support** with retry logic
- **Pagination utilities**
- **Soft delete** functionality
- **Database seeding** for development and testing
- **Middleware** for logging, performance monitoring, and more
- **Auth0 integration** for fine-grained access control across all database types
- **Multi-tenant support** with database-per-tenant, schema-per-tenant, and RLS strategies

## Supported Cloud Providers & Databases

### Amazon Web Services (AWS)
- **RDS**: PostgreSQL, MySQL, MariaDB, Oracle, SQL Server with IAM database authentication
- **DynamoDB**: NoSQL with fine-grained access control using IAM policies and Auth0 JWT claims
- **DocumentDB**: MongoDB-compatible with VPC security groups and Auth0 role validation
- **ElastiCache**: Redis and Memcached with Auth0 token-based access patterns
- **Neptune**: Graph database with SPARQL/Gremlin query authorization
- **Timestream**: Time-series database with Auth0 user context filtering

### Google Cloud Platform (GCP)
- **Cloud SQL**: PostgreSQL, MySQL, SQL Server with Cloud IAM and Auth0 integration
- **Firestore**: NoSQL document database with security rules validating Auth0 JWT claims
- **Firebase Realtime Database**: Real-time synchronization with Auth0 user-based security rules
- **Cloud Spanner**: Globally distributed relational database with fine-grained access control
- **Cloud Bigtable**: Wide-column NoSQL for analytics with Auth0 user filtering
- **Cloud Memorystore**: Redis and Memcached with VPC-native security

### Microsoft Azure
- **Azure SQL Database**: Relational database with Azure AD integration and Auth0 federation
- **Cosmos DB**: Multi-model NoSQL with Auth0 JWT validation in stored procedures
- **Azure Database for PostgreSQL/MySQL**: Managed databases with Auth0 role-based access
- **Azure Cache for Redis**: In-memory caching with Auth0 token validation
- **Azure Digital Twins**: Graph database for IoT with Auth0 device authorization

### Oracle Cloud Infrastructure (OCI)
- **Autonomous Database**: Self-managing Oracle database with Auth0 user provisioning
- **MySQL Database Service**: Managed MySQL with OCI IAM and Auth0 integration
- **NoSQL Database**: Document and key-value store with Auth0 access policies
- **Analytics Cloud**: Data warehouse with Auth0 user context for row-level security

### IBM Cloud
- **Db2 on Cloud**: Enterprise relational database with Auth0 user mapping
- **Cloudant**: Apache CouchDB-based NoSQL with Auth0 document-level permissions
- **Databases for PostgreSQL/MongoDB**: Managed databases with Auth0 integration
- **Redis**: In-memory database with Auth0 connection authentication

### DigitalOcean
- **Managed Databases**: PostgreSQL, MySQL, Redis with Auth0 connection pooling
- **App Platform**: Database integration with Auth0 environment variables
- **Spaces**: Object storage with Auth0 presigned URL generation

### Specialized Database Providers

**Supabase Integration**
- **PostgreSQL Database**: Row-level security (RLS) policies validating Auth0 JWT claims
- **Realtime Subscriptions**: WebSocket connections authenticated via Auth0 tokens
- **Edge Functions**: Serverless compute with Auth0 context for database operations
- **Storage Buckets**: File access control using Auth0 user metadata and roles

**Firebase Integration**
- **Cloud Firestore**: Security rules engine with Auth0 custom claims validation
- **Realtime Database**: Firebase rules validating Auth0 JWT token structures
- **Cloud Storage**: File access rules based on Auth0 user context and permissions
- **Cloud Functions**: Serverless functions with Auth0 token exchange patterns

**MongoDB Atlas**
- **Database Access**: Role-based access control synchronized with Auth0 user roles
- **Application-Level Security**: Field-level permissions using Auth0 metadata
- **Atlas Search**: Full-text search with Auth0 user context filtering
- **Change Streams**: Real-time data synchronization with Auth0 webhook integration

**MindsDB Integration**
- **AI-Powered Analytics**: Machine learning models with Auth0 user context
- **Natural Language Queries**: SQL-like queries with Auth0 permission validation
- **Predictive Models**: User-specific predictions based on Auth0 metadata
- **Data Preprocessing**: Automated data transformation with Auth0 audit trails

## Configuration

Add the following environment variables to your `.env` file to enable and configure multiple database providers. Each provider can be enabled or disabled independently using feature flags.

```env
# ============================================
# Database Provider Feature Flags
# Set to 'true' to enable a specific provider
# ============================================
ENABLE_POSTGRES=false
ENABLE_MONGODB=false
ENABLE_SUPABASE=false
ENABLE_COSMOSDB=false
ENABLE_SQLSERVER=false
ENABLE_DB2=false
ENABLE_REDIS=false
ENABLE_MEMCACHED=false
ENABLE_FIRESTORE=false
ENABLE_REALTIME_DATABASE=false
ENABLE_CLOUD_SQL=false
ENABLE_CLOUD_SPANNER=false
ENABLE_CLOUD_BIGTABLE=false
ENABLE_CLOUD_MEMORSTORE=false
ENABLE_AZURE_SQL_DATABASE=false
ENABLE_AZURE_COSMOS_DB=false
ENABLE_AZURE_CACHE_FOR_REDIS=false
ENABLE_AZURE_DIGITAL_TWINS=false
ENABLE_ORACLE_AUTONOMOUS_DATABASE=false
ENABLE_ORACLE_MYSQL_DATABASE_SERVICE=false
ENABLE_ORACLE_NOSQL_DATABASE=false
ENABLE_ORACLE_ANALYTICS_CLOUD=false
ENABLE_IBM_DB2_ON_CLOUD=false
ENABLE_IBM_CLOUDANT=false
ENABLE_IBM_DATABASES_FOR_POSTGRESQL_MONGODB=false
ENABLE_DIGITALOCEAN_MANAGED_DATABASES=false
ENABLE_DIGITALOCEAN_APP_PLATFORM=false
ENABLE_DIGITALOCEAN_SPACES=false
ENABLE_SUPABASE_INTEGRATION=false
ENABLE_FIREBASE_INTEGRATION=false
ENABLE_MONGODB_ATLAS=false
ENABLE_MINDSDB_INTEGRATION=false

# Default provider to use when multiple are enabled
DEFAULT_DATABASE_PROVIDER=postgres

# ============================================
# PostgreSQL Configuration
# ============================================
# PostgreSQL connection URL for Prisma
POSTGRES_PRISMA_URL=postgresql://user:password@host:5432/db?schema=public
POSTGRES_URL_NON_POOLING=postgresql://user:password@host:5432/db?schema=public

# ============================================
# MongoDB Configuration
# ============================================
# Standard MongoDB connection string
MONGODB_URI=mongodb://user:password@host:27017/db?authSource=admin&retryWrites=true&w=majority
# MongoDB Atlas connection string (alternative)
# MONGODB_URI=mongodb+srv://user:password@cluster.mongodb.net/db?retryWrites=true&w=majority

# ============================================
# Supabase Configuration
# ============================================
SUPABASE_DATABASE_URL=postgresql://postgres:[YOUR-PASSWORD]@db.[YOUR-REF].supabase.co:5432/postgres
SUPABASE_DIRECT_URL=postgresql://postgres:[YOUR-PASSWORD]@db.[YOUR-REF].supabase.co:6543/postgres
NEXT_PUBLIC_SUPABASE_URL=https://your-project.supabase.co
NEXT_PUBLIC_SUPABASE_ANON_KEY=your-anon-key
EXPO_PUBLIC_SUPABASE_URL=https://your-project.supabase.co
EXPO_PUBLIC_SUPABASE_ANON_KEY=your-anon-key

# ============================================
# Azure CosmosDB Configuration
# ============================================
COSMOSDB_MONGODB_URI=mongodb://[YOUR-COSMOSDB-NAME]:[YOUR-PASSWORD]@[YOUR-COSMOSDB-NAME].mongo.cosmos.azure.com:10255/?ssl=true&replicaSet=globaldb&retrywrites=false&maxIdleTimeMS=120000&appName=@[YOUR-COSMOSDB-NAME]@

# ============================================
# SQL Server Configuration
# ============================================
SQLSERVER_URL=sqlserver://[YOUR-SQL-SERVER]:1433;database=[YOUR-DATABASE];user=[YOUR-USERNAME];password=[YOUR-PASSWORD];encrypt=true

# ============================================
# IBM Cloud DB2 Configuration
# ============================================
DB2_URL=db2://user:password@hostname:port/database?currentSchema=schema&security=SSL
IBM_APIKEY=your-ibm-cloud-apikey
IBM_RESOURCE_INSTANCE_ID=your-db2-instance-id

# ============================================
# Advanced Configuration
# ============================================
# Log level for database operations (error, warn, info, debug)
DATABASE_LOG_LEVEL=info

# Enable Prisma query logging (true/false)
PRISMA_LOGGING=false
```

## Database Type Authorization Patterns

### Relational Databases (PostgreSQL, MySQL, SQL Server, Oracle)
- **Connection Pooling**: Auth0 JWT validation at connection establishment
- **Row-Level Security**: Database policies filtering data based on Auth0 user claims
- **Column-Level Encryption**: Field-level security using Auth0 user keys from Vault
- **Query Rewriting**: Dynamic WHERE clauses injected based on Auth0 permissions
- **Stored Procedures**: Database functions validating Auth0 token signatures

### NoSQL Databases (MongoDB, Firestore, DynamoDB, Cosmos DB)
- **Document-Level Permissions**: Access control at individual document granularity
- **Collection Security**: Database collection access based on Auth0 roles
- **Query Filtering**: NoSQL queries automatically scoped to authorized data subsets
- **Index Security**: Search indexes filtered by Auth0 user context
- **Aggregation Pipelines**: Data aggregation restricted by Auth0 permissions

### Time-Series Databases (InfluxDB, Timestream, TimescaleDB)
- **Temporal Access Control**: Time-range restrictions based on Auth0 user attributes
- **Metric Filtering**: Time-series data filtered by Auth0 organizational context
- **Retention Policies**: Data lifecycle management aligned with Auth0 user permissions
- **Downsampling Rules**: Data aggregation levels determined by Auth0 access levels

### In-Memory Databases (Redis, Memcached, ElastiCache)
- **Session Management**: Auth0 session data with automatic expiration
- **Cache Invalidation**: Auth0 user context changes triggering cache updates
- **Key Namespacing**: Cache keys prefixed with Auth0 user/tenant identifiers
- **Access Patterns**: Cache access restricted by Auth0 permission scopes

### Search Databases (Elasticsearch, Solr, Atlas Search)
- **Query Authorization**: Search queries filtered by Auth0 user permissions
- **Index Segmentation**: Search indexes partitioned by Auth0 organizational boundaries
- **Result Filtering**: Search results post-processed using Auth0 access control
- **Faceted Search**: Search facets limited by Auth0 user context

### Graph Databases (Neo4j, Neptune, Azure Digital Twins)
- **Node-Level Security**: Graph nodes accessible based on Auth0 user relationships
- **Edge Permissions**: Graph traversal restricted by Auth0 permission paths
- **Query Scoping**: Graph queries automatically limited to authorized subgraphs
- **Relationship Filtering**: Graph relationships filtered by Auth0 role hierarchies

## Usage

### Basic Usage

```typescript
import { db } from '@your-org/shared-db';

// Create a new user
const user = await db.prisma.user.create({
  data: {
    email: 'user@example.com',
    name: 'Test User',
  },
});

// Query users
const users = await db.prisma.user.findMany({
  where: { email: { contains: 'example' } },
});
```

### Using Transactions

```typescript
import { transaction } from '@your-org/shared-db';

const result = await transaction(async (tx) => {
  const user = await tx.user.create({
    data: { email: 'user@example.com' },
  });
  
  await tx.profile.create({
    data: { userId: user.id, bio: 'Hello, world!' },
  });
  
  return user;
});
```

### Pagination

```typescript
import { paginate } from '@your-org/shared-db';

const result = await paginate('user', {
  where: { active: true },
  page: 1,
  pageSize: 10,
  orderBy: { createdAt: 'desc' },
});

// Result includes pagination metadata
const { data, total, page, pageSize, totalPages } = result;
```

## Middleware

### Logging

```typescript
import { withLogging } from '@your-org/shared-db/middleware';
import { db } from '@your-org/shared-db';

db.prisma.$use(withLogging());
```

### Soft Delete

```typescript
import { withSoftDelete } from '@your-org/shared-db/middleware';
import { db } from '@your-org/shared-db';

db.prisma.$use(withSoftDelete());
```

## Seeding the Database

```typescript
import { seedDatabase } from '@your-org/shared-db/seed-utils';

// In your seed script
seedDatabase()
  .catch((e) => {
    console.error(e);
    process.exit(1);
  })
  .finally(async () => {
    await db.prisma.$disconnect();
  });
```

## Development

### Generating Prisma Client

```bash
npx prisma generate
```

### Running Migrations

```bash
# Create and apply migrations
npx prisma migrate dev --name init

# Apply pending migrations in production
npx prisma migrate deploy
```

### Prisma Studio

```bash
npx prisma studio
```

## Best Practices

1. **Use transactions** for operations that modify multiple records
2. **Always close connections** when your application shuts down
3. **Use connection pooling** in production
4. **Enable logging** in development for debugging
5. **Use environment variables** for sensitive configuration

## License

MIT
