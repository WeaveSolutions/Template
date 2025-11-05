# Database Providers

This package supports multiple database providers with Prisma, each with its own schema and configuration. The multi-database architecture enables flexible deployment across cloud providers with Auth0-based authorization.

## Available Providers

### Relational Databases

1. **PostgreSQL** (`postgres.prisma`)
   - Uses `PostgresUser`, `PostgresPost`, `PostgresRole` models
   - Environment variable: `POSTGRES_PRISMA_URL`
   - Feature flag: `ENABLE_POSTGRES`
   - Best for: Primary production database, complex queries

2. **Supabase** (`supabase.prisma`)
   - Uses `SupabaseUser`, `SupabasePost`, `SupabaseRole` models
   - Environment variables: `SUPABASE_DATABASE_URL`, `SUPABASE_DIRECT_URL`
   - Feature flag: `ENABLE_SUPABASE`
   - Best for: Rapid development with built-in auth and realtime features
   - Auth0 Integration: RLS policies validate JWT claims

3. **SQL Server** (`sqlserver.prisma`)
   - Uses `SqlServerUser`, `SqlServerPost`, `SqlServerRole` models
   - Environment variable: `SQLSERVER_URL`
   - Feature flag: `ENABLE_SQLSERVER`
   - Best for: Enterprise Microsoft environments

4. **IBM Cloud DB2** (`ibmcloud.prisma`)
   - Uses `IbmcloudUser`, `IbmcloudPost`, `IbmcloudRole` models
   - Environment variables: `DB2_URL`, `IBM_APIKEY`, `IBM_RESOURCE_INSTANCE_ID`
   - Feature flag: `ENABLE_DB2`
   - Best for: Enterprise IBM workloads
   - Note: Uses PostgreSQL connector for Prisma compatibility

### NoSQL Databases

5. **MongoDB** (`mongodb.prisma`)
   - Uses `MongodbUser`, `MongodbPost`, `MongodbRole` models
   - Environment variable: `MONGODB_URI`
   - Feature flag: `ENABLE_MONGODB`
   - Best for: Document-based applications, flexible schemas

6. **CosmosDB** (`cosmosdb.prisma`)
   - Uses `CosmosUser`, `CosmosPost`, `CosmosRole` models
   - Environment variable: `COSMOSDB_MONGODB_URI`
   - Feature flag: `ENABLE_COSMOSDB`
   - Best for: Azure multi-region deployments

7. **Firebase Firestore** (External Integration)
   - Environment variables: `FIREBASE_PROJECT_ID`, `FIREBASE_CLIENT_EMAIL`, `FIREBASE_PRIVATE_KEY`
   - Feature flag: `ENABLE_FIREBASE`
   - Best for: Real-time data sync, mobile applications
   - Auth0 Integration: Custom token exchange via CRA service
   - Note: Not a Prisma provider - uses Firebase SDK directly

8. **MongoDB Atlas** (Managed MongoDB)
   - Environment variable: `MONGODB_URI` (same as MongoDB)
   - Feature flag: `ENABLE_MONGODB_ATLAS`
   - Best for: Fully managed MongoDB with global clusters
   - Features: Auto-scaling, backup, search, charts, realm sync
   - Auth0 Integration: Network peering, IP whitelisting

### Identity & Authentication Databases

9. **Auth0 Database** (`auth0.prisma`)
   - Uses `Auth0User`, `Auth0AppMetadata`, `Auth0UserMetadata`, `Auth0ConnectedProvider` models
   - Environment variables: `AUTH0_DOMAIN`, `AUTH0_CLIENT_ID`, `AUTH0_CLIENT_SECRET`, `AUTH0_AUDIENCE`
   - Feature flag: `ENABLE_AUTH0_DATABASE`
   - Best for: Central identity management, federated authentication, CRA Account Center
   - Features: Linked list optimization, 10-year audit retention, Vault integration, schema integrity verification
   - Auth0 Integration: Native - uses Auth0 Management API as data source

### AI-Powered Databases

10. **MindsDB** (External Integration)
   - Environment variables: `MINDSDB_URL`, `MINDSDB_API_KEY`, `MINDSDB_USERNAME`, `MINDSDB_PASSWORD`
   - Feature flag: `ENABLE_MINDSDB`
   - Best for: AI-powered analytics, natural language queries
   - Auth0 Integration: User context in predictions, permission-based model access

## Auth0 Integration

All database providers integrate with Auth0 for authentication and authorization:

```javascript
// JWT claims from Auth0 used for database access control
{
  "sub": "auth0|user123",
  "permissions": ["read:database", "write:database"],
  "app_metadata": {
    "tenant_id": "tenant_789",
    "database_access": {
      "postgres": ["db1", "db2"],
      "mongodb": ["analytics"]
    }
  }
}
```

## Multi-Tenant Configuration

Control multi-tenant database access through environment variables:

```env
# Multi-Tenant Configuration
ENABLE_MULTI_TENANT=true
TENANT_ISOLATION_LEVEL=strict
DATABASE_PER_TENANT=false
SCHEMA_PER_TENANT=true
ROW_LEVEL_SECURITY=false
```

## Setup

1. Install dependencies:
   ```bash
   pnpm install
   ```

2. Set the appropriate environment variables in your `.env` file:
   ```env
   # Default provider
   DEFAULT_DATABASE_PROVIDER=postgres
   
   # Enable providers
   ENABLE_POSTGRES=true
   ENABLE_MONGODB=true
   ENABLE_SUPABASE=false
   ENABLE_COSMOSDB=false
   ENABLE_SQLSERVER=false
   ENABLE_DB2=false
   ENABLE_MINDSDB=true
   ENABLE_FIREBASE=false
   ENABLE_MONGODB_ATLAS=false
   ENABLE_AUTH0_DATABASE=true
   
   # PostgreSQL
   POSTGRES_PRISMA_URL="postgresql://user:password@localhost:5432/mydb?schema=public"
   
   # MongoDB
   MONGODB_URI="mongodb://user:password@localhost:27017/mydb?authSource=admin"
   
   # IBM Cloud DB2
   DB2_URL="postgresql://user:password@db2.ibmcloud.com:5432/mydb"
   IBM_APIKEY="your-ibm-cloud-apikey"
   
   # Auth0 Database
   AUTH0_DOMAIN="your-auth0-domain.com"
   AUTH0_CLIENT_ID="your-auth0-client-id"
   AUTH0_CLIENT_SECRET="your-auth0-client-secret"
   AUTH0_AUDIENCE="https://your-auth0-audience.com/api/v2/"
   ```

## Usage

### Generate Prisma Client

```bash
# For a specific provider
pnpm run db:generate -- --provider=postgres

# For all enabled providers
pnpm run db:generate
```

### Run Migrations

```bash
# Create and apply a new migration
pnpm run db:migrate -- --provider=postgres --name=init

# Reset the database (DANGER: This will drop all data)
pnpm run db:reset -- --provider=postgres
```

### Open Prisma Studio

```bash
pnpm run db:studio -- --provider=postgres
```

### List Available Providers

```bash
pnpm run db:list
```

## Environment Variables

### Database Connection Strings
- `POSTGRES_PRISMA_URL`: PostgreSQL connection string (pooled)
- `POSTGRES_URL_NON_POOLING`: PostgreSQL direct connection
- `MONGODB_URI`: MongoDB connection string
- `SUPABASE_DATABASE_URL`: Supabase PostgreSQL connection (pooled)
- `SUPABASE_DIRECT_URL`: Supabase direct connection
- `COSMOSDB_MONGODB_URI`: CosmosDB MongoDB API connection
- `SQLSERVER_URL`: SQL Server connection string
- `DB2_URL`: IBM DB2 connection string (PostgreSQL-compatible)
- `FIREBASE_PROJECT_ID`: Firebase project ID
- `FIREBASE_CLIENT_EMAIL`: Firebase client email
- `FIREBASE_PRIVATE_KEY`: Firebase private key
- `AUTH0_DOMAIN`: Auth0 domain
- `AUTH0_CLIENT_ID`: Auth0 client ID
- `AUTH0_CLIENT_SECRET`: Auth0 client secret
- `AUTH0_AUDIENCE`: Auth0 audience

### Feature Flags
- `ENABLE_POSTGRES`: Enable PostgreSQL provider
- `ENABLE_MONGODB`: Enable MongoDB provider
- `ENABLE_SUPABASE`: Enable Supabase provider
- `ENABLE_COSMOSDB`: Enable CosmosDB provider
- `ENABLE_SQLSERVER`: Enable SQL Server provider
- `ENABLE_DB2`: Enable IBM DB2 provider
- `ENABLE_MINDSDB`: Enable MindsDB integration
- `ENABLE_FIREBASE`: Enable Firebase Firestore integration
- `ENABLE_MONGODB_ATLAS`: Enable MongoDB Atlas integration
- `ENABLE_AUTH0_DATABASE`: Enable Auth0 Database provider

### Database Configuration
- `DEFAULT_DATABASE_PROVIDER`: Default provider when multiple are enabled
- `DATABASE_LOG_LEVEL`: Logging level (error, warn, info, debug)
- `PRISMA_LOGGING`: Enable Prisma query logging

## Model Naming Conventions

Each provider's models are prefixed with the provider name to prevent naming conflicts:

- `PostgresUser`, `PostgresPost`, `PostgresRole`
- `MongodbUser`, `MongodbPost`, `MongodbRole`
- `SupabaseUser`, `SupabasePost`, `SupabaseRole`
- `CosmosUser`, `CosmosPost`, `CosmosRole`
- `SqlServerUser`, `SqlServerPost`, `SqlServerRole`
- `IbmcloudUser`, `IbmcloudPost`, `IbmcloudRole`
- `Auth0User`, `Auth0AppMetadata`, `Auth0UserMetadata`, `Auth0ConnectedProvider`

## Best Practices

1. **Security**: Always use environment variables for connection strings
2. **Feature Flags**: Enable only the providers you need
3. **Multi-Tenant**: Configure appropriate isolation level for your use case
4. **Auth0 Integration**: Ensure JWT validation is configured at Kong Gateway
5. **Migrations**: Always backup data before running migrations
6. **Connection Pooling**: Use pooled connections for production
7. **Monitoring**: Enable database logging for troubleshooting

## Cloud Provider Support

| Provider | AWS | GCP | Azure | OCI | IBM | DigitalOcean |
|----------|-----|-----|-------|-----|-----|--------------|
| PostgreSQL | RDS | Cloud SQL | Azure Database | OCI Database | Databases for PostgreSQL | Managed Databases |
| MongoDB | DocumentDB | Firestore | CosmosDB | - | Cloudant | - |
| SQL Server | RDS | Cloud SQL | Azure SQL | - | - | - |
| DB2 | - | - | - | - | Db2 on Cloud | - |

## Additional Resources

- [Prisma Documentation](https://www.prisma.io/docs)
- [Auth0 Database Connections](https://auth0.com/docs/connections/database)
- [MindsDB Documentation](https://docs.mindsdb.com)
- [Multi-Tenant Architecture Guide](../../../docs/Architecture/Multi-Tenant.md)
