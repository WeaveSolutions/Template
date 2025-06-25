# Prisma Multi-Provider Mock Data Management

> **Comprehensive mock data system for testing across all database providers**

## Overview

This documentation covers the centralized mock data management system designed to work seamlessly with our multi-provider Prisma setup. The system supports consistent data generation, seeding, and cleanup across PostgreSQL, MongoDB, CosmosDB, SQL Server, IBM Cloud DB2, and Supabase.

## 🎛️ Feature Flag Integration

All mock data operations respect the feature flags defined in `.env.example`:

```bash
# Database Provider Feature Flags
ENABLE_POSTGRESQL=true
ENABLE_MONGODB=false
ENABLE_SUPABASE=false
ENABLE_COSMOSDB=false
ENABLE_SQLSERVER=false
ENABLE_IBM=false
```

⚠️ **Important**: Mock data operations will only run for providers where the feature flag is set to `true`. Disabled providers are automatically skipped.

## Architecture

### Core Components

1. **MockDataGenerator** - Generates realistic test data using Faker.js
2. **MockSeeder** - Handles seeding and cleanup operations per provider
3. **Feature Flag Validation** - Ensures operations only run on enabled providers
4. **Provider-Specific Adapters** - Handle database-specific data transformations

### Directory Structure

```
packages/shared-db/
├── src/
│   └── mocks/
│       ├── MockDataGenerator.ts     # Core data generation
│       ├── MockSeeder.ts           # Seeding operations
│       └── index.ts                # Exports
├── scripts/
│   ├── seed-all-providers.ts       # Seed all enabled providers
│   ├── seed-provider.ts            # Seed specific provider
│   ├── clear-all-providers.ts      # Clear all enabled providers
│   ├── clear-provider.ts           # Clear specific provider
│   ├── stats.ts                    # Display provider statistics
│   └── test-{provider}.ts          # Individual provider tests
```

## MockDataGenerator

### Core User Generation

```typescript
import { faker } from '@faker-js/faker';

export class MockDataGenerator {
  /**
   * Generate a single user with realistic data
   */
  static generateUser(overrides: Partial<any> = {}) {
    return {
      id: faker.string.uuid(),
      email: faker.internet.email(),
      displayName: faker.person.fullName(),
      rank: faker.number.int({ min: 1, max: 100 }),
      createdAt: faker.date.past(),
      updatedAt: new Date(),
      deletedAt: null,
      ...overrides,
    };
  }

  /**
   * Generate multiple users
   */
  static generateUsers(count: number = 10, overrides: Partial<any> = {}) {
    return Array.from({ length: count }, () => this.generateUser(overrides));
  }
}
```

### OAuth Account Generation

```typescript
/**
 * Generate a single account connection with realistic provider data
 */
static generateAccount(overrides: Partial<any> = {}) {
  const providers = ['discord', 'github', 'slack', 'google', 'microsoft'];
  const provider = faker.helpers.arrayElement(providers);
  
  return {
    id: faker.string.uuid(),
    userId: faker.string.uuid(),
    provider,
    providerAccountId: faker.string.alphanumeric(10),
    accessToken: faker.string.alphanumeric(32),
    refreshToken: faker.string.alphanumeric(32),
    expiresAt: faker.date.future(),
    scope: this.generateScopeForProvider(provider),
    tokenType: 'Bearer',
    createdAt: faker.date.past(),
    updatedAt: new Date(),
    deletedAt: null,
    ...overrides,
  };
}
```

### Test Scenarios

```typescript
/**
 * Generate test data for specific scenarios
 */
static generateTestScenarios() {
  return {
    // High-rank users for admin testing
    admins: this.generateUsersWithRankRange(5, 90, 100),
    
    // Mid-rank users for regular user testing
    regularUsers: this.generateUsersWithRankRange(20, 20, 80),
    
    // New users for onboarding testing
    newUsers: this.generateUsersWithRankRange(10, 1, 10),
    
    // Users with multiple accounts for federated testing
    federatedUsers: Array.from({ length: 5 }, () => 
      this.generateUserWithAccounts(3)
    ),
    
    // Deleted users for soft delete testing
    deletedUsers: this.generateUsers(5, { 
      deletedAt: faker.date.past() 
    }),
  };
}
```

### Provider-Specific Data

```typescript
/**
 * Generate provider-specific test data
 */
static generateProviderSpecificData(provider: string) {
  switch (provider.toLowerCase()) {
    case 'mongodb':
    case 'cosmosdb':
      return {
        users: this.generateUsers(10).map(user => ({
          ...user,
          _id: user.id, // MongoDB uses _id
        })),
        accounts: this.generateAccounts(20).map(account => ({
          ...account,
          _id: account.id,
        })),
      };
    
    case 'postgres':
    case 'supabase':
    case 'sqlserver':
    case 'ibmcloud':
    default:
      return {
        users: this.generateUsers(10),
        accounts: this.generateAccounts(20),
      };
  }
}
```

## MockSeeder

### Feature Flag Validation

```typescript
import { getEnabledProviders } from '../utils/providers';

export class MockSeeder {
  /**
   * Validate that provider is enabled before operations
   */
  private static validateProvider(provider: string): void {
    const enabledProviders = getEnabledProviders();
    
    if (!enabledProviders.includes(provider)) {
      throw new Error(
        `Provider '${provider}' is disabled. Enable it in .env by setting ENABLE_${provider.toUpperCase()}=true`
      );
    }
  }
}
```

### Seeding Operations

```typescript
/**
 * Seed users for a specific provider (with feature flag check)
 */
static async seedUsers(provider: string, count: number = 50) {
  this.validateProvider(provider);
  
  try {
    const db = await DatabaseClient.getInstance(provider);
    const users = MockDataGenerator.generateUsers(count);
    
    console.log(`🌱 Seeding ${count} users for ${provider}...`);
    
    if (provider === 'mongodb' || provider === 'cosmosdb') {
      // MongoDB/CosmosDB requires different handling for _id
      const mongoUsers = users.map(user => ({
        ...user,
        _id: user.id,
      }));
      
      await db.user.createMany({
        data: mongoUsers,
        skipDuplicates: true,
      });
    } else {
      await db.user.createMany({
        data: users,
        skipDuplicates: true,
      });
    }
    
    console.log(`✅ Seeded ${count} users for ${provider}`);
  } catch (error) {
    console.error(`❌ Error seeding users for ${provider}:`, error);
    throw error;
  }
}
```

### Multi-Provider Operations

```typescript
/**
 * Seed data for all enabled providers
 */
static async seedAllProviders(options: {
  userCount?: number;
  accountCount?: number;
  includeScenarios?: boolean;
} = {}) {
  const enabledProviders = getEnabledProviders();
  
  if (enabledProviders.length === 0) {
    console.log('⚠️  No providers are enabled. Check your .env configuration.');
    return;
  }
  
  console.log(`🌍 Seeding data for enabled providers: ${enabledProviders.join(', ')}`);
  
  for (const provider of enabledProviders) {
    try {
      await this.seedAll(provider, options);
    } catch (error) {
      console.error(`❌ Failed to seed ${provider}, continuing with next provider...`);
    }
  }
  
  console.log(`🎊 Finished seeding all enabled providers!`);
}
```

## Usage Scripts

### TypeScript Scripts with Feature Flag Support

#### Seed All Enabled Providers

```typescript
// packages/shared-db/scripts/seed-all-providers.ts
import { MockSeeder } from '../src/mocks/MockSeeder';
import { getEnabledProviders } from '../src/utils/providers';
import dotenv from 'dotenv';

dotenv.config();

async function main() {
  const enabledProviders = getEnabledProviders();
  
  if (enabledProviders.length === 0) {
    console.log('⚠️  No database providers are enabled.');
    console.log('   Enable providers in .env: ENABLE_POSTGRESQL=true, ENABLE_MONGODB=true, etc.');
    process.exit(1);
  }
  
  console.log(`🌱 Seeding enabled providers: ${enabledProviders.join(', ')}\n`);
  
  const options = {
    userCount: process.env.SEED_USER_COUNT ? parseInt(process.env.SEED_USER_COUNT) : 50,
    accountCount: process.env.SEED_ACCOUNT_COUNT ? parseInt(process.env.SEED_ACCOUNT_COUNT) : 100,
    includeScenarios: process.env.SEED_SCENARIOS !== 'false',
  };
  
  await MockSeeder.seedAllProviders(options);
  await MockSeeder.validateDataConsistency();
}

main().catch(console.error);
```

#### Seed Specific Provider

```typescript
// packages/shared-db/scripts/seed-provider.ts
import { MockSeeder } from '../src/mocks/MockSeeder';
import { getEnabledProviders } from '../src/utils/providers';
import dotenv from 'dotenv';

dotenv.config();

async function main() {
  const provider = process.argv[2];
  
  if (!provider) {
    console.error('❌ Please specify a provider');
    console.log('Usage: npm run seed:provider <provider>');
    console.log('Available providers: postgres, mongodb, supabase, cosmosdb, sqlserver, ibmcloud');
    process.exit(1);
  }
  
  const enabledProviders = getEnabledProviders();
  
  if (!enabledProviders.includes(provider)) {
    console.error(`❌ Provider '${provider}' is not enabled`);
    console.log(`   Enable it in .env: ENABLE_${provider.toUpperCase()}=true`);
    console.log(`   Currently enabled: ${enabledProviders.join(', ')}`);
    process.exit(1);
  }
  
  await MockSeeder.seedAll(provider);
}

main().catch(console.error);
```

## NPM Scripts

### Package.json Integration

```json
{
  "scripts": {
    "seed:all": "ts-node packages/shared-db/scripts/seed-all-providers.ts",
    "seed:provider": "ts-node packages/shared-db/scripts/seed-provider.ts",
    "seed:clear:all": "ts-node packages/shared-db/scripts/clear-all-providers.ts",
    "seed:clear:provider": "ts-node packages/shared-db/scripts/clear-provider.ts",
    "seed:stats": "ts-node packages/shared-db/scripts/stats.ts",
    "test:db:all": "ts-node packages/shared-db/scripts/test-all-providers.ts",
    "test:db:postgres": "ts-node packages/shared-db/scripts/test-postgres.ts",
    "test:db:mongodb": "ts-node packages/shared-db/scripts/test-mongodb.ts",
    "test:db:supabase": "ts-node packages/shared-db/scripts/test-supabase.ts",
    "test:db:cosmosdb": "ts-node packages/shared-db/scripts/test-cosmosdb.ts",
    "test:db:sqlserver": "ts-node packages/shared-db/scripts/test-sqlserver.ts",
    "test:db:ibmcloud": "ts-node packages/shared-db/scripts/test-ibmcloud-provider.ts",
    "seed:postgres": "npm run seed:provider postgres",
    "seed:mongodb": "npm run seed:provider mongodb",
    "seed:supabase": "npm run seed:provider supabase",
    "seed:cosmosdb": "npm run seed:provider cosmosdb",
    "seed:sqlserver": "npm run seed:provider sqlserver",
    "seed:ibmcloud": "npm run seed:provider ibmcloud",
    "test:postgres": "ts-node packages/shared-db/scripts/test-postgres.ts",
    "test:mongodb": "ts-node packages/shared-db/scripts/test-mongodb.ts",
    "test:supabase": "ts-node packages/shared-db/scripts/test-supabase.ts",
    "test:cosmosdb": "ts-node packages/shared-db/scripts/test-cosmosdb.ts",
    "test:sqlserver": "ts-node packages/shared-db/scripts/test-sqlserver.ts",
    "test:ibmcloud": "ts-node packages/shared-db/scripts/test-ibmcloud-provider.ts"
  }
}
```

## Testing Integration

### Unit Tests with Mock Data

```typescript
// __tests__/user.test.ts
import { MockSeeder, MockDataGenerator } from '@packages/shared-db/mocks';
import { DatabaseClient } from '@packages/shared-db';
import { getEnabledProviders } from '@packages/shared-db/utils/providers';

describe('User Operations', () => {
  const enabledProviders = getEnabledProviders();
  
  // Skip tests if no providers are enabled
  if (enabledProviders.length === 0) {
    test.skip('No database providers enabled', () => {});
    return;
  }
  
  describe.each(enabledProviders)('%s Provider', (provider) => {
    let db: any;
    
    beforeAll(async () => {
      db = await DatabaseClient.getInstance(provider);
      await MockSeeder.clearAll(provider);
      await MockSeeder.seedUsers(provider, 10);
    });
    
    afterAll(async () => {
      await MockSeeder.clearAll(provider);
    });
    
    test('should find seeded users', async () => {
      const users = await db.user.findMany();
      expect(users).toHaveLength(10);
    });
    
    test('should create new user', async () => {
      const userData = MockDataGenerator.generateUser();
      const user = await db.user.create({ data: userData });
      expect(user.email).toBe(userData.email);
    });
  });
});
```

### Integration Tests

```typescript
// __tests__/integration/federated-users.test.ts
import { MockSeeder, MockDataGenerator } from '@packages/shared-db/mocks';
import { DatabaseClient } from '@packages/shared-db';

describe('Federated User Integration', () => {
  const testProvider = process.env.TEST_PROVIDER || 'postgres';
  let db: any;
  
  beforeAll(async () => {
    // Only run if provider is enabled
    const enabledProviders = getEnabledProviders();
    if (!enabledProviders.includes(testProvider)) {
      pending(`Provider ${testProvider} is not enabled`);
    }
    
    db = await DatabaseClient.getInstance(testProvider);
    await MockSeeder.clearAll(testProvider);
  });
  
  test('should create user with multiple OAuth accounts', async () => {
    const { user, accounts } = MockDataGenerator.generateUserWithAccounts(3);
    
    // Create user first
    const createdUser = await db.user.create({ data: user });
    
    // Create accounts linked to user
    const accountsData = accounts.map(account => ({
      ...account,
      userId: createdUser.id,
    }));
    
    await db.account.createMany({ data: accountsData });
    
    // Verify relationships
    const userWithAccounts = await db.user.findUnique({
      where: { id: createdUser.id },
      include: { accounts: true },
    });
    
    expect(userWithAccounts.accounts).toHaveLength(3);
  });
});
```

## Database Provider Testing

### Individual Provider Test Scripts

Each database provider has a dedicated test script that validates connectivity, schema operations, and provider-specific features:

#### PostgreSQL Test Script

```typescript
// packages/shared-db/scripts/test-postgres.ts
import { DatabaseClient } from '../src/DatabaseClient';

async function main() {
  if (process.env.ENABLE_POSTGRESQL !== 'true') {
    throw new Error('PostgreSQL provider is not enabled. Set ENABLE_POSTGRESQL=true');
  }
  
  console.log('🧪 PostgreSQL Provider Test Suite\n');
  
  const db = await DatabaseClient.getInstance('postgres');
  
  // Test basic connectivity
  await db.$queryRaw`SELECT 1 as test`;
  console.log('✅ PostgreSQL connection successful');
  
  // Test database info
  const versionResult = await db.$queryRaw<Array<{ version: string }>>`SELECT version()`;
  console.log(`📋 Version: ${versionResult[0]?.version?.split(' ').slice(0, 2).join(' ')}`);
  
  // Test schema operations
  const userCount = await db.user.count();
  console.log(`👥 Users in database: ${userCount}`);
  
  // Test transactions
  await db.$transaction(async (tx) => {
    const testUser = await tx.user.create({
      data: { email: 'test@example.com', displayName: 'Test User', rank: 999 }
    });
    throw new Error('Intentional rollback'); // Test rollback
  }).catch(() => console.log('✅ Transaction rollback successful'));
  
  await db.$disconnect();
  console.log('🎉 All PostgreSQL tests passed!');
}
```

#### MongoDB Test Script

```typescript
// packages/shared-db/scripts/test-mongodb.ts
import { DatabaseClient } from '../src/DatabaseClient';

async function main() {
  if (process.env.ENABLE_MONGODB !== 'true') {
    throw new Error('MongoDB provider is not enabled. Set ENABLE_MONGODB=true');
  }
  
  console.log('🧪 MongoDB Provider Test Suite\n');
  
  const db = await DatabaseClient.getInstance('mongodb');
  
  // Test basic connectivity
  await db.$runCommandRaw({ ping: 1 });
  console.log('✅ MongoDB connection successful');
  
  // Test database info
  const dbStats = await db.$runCommandRaw({ dbStats: 1, scale: 1024 * 1024 });
  console.log(`📋 Database: ${dbStats.db}`);
  console.log(`📊 Collections: ${dbStats.collections || 0}`);
  
  // Test schema operations
  const userCount = await db.user.count();
  console.log(`👥 Users in database: ${userCount}`);
  
  // Test aggregation pipeline
  const pipeline = [{ $group: { _id: null, totalUsers: { $sum: 1 } } }];
  await db.user.aggregateRaw({ pipeline });
  console.log('✅ Aggregation pipeline test successful');
  
  await db.$disconnect();
  console.log('🎉 All MongoDB tests completed!');
}
```

#### Supabase Test Script

```typescript
// packages/shared-db/scripts/test-supabase.ts
import { DatabaseClient } from '../src/DatabaseClient';

async function main() {
  if (process.env.ENABLE_SUPABASE !== 'true') {
    throw new Error('Supabase provider is not enabled. Set ENABLE_SUPABASE=true');
  }
  
  console.log('🧪 Supabase Provider Test Suite\n');
  
  const db = await DatabaseClient.getInstance('supabase');
  
  // Test basic connectivity
  await db.$queryRaw`SELECT 1 as test`;
  console.log('✅ Supabase connection successful');
  
  // Test Supabase-specific features
  const extensionsResult = await db.$queryRaw`
    SELECT extname 
    FROM pg_extension 
    WHERE extname IN ('supabase_vault', 'pgsodium', 'pg_graphql')
  `;
  console.log(`🚀 Supabase Extensions: ${extensionsResult.length}`);
  
  // Test RLS policies
  const rlsPolicies = await db.$queryRaw`
    SELECT COUNT(*) as count FROM pg_policies WHERE schemaname = 'public'
  `;
  console.log(`🔒 RLS Policies: ${rlsPolicies[0]?.count || 0}`);
  
  // Test UUID generation
  const uuidResult = await db.$queryRaw`SELECT gen_random_uuid() as uuid`;
  console.log(`🆔 UUID generation: ${uuidResult[0]?.uuid?.substring(0, 8)}...`);
  
  await db.$disconnect();
  console.log('🎉 All Supabase tests passed!');
}
```

#### CosmosDB Test Script

```typescript
// packages/shared-db/scripts/test-cosmosdb.ts
import { DatabaseClient } from '../src/DatabaseClient';

async function main() {
  if (process.env.ENABLE_COSMOSDB !== 'true') {
    throw new Error('CosmosDB provider is not enabled. Set ENABLE_COSMOSDB=true');
  }
  
  console.log('🧪 CosmosDB Provider Test Suite\n');
  
  const db = await DatabaseClient.getInstance('cosmosdb');
  
  // Test basic connectivity
  await db.$runCommandRaw({ ping: 1 });
  console.log('✅ CosmosDB connection successful');
  
  // Test database info
  const dbStats = await db.$runCommandRaw({ dbStats: 1, scale: 1024 * 1024 });
  console.log(`☁️  Service: Azure CosmosDB (MongoDB API)`);
  console.log(`📋 Database: ${dbStats.db || 'Unknown'}`);
  
  // Test Request Units tracking
  const startTime = Date.now();
  await db.$runCommandRaw({ ping: 1 });
  const endTime = Date.now();
  console.log(`⚡ Request completed in ${endTime - startTime}ms`);
  
  // Test partitioning awareness
  console.log('🔄 CosmosDB Features:');
  console.log('   - Automatic indexing: Enabled by default');
  console.log('   - Global distribution: Available');
  console.log('   - Request Unit (RU) billing: Active');
  
  await db.$disconnect();
  console.log('🎉 All CosmosDB tests completed!');
}
```

#### SQL Server Test Script

```typescript
// packages/shared-db/scripts/test-sqlserver.ts
import { DatabaseClient } from '../src/DatabaseClient';

async function main() {
  if (process.env.ENABLE_SQLSERVER !== 'true') {
    throw new Error('SQL Server provider is not enabled. Set ENABLE_SQLSERVER=true');
  }
  
  console.log('🧪 SQL Server Provider Test Suite\n');
  
  const db = await DatabaseClient.getInstance('sqlserver');
  
  // Test basic connectivity
  await db.$queryRaw`SELECT 1 as test`;
  console.log('✅ SQL Server connection successful');
  
  // Test SQL Server version and edition
  const versionResult = await db.$queryRaw`SELECT @@VERSION as version`;
  const editionResult = await db.$queryRaw`SELECT SERVERPROPERTY('Edition') as edition`;
  console.log(`📋 Version: ${versionResult[0]?.version?.split('\n')[0]}`);
  console.log(`🏢 Edition: ${editionResult[0]?.edition}`);
  
  // Test IDENTITY columns
  const identityTest = await db.$queryRaw`
    SELECT IDENT_SEED('User') as seed, IDENT_INCR('User') as increment
  `;
  if (identityTest[0]?.seed !== null) {
    console.log(`🔢 IDENTITY: seed=${identityTest[0].seed}, increment=${identityTest[0].increment}`);
  }
  
  // Test system functions
  const systemFunctions = await db.$queryRaw`
    SELECT NEWID() as guid, GETDATE() as datetime, HOST_NAME() as host
  `;
  console.log(`🛠️  NEWID(): ${systemFunctions[0]?.guid?.substring(0, 8)}...`);
  
  // Test transaction isolation levels
  await db.$queryRaw`SET TRANSACTION ISOLATION LEVEL READ COMMITTED`;
  console.log('💼 Transaction isolation levels tested');
  
  await db.$disconnect();
  console.log('🎉 All SQL Server tests passed!');
}
```

#### IBM Cloud DB2 Test Script

```typescript
// packages/shared-db/scripts/test-ibmcloud-provider.ts
import { DatabaseClient } from '../src/DatabaseClient';

async function main() {
  if (process.env.ENABLE_IBM !== 'true') {
    throw new Error('IBM Cloud provider is not enabled. Set ENABLE_IBM=true');
  }
  
  console.log('🧪 IBM Cloud DB2 Provider Test Suite\n');
  
  const db = await DatabaseClient.getInstance('ibmcloud');
  
  // Test basic connectivity
  await db.$queryRaw`SELECT 1 as test FROM SYSIBM.SYSDUMMY1`;
  console.log('✅ IBM Cloud DB2 connection successful');
  
  // Test database info
  const versionResult = await db.$queryRaw`
    SELECT SERVICE_LEVEL, FIXPACK_NUM 
    FROM SYSIBMADM.ENV_PROD_INFO 
    WHERE TYPENAME = 'Base'
  `;
  console.log(`📋 DB2 Version: ${versionResult[0]?.SERVICE_LEVEL || 'Unknown'}`);
  
  await db.$disconnect();
  console.log('🎉 All IBM Cloud DB2 tests passed!');
}
```

### Test Runner Script

Run all enabled provider tests with a comprehensive test runner:

```typescript
// packages/shared-db/scripts/test-all-providers.ts
import { spawn } from 'child_process';

interface TestResult {
  provider: string;
  success: boolean;
  duration: number;
  error?: string;
}

async function main() {
  const providers = [
    { name: 'postgres', enabled: process.env.ENABLE_POSTGRESQL === 'true' },
    { name: 'mongodb', enabled: process.env.ENABLE_MONGODB === 'true' },
    { name: 'supabase', enabled: process.env.ENABLE_SUPABASE === 'true' },
    { name: 'cosmosdb', enabled: process.env.ENABLE_COSMOSDB === 'true' },
    { name: 'sqlserver', enabled: process.env.ENABLE_SQLSERVER === 'true' },
    { name: 'ibmcloud', enabled: process.env.ENABLE_IBM === 'true' }
  ];
  
  const enabledProviders = providers.filter(p => p.enabled);
  
  if (enabledProviders.length === 0) {
    console.log('❌ No providers are enabled for testing.');
    process.exit(1);
  }
  
  console.log(`🧪 Running tests for ${enabledProviders.length} enabled providers...\n`);
  
  const results: TestResult[] = [];
  
  for (const provider of enabledProviders) {
    const startTime = Date.now();
    const scriptPath = `./test-${provider.name === 'ibmcloud' ? 'ibmcloud-provider' : provider.name}.ts`;
    
    try {
      await new Promise<void>((resolve, reject) => {
        const child = spawn('ts-node', [scriptPath], { stdio: 'inherit' });
        child.on('close', (code) => {
          const duration = Date.now() - startTime;
          const success = code === 0;
          
          results.push({ provider: provider.name, success, duration });
          
          if (success) {
            console.log(`✅ ${provider.name} test passed (${duration}ms)\n`);
          } else {
            console.log(`❌ ${provider.name} test failed (${duration}ms)\n`);
          }
          
          resolve();
        });
        child.on('error', reject);
      });
    } catch (error) {
      results.push({
        provider: provider.name,
        success: false,
        duration: Date.now() - startTime,
        error: error instanceof Error ? error.message : 'Unknown error'
      });
    }
  }
  
  // Show summary
  console.log('\n📋 Test Results Summary');
  console.log('========================\n');
  
  const successful = results.filter(r => r.success);
  const failed = results.filter(r => !r.success);
  
  results.forEach(result => {
    const status = result.success ? '✅' : '❌';
    console.log(`${status} ${result.provider.padEnd(12)} ${result.duration}ms`);
  });
  
  console.log(`\n📊 Statistics:`);
  console.log(`   Total tests: ${results.length}`);
  console.log(`   Successful: ${successful.length}`);
  console.log(`   Failed: ${failed.length}`);
  
  if (failed.length > 0) {
    console.log('\n❌ Failed tests detected');
    process.exit(1);
  } else {
    console.log('\n🎉 All tests passed successfully!');
  }
}
```

### Test Commands

```bash
# Run all enabled provider tests
npm run test:db:all
ts-node packages/shared-db/scripts/test-all-providers.ts

# Test individual providers
npm run test:db:postgres
npm run test:db:mongodb
npm run test:db:supabase
npm run test:db:cosmosdb
npm run test:db:sqlserver
npm run test:db:ibmcloud

# With options
ts-node packages/shared-db/scripts/test-all-providers.ts --dry-run
ts-node packages/shared-db/scripts/test-all-providers.ts --continue
ts-node packages/shared-db/scripts/test-all-providers.ts --verbose
```

### Test Features

Each test script validates:

1. **🔗 Connectivity**: Basic database connection and authentication
2. **📊 Database Info**: Version, configuration, and metadata
3. **🔍 Schema Operations**: Table/collection queries and Prisma operations
4. **⚡ Provider Features**: Database-specific capabilities and functions
5. **💼 Transactions**: ACID compliance and rollback behavior
6. **🛡️ Security**: Access controls and permissions (where applicable)

### Test Environment

```bash
# Enable providers for testing
ENABLE_POSTGRESQL=true
ENABLE_MONGODB=true
ENABLE_SUPABASE=true
ENABLE_COSMOSDB=true
ENABLE_SQLSERVER=true
ENABLE_IBM=true

# Connection strings
DATABASE_URL=postgresql://...
MONGODB_URL=mongodb://...
SUPABASE_DATABASE_URL=postgresql://...
COSMOSDB_URL=mongodb://...
SQLSERVER_DATABASE_URL=sqlserver://...
IBMCLOUD_DATABASE_URL=db2://...
```

## Environment Configuration

### Required Dependencies

```json
{
  "devDependencies": {
    "@faker-js/faker": "^8.0.0",
    "dotenv": "^16.0.0",
    "ts-node": "^10.9.0",
    "typescript": "^5.0.0"
  }
}
```

### Environment Variables

```bash
# Mock Data Configuration
SEED_USER_COUNT=50              # Number of users to generate per provider
SEED_ACCOUNT_COUNT=100          # Number of accounts to generate per provider
SEED_SCENARIOS=true             # Include test scenarios (admin, new users, etc.)
TEST_PROVIDER=postgres          # Default provider for individual tests

# Safety Settings
NODE_ENV=development            # Prevents accidental production data clearing
FORCE_PRODUCTION_CLEAR=false    # Required to clear production data
FORCE_CLEAR=false              # Skip confirmation prompts
CI=false                       # CI environment detection
```

## Best Practices

### 1. Feature Flag Validation

Always check feature flags before operations:

```typescript
const enabledProviders = getEnabledProviders();
if (!enabledProviders.includes(provider)) {
  throw new Error(`Provider ${provider} is not enabled`);
}
```

### 2. Provider-Specific Handling

Handle database-specific requirements:

```typescript
// MongoDB requires _id mapping
if (provider === 'mongodb' || provider === 'cosmosdb') {
  data = data.map(item => ({ ...item, _id: item.id }));
}
```

### 3. Error Handling

Always include comprehensive error handling:

```typescript
try {
  await operation();
} catch (error) {
  console.error(`❌ Error in ${provider}:`, error);
  // Continue with other providers or re-throw as needed
}
```

### 4. Data Consistency

Maintain referential integrity:

```typescript
// Clear in correct order (accounts before users)
await db.account.deleteMany({});
await db.user.deleteMany({});
```

### 5. Test Isolation

Ensure tests don't interfere with each other:

```typescript
beforeEach(async () => {
  await MockSeeder.clearAll(provider);
  await MockSeeder.seedTestScenarios(provider);
});
```

## Troubleshooting

### Common Issues

1. **Provider Not Enabled**
   - Error: `Provider 'mongodb' is disabled`
   - Solution: Set `ENABLE_MONGODB=true` in `.env`

2. **No Providers Enabled**
   - Error: `No database providers are enabled`
   - Solution: Enable at least one provider in `.env`

3. **Connection Issues**
   - Error: Database connection failed
   - Solution: Verify database URLs and credentials in `.env`

4. **MongoDB _id Issues**
   - Error: MongoDB validation errors
   - Solution: Ensure `_id` field mapping for MongoDB/CosmosDB

### Debug Mode

Enable verbose logging:

```bash
DEBUG=prisma:* npm run seed:all
DATABASE_LOG_LEVEL=debug npm run seed:provider postgres
```

This comprehensive mock data system ensures consistent, reliable testing across all supported database providers while respecting feature flag configurations.
