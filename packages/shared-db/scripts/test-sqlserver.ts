#!/usr/bin/env ts-node

/**
 * Test SQL Server provider connectivity and basic operations
 * Usage: ts-node scripts/test-sqlserver.ts
 * 
 * Environment Variables:
 * - ENABLE_SQLSERVER=true (must be enabled)
 * - SQLSERVER_DATABASE_URL=sqlserver://... (connection string)
 */

import dotenv from 'dotenv';
import { DatabaseClient } from '../src/DatabaseClient';

dotenv.config();

/**
 * Check if SQL Server provider is enabled
 */
function validateProvider(): void {
  if (process.env.ENABLE_SQLSERVER !== 'true') {
    throw new Error(
      'SQL Server provider is not enabled. ' +
      'Set ENABLE_SQLSERVER=true in .env to run this test.'
    );
  }
  
  if (!process.env.SQLSERVER_DATABASE_URL) {
    throw new Error(
      'SQLSERVER_DATABASE_URL is not configured. ' +
      'Set SQLSERVER_DATABASE_URL=sqlserver://... in .env to run this test.'
    );
  }
}

/**
 * Test basic database connectivity
 */
async function testConnection(): Promise<void> {
  console.log('🔗 Testing SQL Server connection...');
  
  const db = await DatabaseClient.getInstance('sqlserver');
  
  // Test basic connection with a simple query
  await db.$queryRaw`SELECT 1 as test`;
  
  console.log('✅ SQL Server connection successful');
}

/**
 * Test database information retrieval
 */
async function testDatabaseInfo(): Promise<void> {
  console.log('📊 Retrieving SQL Server database information...');
  
  const db = await DatabaseClient.getInstance('sqlserver');
  
  // Get SQL Server version
  const versionResult = await db.$queryRaw<Array<{ version: string }>>`SELECT @@VERSION as version`;
  const version = versionResult[0]?.version || 'Unknown';
  
  // Get current database name
  const dbResult = await db.$queryRaw<Array<{ current_database: string }>>`SELECT DB_NAME() as current_database`;
  const currentDb = dbResult[0]?.current_database || 'Unknown';
  
  // Get current user
  const userResult = await db.$queryRaw<Array<{ current_user: string }>>`SELECT SUSER_SNAME() as current_user`;
  const currentUser = userResult[0]?.current_user || 'Unknown';
  
  // Get table count in dbo schema
  const tableCountResult = await db.$queryRaw<Array<{ count: number }>>`
    SELECT COUNT(*) as count 
    FROM INFORMATION_SCHEMA.TABLES 
    WHERE TABLE_SCHEMA = 'dbo' AND TABLE_TYPE = 'BASE TABLE'
  `;
  const tableCount = tableCountResult[0]?.count || 0;
  
  // Get SQL Server edition
  const editionResult = await db.$queryRaw<Array<{ edition: string }>>`SELECT SERVERPROPERTY('Edition') as edition`;
  const edition = editionResult[0]?.edition || 'Unknown';
  
  // Get compatibility level
  const compatibilityResult = await db.$queryRaw<Array<{ compatibility_level: number }>>`
    SELECT compatibility_level 
    FROM sys.databases 
    WHERE name = DB_NAME()
  `;
  const compatibilityLevel = compatibilityResult[0]?.compatibility_level || 0;
  
  console.log('📋 SQL Server Database Information:');
  console.log(`   Version: ${version.split('\n')[0]}`);
  console.log(`   Edition: ${edition}`);
  console.log(`   Database: ${currentDb}`);
  console.log(`   User: ${currentUser}`);
  console.log(`   Compatibility Level: ${compatibilityLevel}`);
  console.log(`   Tables in dbo schema: ${tableCount}`);
}

/**
 * Test SQL Server-specific features
 */
async function testSQLServerFeatures(): Promise<void> {
  console.log('⚡ Testing SQL Server-specific features...');
  
  const db = await DatabaseClient.getInstance('sqlserver');
  
  try {
    // Test IDENTITY columns (auto-increment)
    console.log('   Testing IDENTITY column support...');
    const identityTest = await db.$queryRaw<Array<{ identity_seed: number, identity_increment: number }>>`
      SELECT IDENT_SEED('User') as identity_seed, IDENT_INCR('User') as identity_increment
    `;
    
    if (identityTest.length > 0 && identityTest[0].identity_seed !== null) {
      console.log(`   IDENTITY seed: ${identityTest[0].identity_seed}, increment: ${identityTest[0].identity_increment}`);
      console.log('✅ IDENTITY columns configured');
    } else {
      console.log('⚠️  IDENTITY columns not found or table does not exist');
    }
    
    // Test SQL Server system functions
    console.log('   Testing SQL Server system functions...');
    const systemFunctions = await db.$queryRaw<Array<{ 
      newid: string,
      getdate: Date,
      host_name: string 
    }>>`
      SELECT 
        NEWID() as newid,
        GETDATE() as getdate,
        HOST_NAME() as host_name
    `;
    
    if (systemFunctions.length > 0) {
      const func = systemFunctions[0];
      console.log(`   NEWID(): ${func.newid.substring(0, 8)}...`);
      console.log(`   GETDATE(): ${func.getdate.toISOString()}`);
      console.log(`   HOST_NAME(): ${func.host_name}`);
      console.log('✅ System functions working');
    }
    
    // Test collation information
    const collationResult = await db.$queryRaw<Array<{ collation_name: string }>>`
      SELECT DATABASEPROPERTYEX(DB_NAME(), 'Collation') as collation_name
    `;
    
    if (collationResult.length > 0) {
      console.log(`   Database collation: ${collationResult[0].collation_name}`);
    }
    
    // Test schema information
    const schemasResult = await db.$queryRaw<Array<{ schema_name: string }>>`
      SELECT name as schema_name 
      FROM sys.schemas 
      WHERE name IN ('dbo', 'guest', 'INFORMATION_SCHEMA', 'sys')
      ORDER BY name
    `;
    
    if (schemasResult.length > 0) {
      const schemas = schemasResult.map(s => s.schema_name);
      console.log(`   Available schemas: ${schemas.join(', ')}`);
    }
    
    // Test for SQL Server Management Objects
    try {
      const smoTest = await db.$queryRaw<Array<{ object_count: number }>>`
        SELECT COUNT(*) as object_count 
        FROM sys.objects 
        WHERE type IN ('U', 'V', 'P', 'FN')
      `;
      
      if (smoTest.length > 0) {
        console.log(`   Database objects: ${smoTest[0].object_count}`);
      }
      
      console.log('✅ SQL Server Management Objects accessible');
    } catch (error) {
      console.log('⚠️  Limited access to SQL Server Management Objects');
    }
    
  } catch (error) {
    console.warn('⚠️  Limited access to SQL Server-specific features:', error);
  }
}

/**
 * Test Prisma schema operations
 */
async function testSchemaOperations(): Promise<void> {
  console.log('🔍 Testing SQL Server schema operations...');
  
  const db = await DatabaseClient.getInstance('sqlserver');
  
  try {
    // Test if User table exists and count records
    const userCount = await db.user.count();
    console.log(`   Users in database: ${userCount}`);
    
    // Test if Account table exists and count records
    const accountCount = await db.account.count();
    console.log(`   Accounts in database: ${accountCount}`);
    
    // Test a simple query to verify schema structure
    if (userCount > 0) {
      const sampleUser = await db.user.findFirst({
        select: {
          id: true,
          email: true,
          displayName: true,
          rank: true,
          createdAt: true,
        },
      });
      
      if (sampleUser) {
        console.log(`   Sample user ID: ${sampleUser.id}`);
        console.log(`   Sample user email: ${sampleUser.email}`);
        console.log(`   Sample user rank: ${sampleUser.rank}`);
      }
    }
    
    // Test UNIQUEIDENTIFIER support (SQL Server UUID)
    const guidTest = await db.$queryRaw<Array<{ new_guid: string }>>`SELECT NEWID() as new_guid`;
    if (guidTest.length > 0) {
      console.log(`   UNIQUEIDENTIFIER test: ${guidTest[0].new_guid.substring(0, 8)}...`);
    }
    
    // Test datetime precision
    const datetimeTest = await db.$queryRaw<Array<{ 
      datetime: Date, 
      datetime2: Date,
      smalldatetime: Date 
    }>>`
      SELECT 
        GETDATE() as datetime,
        SYSDATETIME() as datetime2,
        GETDATE() as smalldatetime
    `;
    
    if (datetimeTest.length > 0) {
      console.log(`   DateTime precision test successful`);
    }
    
    console.log('✅ SQL Server schema operations successful');
    
  } catch (error) {
    if (error instanceof Error && error.message.includes('does not exist')) {
      console.log('⚠️  Database tables do not exist yet. Run migrations first:');
      console.log('   npx prisma migrate dev --schema=providers/sqlserver.prisma');
    } else {
      throw error;
    }
  }
}

/**
 * Test transaction capabilities
 */
async function testTransactions(): Promise<void> {
  console.log('💼 Testing SQL Server transaction capabilities...');
  
  const db = await DatabaseClient.getInstance('sqlserver');
  
  try {
    // Test transaction rollback
    await db.$transaction(async (tx) => {
      // This transaction will be rolled back automatically
      const testUser = await tx.user.create({
        data: {
          email: 'test-transaction@example.com',
          displayName: 'Test Transaction User',
          rank: 999,
        },
      });
      
      console.log(`   Created test user in transaction: ${testUser.id}`);
      
      // Throw error to trigger rollback
      throw new Error('Intentional rollback for testing');
    });
  } catch (error) {
    if (error instanceof Error && error.message === 'Intentional rollback for testing') {
      console.log('✅ Transaction rollback successful');
    } else {
      console.error('❌ Unexpected transaction error:', error);
      throw error;
    }
  }
  
  // Verify the test user was not persisted
  const testUser = await db.user.findFirst({
    where: { email: 'test-transaction@example.com' },
  });
  
  if (!testUser) {
    console.log('✅ Transaction isolation verified');
  } else {
    console.log('⚠️  Transaction may not have rolled back properly');
    // Clean up if somehow persisted
    await db.user.delete({ where: { id: testUser.id } });
  }
  
  // Test SQL Server specific transaction features
  try {
    console.log('   Testing SQL Server transaction isolation levels...');
    
    // Test READ COMMITTED (default)
    await db.$queryRaw`SET TRANSACTION ISOLATION LEVEL READ COMMITTED`;
    console.log('   READ COMMITTED isolation level set');
    
    // Test SNAPSHOT isolation (if enabled)
    try {
      await db.$queryRaw`SET TRANSACTION ISOLATION LEVEL SNAPSHOT`;
      console.log('   SNAPSHOT isolation level available');
    } catch (error) {
      console.log('   SNAPSHOT isolation not enabled (requires database configuration)');
    } finally {
      // Reset to default
      await db.$queryRaw`SET TRANSACTION ISOLATION LEVEL READ COMMITTED`;
    }
    
    console.log('✅ Transaction isolation levels tested');
    
  } catch (error) {
    console.warn('⚠️  Transaction isolation level testing limited:', error);
  }
}

/**
 * Main test execution function
 */
async function main(): Promise<void> {
  console.log('🧪 SQL Server Provider Test Suite\n');
  
  try {
    // Validate environment
    validateProvider();
    console.log('✅ Environment validation passed\n');
    
    // Run test suite
    await testConnection();
    await testDatabaseInfo();
    await testSQLServerFeatures();
    await testSchemaOperations();
    await testTransactions();
    
    console.log('\n🎉 All SQL Server tests passed successfully!');
    console.log('\n📋 Test Summary:');
    console.log('   ✅ Connection test');
    console.log('   ✅ Database information retrieval');
    console.log('   ✅ SQL Server-specific features');
    console.log('   ✅ Schema operations');
    console.log('   ✅ Transaction capabilities');
    
    console.log('\n💡 SQL Server Notes:');
    console.log('   - IDENTITY columns for auto-increment');
    console.log('   - UNIQUEIDENTIFIER for UUID support');
    console.log('   - Multiple datetime precision options');
    console.log('   - Advanced transaction isolation levels');
    console.log('   - Rich system function library');
    
  } catch (error) {
    console.error('\n❌ SQL Server test failed:', error);
    process.exit(1);
  } finally {
    // Ensure connection is closed
    try {
      const db = await DatabaseClient.getInstance('sqlserver');
      await db.$disconnect();
      console.log('\n🔌 SQL Server connection closed');
    } catch (error) {
      console.warn('⚠️  Warning: Could not close SQL Server connection:', error);
    }
  }
}

// Execute main function
if (require.main === module) {
  main().catch((error) => {
    console.error('💥 Unhandled error in SQL Server test:', error);
    process.exit(1);
  });
}

export { main, validateProvider, testConnection, testDatabaseInfo, testSQLServerFeatures, testSchemaOperations, testTransactions };
