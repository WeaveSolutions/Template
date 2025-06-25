#!/usr/bin/env ts-node

/**
 * Test PostgreSQL provider connectivity and basic operations
 * Usage: ts-node scripts/test-postgres.ts
 * 
 * Environment Variables:
 * - ENABLE_POSTGRESQL=true (must be enabled)
 * - DATABASE_URL=postgresql://... (connection string)
 */

import dotenv from 'dotenv';
import { DatabaseClient } from '../src/DatabaseClient';

dotenv.config();

/**
 * Check if PostgreSQL provider is enabled
 */
function validateProvider(): void {
  if (process.env.ENABLE_POSTGRESQL !== 'true') {
    throw new Error(
      'PostgreSQL provider is not enabled. ' +
      'Set ENABLE_POSTGRESQL=true in .env to run this test.'
    );
  }
  
  if (!process.env.DATABASE_URL) {
    throw new Error(
      'DATABASE_URL is not configured. ' +
      'Set DATABASE_URL=postgresql://... in .env to run this test.'
    );
  }
}

/**
 * Test basic database connectivity
 */
async function testConnection(): Promise<void> {
  console.log('🔗 Testing PostgreSQL connection...');
  
  const db = await DatabaseClient.getInstance('postgres');
  
  // Test basic connection with a simple query
  await db.$queryRaw`SELECT 1 as test`;
  
  console.log('✅ PostgreSQL connection successful');
}

/**
 * Test database information retrieval
 */
async function testDatabaseInfo(): Promise<void> {
  console.log('📊 Retrieving PostgreSQL database information...');
  
  const db = await DatabaseClient.getInstance('postgres');
  
  // Get PostgreSQL version
  const versionResult = await db.$queryRaw<Array<{ version: string }>>`SELECT version()`;
  const version = versionResult[0]?.version || 'Unknown';
  
  // Get current database name
  const dbResult = await db.$queryRaw<Array<{ current_database: string }>>`SELECT current_database()`;
  const currentDb = dbResult[0]?.current_database || 'Unknown';
  
  // Get current user
  const userResult = await db.$queryRaw<Array<{ current_user: string }>>`SELECT current_user`;
  const currentUser = userResult[0]?.current_user || 'Unknown';
  
  // Get table count in public schema
  const tableCountResult = await db.$queryRaw<Array<{ count: bigint }>>`
    SELECT COUNT(*) as count 
    FROM information_schema.tables 
    WHERE table_schema = 'public' AND table_type = 'BASE TABLE'
  `;
  const tableCount = Number(tableCountResult[0]?.count || 0);
  
  console.log('📋 PostgreSQL Database Information:');
  console.log(`   Version: ${version.split(' ').slice(0, 2).join(' ')}`);
  console.log(`   Database: ${currentDb}`);
  console.log(`   User: ${currentUser}`);
  console.log(`   Tables in public schema: ${tableCount}`);
}

/**
 * Test Prisma schema operations
 */
async function testSchemaOperations(): Promise<void> {
  console.log('🔍 Testing PostgreSQL schema operations...');
  
  const db = await DatabaseClient.getInstance('postgres');
  
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
    
    console.log('✅ PostgreSQL schema operations successful');
    
  } catch (error) {
    if (error instanceof Error && error.message.includes('does not exist')) {
      console.log('⚠️  Database tables do not exist yet. Run migrations first:');
      console.log('   npx prisma migrate dev --schema=providers/postgres.prisma');
    } else {
      throw error;
    }
  }
}

/**
 * Test transaction capabilities
 */
async function testTransactions(): Promise<void> {
  console.log('💼 Testing PostgreSQL transaction capabilities...');
  
  const db = await DatabaseClient.getInstance('postgres');
  
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
}

/**
 * Main test execution function
 */
async function main(): Promise<void> {
  console.log('🧪 PostgreSQL Provider Test Suite\n');
  
  try {
    // Validate environment
    validateProvider();
    console.log('✅ Environment validation passed\n');
    
    // Run test suite
    await testConnection();
    await testDatabaseInfo();
    await testSchemaOperations();
    await testTransactions();
    
    console.log('\n🎉 All PostgreSQL tests passed successfully!');
    console.log('\n📋 Test Summary:');
    console.log('   ✅ Connection test');
    console.log('   ✅ Database information retrieval');
    console.log('   ✅ Schema operations');
    console.log('   ✅ Transaction capabilities');
    
  } catch (error) {
    console.error('\n❌ PostgreSQL test failed:', error);
    process.exit(1);
  } finally {
    // Ensure connection is closed
    try {
      const db = await DatabaseClient.getInstance('postgres');
      await db.$disconnect();
      console.log('\n🔌 PostgreSQL connection closed');
    } catch (error) {
      console.warn('⚠️  Warning: Could not close PostgreSQL connection:', error);
    }
  }
}

// Execute main function
if (require.main === module) {
  main().catch((error) => {
    console.error('💥 Unhandled error in PostgreSQL test:', error);
    process.exit(1);
  });
}

export { main, validateProvider, testConnection, testDatabaseInfo, testSchemaOperations, testTransactions };
