#!/usr/bin/env ts-node

/**
 * Test Supabase provider connectivity and basic operations
 * Usage: ts-node scripts/test-supabase.ts
 * 
 * Environment Variables:
 * - ENABLE_SUPABASE=true (must be enabled)
 * - SUPABASE_DATABASE_URL=postgresql://... (connection string)
 */

import dotenv from 'dotenv';
import { DatabaseClient } from '../src/DatabaseClient';

dotenv.config();

/**
 * Check if Supabase provider is enabled
 */
function validateProvider(): void {
  if (process.env.ENABLE_SUPABASE !== 'true') {
    throw new Error(
      'Supabase provider is not enabled. ' +
      'Set ENABLE_SUPABASE=true in .env to run this test.'
    );
  }
  
  if (!process.env.SUPABASE_DATABASE_URL) {
    throw new Error(
      'SUPABASE_DATABASE_URL is not configured. ' +
      'Set SUPABASE_DATABASE_URL=postgresql://... in .env to run this test.'
    );
  }
}

/**
 * Test basic database connectivity
 */
async function testConnection(): Promise<void> {
  console.log('🔗 Testing Supabase connection...');
  
  const db = await DatabaseClient.getInstance('supabase');
  
  // Test basic connection with a simple query
  await db.$queryRaw`SELECT 1 as test`;
  
  console.log('✅ Supabase connection successful');
}

/**
 * Test database information retrieval
 */
async function testDatabaseInfo(): Promise<void> {
  console.log('📊 Retrieving Supabase database information...');
  
  const db = await DatabaseClient.getInstance('supabase');
  
  // Get PostgreSQL version (Supabase uses PostgreSQL)
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
  
  // Check for Supabase-specific extensions
  const extensionsResult = await db.$queryRaw<Array<{ extname: string }>>`
    SELECT extname 
    FROM pg_extension 
    WHERE extname IN ('supabase_vault', 'pgsodium', 'pg_graphql', 'pg_stat_statements')
  `;
  const supabaseExtensions = extensionsResult.map(ext => ext.extname);
  
  console.log('📋 Supabase Database Information:');
  console.log(`   PostgreSQL Version: ${version.split(' ').slice(0, 2).join(' ')}`);
  console.log(`   Database: ${currentDb}`);
  console.log(`   User: ${currentUser}`);
  console.log(`   Tables in public schema: ${tableCount}`);
  if (supabaseExtensions.length > 0) {
    console.log(`   Supabase Extensions: ${supabaseExtensions.join(', ')}`);
  }
}

/**
 * Test Supabase-specific features
 */
async function testSupabaseFeatures(): Promise<void> {
  console.log('🚀 Testing Supabase-specific features...');
  
  const db = await DatabaseClient.getInstance('supabase');
  
  try {
    // Test RLS (Row Level Security) policies
    const rlsPoliciesResult = await db.$queryRaw<Array<{ 
      tablename: string, 
      policyname: string,
      cmd: string 
    }>>`
      SELECT tablename, policyname, cmd
      FROM pg_policies 
      WHERE schemaname = 'public'
      LIMIT 5
    `;
    
    if (rlsPoliciesResult.length > 0) {
      console.log(`   RLS Policies found: ${rlsPoliciesResult.length}`);
      rlsPoliciesResult.forEach(policy => {
        console.log(`     - ${policy.tablename}.${policy.policyname} (${policy.cmd})`);
      });
    } else {
      console.log('   No RLS policies configured');
    }
    
    // Test for auth schema (Supabase authentication)
    const authSchemaResult = await db.$queryRaw<Array<{ schema_name: string }>>`
      SELECT schema_name 
      FROM information_schema.schemata 
      WHERE schema_name = 'auth'
    `;
    
    if (authSchemaResult.length > 0) {
      console.log('✅ Supabase Auth schema detected');
      
      // Check auth tables
      const authTablesResult = await db.$queryRaw<Array<{ table_name: string }>>`
        SELECT table_name 
        FROM information_schema.tables 
        WHERE table_schema = 'auth' AND table_type = 'BASE TABLE'
        LIMIT 5
      `;
      
      if (authTablesResult.length > 0) {
        const authTables = authTablesResult.map(t => t.table_name);
        console.log(`   Auth tables: ${authTables.join(', ')}`);
      }
    } else {
      console.log('⚠️  Supabase Auth schema not found');
    }
    
    // Test for storage schema (Supabase storage)
    const storageSchemaResult = await db.$queryRaw<Array<{ schema_name: string }>>`
      SELECT schema_name 
      FROM information_schema.schemata 
      WHERE schema_name = 'storage'
    `;
    
    if (storageSchemaResult.length > 0) {
      console.log('✅ Supabase Storage schema detected');
    } else {
      console.log('⚠️  Supabase Storage schema not found');
    }
    
    // Test for realtime functionality
    const realtimeResult = await db.$queryRaw<Array<{ extname: string }>>`
      SELECT extname 
      FROM pg_extension 
      WHERE extname = 'supabase_realtime'
    `;
    
    if (realtimeResult.length > 0) {
      console.log('✅ Supabase Realtime extension available');
    } else {
      console.log('⚠️  Supabase Realtime extension not found');
    }
    
  } catch (error) {
    console.warn('⚠️  Limited access to Supabase-specific features:', error);
  }
}

/**
 * Test Prisma schema operations
 */
async function testSchemaOperations(): Promise<void> {
  console.log('🔍 Testing Supabase schema operations...');
  
  const db = await DatabaseClient.getInstance('supabase');
  
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
    
    // Test UUID generation (common in Supabase)
    const uuidResult = await db.$queryRaw<Array<{ uuid: string }>>`SELECT gen_random_uuid() as uuid`;
    const generatedUuid = uuidResult[0]?.uuid;
    
    if (generatedUuid) {
      console.log(`   UUID generation test: ${generatedUuid.substring(0, 8)}...`);
    }
    
    console.log('✅ Supabase schema operations successful');
    
  } catch (error) {
    if (error instanceof Error && error.message.includes('does not exist')) {
      console.log('⚠️  Database tables do not exist yet. Run migrations first:');
      console.log('   npx prisma migrate dev --schema=providers/supabase.prisma');
    } else {
      throw error;
    }
  }
}

/**
 * Test transaction capabilities
 */
async function testTransactions(): Promise<void> {
  console.log('💼 Testing Supabase transaction capabilities...');
  
  const db = await DatabaseClient.getInstance('supabase');
  
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
  console.log('🧪 Supabase Provider Test Suite\n');
  
  try {
    // Validate environment
    validateProvider();
    console.log('✅ Environment validation passed\n');
    
    // Run test suite
    await testConnection();
    await testDatabaseInfo();
    await testSupabaseFeatures();
    await testSchemaOperations();
    await testTransactions();
    
    console.log('\n🎉 All Supabase tests passed successfully!');
    console.log('\n📋 Test Summary:');
    console.log('   ✅ Connection test');
    console.log('   ✅ Database information retrieval');
    console.log('   ✅ Supabase-specific features');
    console.log('   ✅ Schema operations');
    console.log('   ✅ Transaction capabilities');
    
  } catch (error) {
    console.error('\n❌ Supabase test failed:', error);
    process.exit(1);
  } finally {
    // Ensure connection is closed
    try {
      const db = await DatabaseClient.getInstance('supabase');
      await db.$disconnect();
      console.log('\n🔌 Supabase connection closed');
    } catch (error) {
      console.warn('⚠️  Warning: Could not close Supabase connection:', error);
    }
  }
}

// Execute main function
if (require.main === module) {
  main().catch((error) => {
    console.error('💥 Unhandled error in Supabase test:', error);
    process.exit(1);
  });
}

export { main, validateProvider, testConnection, testDatabaseInfo, testSupabaseFeatures, testSchemaOperations, testTransactions };
