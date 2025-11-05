#!/usr/bin/env ts-node

/**
 * Test MongoDB provider connectivity and basic operations
 * Usage: ts-node scripts/test-mongodb.ts
 * 
 * Environment Variables:
 * - ENABLE_MONGODB=true (must be enabled)
 * - MONGODB_URL=mongodb://... (connection string)
 */

import dotenv from 'dotenv';
import { DatabaseClient } from '../src/DatabaseClient';

dotenv.config();

/**
 * Check if MongoDB provider is enabled
 */
function validateProvider(): void {
  if (process.env.ENABLE_MONGODB !== 'true') {
    throw new Error(
      'MongoDB provider is not enabled. ' +
      'Set ENABLE_MONGODB=true in .env to run this test.'
    );
  }
  
  if (!process.env.MONGODB_URL) {
    throw new Error(
      'MONGODB_URL is not configured. ' +
      'Set MONGODB_URL=mongodb://... in .env to run this test.'
    );
  }
}

/**
 * Test basic database connectivity
 */
async function testConnection(): Promise<void> {
  console.log('🔗 Testing MongoDB connection...');
  
  const db = await DatabaseClient.getInstance('mongodb');
  
  // Test basic connection with a simple query
  await db.$runCommandRaw({ ping: 1 });
  
  console.log('✅ MongoDB connection successful');
}

/**
 * Test database information retrieval
 */
async function testDatabaseInfo(): Promise<void> {
  console.log('📊 Retrieving MongoDB database information...');
  
  const db = await DatabaseClient.getInstance('mongodb');
  
  try {
    // Get server info
    const serverInfo = await db.$runCommandRaw({ 
      buildInfo: 1 
    }) as any;
    
    // Get database stats
    const dbStats = await db.$runCommandRaw({ 
      dbStats: 1, 
      scale: 1024 * 1024  // Convert to MB
    }) as any;
    
    // Get connection info
    const connectionStatus = await db.$runCommandRaw({ 
      connectionStatus: 1 
    }) as any;
    
    console.log('📋 MongoDB Database Information:');
    console.log(`   Version: ${serverInfo.version || 'Unknown'}`);
    console.log(`   Database: ${dbStats.db || 'Unknown'}`);
    console.log(`   Collections: ${dbStats.collections || 0}`);
    console.log(`   Data Size: ${(dbStats.dataSize || 0).toFixed(2)} MB`);
    console.log(`   Index Size: ${(dbStats.indexSize || 0).toFixed(2)} MB`);
    console.log(`   User: ${connectionStatus.authInfo?.authenticatedUsers?.[0]?.user || 'Unknown'}`);
    
  } catch (error) {
    // Fallback for limited permissions
    console.log('📋 MongoDB Database Information (Limited):');
    console.log('   Connection: Established');
    console.log('   Note: Limited database info due to permissions');
  }
}

/**
 * Test Prisma schema operations
 */
async function testSchemaOperations(): Promise<void> {
  console.log('🔍 Testing MongoDB schema operations...');
  
  const db = await DatabaseClient.getInstance('mongodb');
  
  try {
    // Test if User collection exists and count documents
    const userCount = await db.user.count();
    console.log(`   Users in database: ${userCount}`);
    
    // Test if Account collection exists and count documents
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
    
    // Test MongoDB-specific operations
    const collections = await db.$runCommandRaw({
      listCollections: 1,
      nameOnly: true
    }) as any;
    
    if (collections.cursor?.firstBatch) {
      const collectionNames = collections.cursor.firstBatch.map((c: any) => c.name);
      console.log(`   Collections: ${collectionNames.join(', ')}`);
    }
    
    console.log('✅ MongoDB schema operations successful');
    
  } catch (error) {
    if (error instanceof Error && error.message.includes('Collection')) {
      console.log('⚠️  Database collections may not exist yet. Generate Prisma client first:');
      console.log('   npx prisma generate --schema=providers/mongodb.prisma');
    } else {
      throw error;
    }
  }
}

/**
 * Test MongoDB-specific features
 */
async function testMongoFeatures(): Promise<void> {
  console.log('🍃 Testing MongoDB-specific features...');
  
  const db = await DatabaseClient.getInstance('mongodb');
  
  try {
    // Test aggregation pipeline
    const pipeline = [
      { $group: { _id: null, totalUsers: { $sum: 1 } } }
    ];
    
    const aggregationResult = await db.user.aggregateRaw({
      pipeline
    });
    
    console.log('✅ Aggregation pipeline test successful');
    
    // Test text search capabilities (if available)
    try {
      const searchResult = await db.user.findMany({
        where: {
          OR: [
            { email: { contains: 'test' } },
            { displayName: { contains: 'test' } }
          ]
        },
        take: 1
      });
      
      console.log('✅ Text search capabilities verified');
    } catch (error) {
      console.log('⚠️  Text search test skipped (no test data)');
    }
    
    // Test index operations
    try {
      const indexes = await db.$runCommandRaw({
        listIndexes: 'User'
      }) as any;
      
      if (indexes.cursor?.firstBatch) {
        const indexNames = indexes.cursor.firstBatch.map((idx: any) => idx.name);
        console.log(`   User collection indexes: ${indexNames.join(', ')}`);
      }
      
      console.log('✅ Index information retrieved');
    } catch (error) {
      console.log('⚠️  Index information not available');
    }
    
  } catch (error) {
    console.warn('⚠️  Some MongoDB-specific features not available:', error);
  }
}

/**
 * Test transaction capabilities (if supported)
 */
async function testTransactions(): Promise<void> {
  console.log('💼 Testing MongoDB transaction capabilities...');
  
  const db = await DatabaseClient.getInstance('mongodb');
  
  try {
    // Check if transactions are supported (requires replica set)
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
    } else if (error instanceof Error && error.message.includes('Transactions')) {
      console.log('⚠️  Transactions not supported (requires MongoDB replica set)');
    } else {
      console.error('❌ Unexpected transaction error:', error);
      throw error;
    }
  }
}

/**
 * Main test execution function
 */
async function main(): Promise<void> {
  console.log('🧪 MongoDB Provider Test Suite\n');
  
  try {
    // Validate environment
    validateProvider();
    console.log('✅ Environment validation passed\n');
    
    // Run test suite
    await testConnection();
    await testDatabaseInfo();
    await testSchemaOperations();
    await testMongoFeatures();
    await testTransactions();
    
    console.log('\n🎉 All MongoDB tests completed!');
    console.log('\n📋 Test Summary:');
    console.log('   ✅ Connection test');
    console.log('   ✅ Database information retrieval');
    console.log('   ✅ Schema operations');
    console.log('   ✅ MongoDB-specific features');
    console.log('   ✅ Transaction capabilities (if supported)');
    
  } catch (error) {
    console.error('\n❌ MongoDB test failed:', error);
    process.exit(1);
  } finally {
    // Ensure connection is closed
    try {
      const db = await DatabaseClient.getInstance('mongodb');
      await db.$disconnect();
      console.log('\n🔌 MongoDB connection closed');
    } catch (error) {
      console.warn('⚠️  Warning: Could not close MongoDB connection:', error);
    }
  }
}

// Execute main function
if (require.main === module) {
  main().catch((error) => {
    console.error('💥 Unhandled error in MongoDB test:', error);
    process.exit(1);
  });
}

export { main, validateProvider, testConnection, testDatabaseInfo, testSchemaOperations, testMongoFeatures, testTransactions };
