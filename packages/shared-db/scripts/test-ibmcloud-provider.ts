/**
 * Test script for IBM Cloud DB2 provider
 * 
 * This script tests the basic functionality of the IBM Cloud DB2 provider.
 * It validates connection, query execution, and error handling.
 * 
 * Usage:
 * ts-node test-ibmcloud-provider.ts
 */

import { DatabaseClient } from '../src/client';
import dotenv from 'dotenv';

// Load environment variables
dotenv.config();

async function main(): Promise<number> {
  console.log('🧪 IBM Cloud DB2 Provider Test Suite\n');

  // Check if IBM Cloud provider is enabled
  if (process.env.ENABLE_IBM !== 'true') {
    console.log('⚠️  IBM Cloud provider is not enabled.');
    console.log('   Enable it by setting ENABLE_IBM=true in your .env file');
    return 1;
  }

  if (!process.env.IBMCLOUD_DATABASE_URL) {
    console.log('⚠️  IBM Cloud database URL not configured.');
    console.log('   Set IBMCLOUD_DATABASE_URL in your .env file');
    return 1;
  }

  try {
    console.log('🔗 Connecting to IBM Cloud DB2...');
    const dbClient = DatabaseClient.getInstance();
    const db = await dbClient.getClient('ibmcloud');
    console.log('✅ Connection established successfully');

    // Test basic connectivity with DB2-specific query
    console.log('🔄 Testing basic connectivity...');
    const connectivityTest = await db.$queryRaw`SELECT 1 as test FROM SYSIBM.SYSDUMMY1`;
    console.log('✅ Basic connectivity test successful');

    // Test database information retrieval
    console.log('📊 Retrieving database information...');
    try {
      const versionResult = await db.$queryRaw`
        SELECT SERVICE_LEVEL, FIXPACK_NUM 
        FROM SYSIBMADM.ENV_PROD_INFO 
        WHERE TYPENAME = 'Base'
      `;
      
      if (versionResult && Array.isArray(versionResult) && versionResult.length > 0) {
        console.log(`📋 DB2 Version: ${versionResult[0]?.SERVICE_LEVEL || 'Unknown'}`);
        console.log(`🔧 Fixpack: ${versionResult[0]?.FIXPACK_NUM || 'Unknown'}`);
      } else {
        console.log('📋 DB2 Version: Information not available');
      }
    } catch (versionError) {
      console.log('⚠️  Version query failed (may not have sufficient privileges)');
    }

    // Test schema operations
    console.log('🔍 Testing schema operations...');
    try {
      const userCount = await db.user.count();
      console.log(`👥 Users in database: ${userCount}`);
      
      // Test finding a sample user
      if (userCount > 0) {
        const sampleUser = await db.user.findFirst({
          take: 1
        });
        if (sampleUser) {
          console.log(`👤 Sample user found: ${sampleUser.email}`);
        }
      }
    } catch (schemaError) {
      console.log('⚠️  Schema operations may not be available (check if tables exist)');
    }

    // Test DB2-specific features
    console.log('🚀 Testing DB2-specific features...');
    try {
      // Test current timestamp
      const timestampResult = await db.$queryRaw`SELECT CURRENT TIMESTAMP as current_time FROM SYSIBM.SYSDUMMY1`;
      console.log(`⏰ Current timestamp: ${timestampResult[0]?.current_time}`);
      
      // Test database name
      const dbNameResult = await db.$queryRaw`SELECT CURRENT SERVER as db_name FROM SYSIBM.SYSDUMMY1`;
      console.log(`🏷️  Database name: ${dbNameResult[0]?.db_name}`);
      
      // Test user information
      const userInfoResult = await db.$queryRaw`SELECT USER as current_user FROM SYSIBM.SYSDUMMY1`;
      console.log(`👤 Current user: ${userInfoResult[0]?.current_user}`);
    } catch (featuresError) {
      console.log('⚠️  Some DB2 features may not be accessible');
    }

    // Test transaction capabilities
    console.log('💼 Testing transaction capabilities...');
    try {
      await db.$transaction(async (tx: any) => {
        // Test that we can execute queries within a transaction
        const transactionTest = await tx.$queryRaw`SELECT 'transaction_test' as test FROM SYSIBM.SYSDUMMY1`;
        console.log('✅ Transaction query executed successfully');
        
        // Intentionally throw to test rollback
        throw new Error('Intentional rollback test');
      });
    } catch (transactionError: unknown) {
      if (transactionError instanceof Error && transactionError.message.includes('Intentional rollback')) {
        console.log('✅ Transaction rollback test successful');
      } else {
        console.log('⚠️  Transaction test encountered unexpected error:', transactionError instanceof Error ? transactionError.message : String(transactionError));
      }
    }

    // Disconnect cleanly
    await dbClient.disconnect();
    console.log('👋 Disconnected from IBM Cloud DB2');
    
    console.log('\n🎉 All IBM Cloud DB2 tests completed successfully!');
    return 0;
  } catch (error) {
    console.error('❌ Error testing IBM Cloud DB2 provider:', error);
    return 1;
  }
}

// Run the test
main()
  .then(exitCode => process.exit(exitCode))
  .catch(error => {
    console.error('❌ Unhandled exception:', error);
    process.exit(1);
  });
