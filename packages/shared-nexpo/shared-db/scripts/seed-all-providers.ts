#!/usr/bin/env ts-node

/**
 * Seed mock data for all enabled Prisma providers
 * Usage: ts-node scripts/seed-all-providers.ts [options]
 * 
 * Environment Variables:
 * - ENABLE_POSTGRESQL=true|false
 * - ENABLE_MONGODB=true|false  
 * - ENABLE_SUPABASE=true|false
 * - ENABLE_COSMOSDB=true|false
 * - ENABLE_SQLSERVER=true|false
 * - ENABLE_IBM=true|false
 * - SEED_USER_COUNT=50 (number of users to generate)
 * - SEED_ACCOUNT_COUNT=100 (number of accounts to generate)
 * - SEED_SCENARIOS=true (include test scenarios)
 */

import dotenv from 'dotenv';
import { MockSeeder } from '../src/mocks/MockSeeder';

dotenv.config();

interface SeedOptions {
  userCount: number;
  accountCount: number;
  includeScenarios: boolean;
  dryRun: boolean;
}

/**
 * Get enabled providers from environment variables
 */
function getEnabledProviders(): string[] {
  const providers: string[] = [];
  
  if (process.env.ENABLE_POSTGRESQL === 'true') providers.push('postgres');
  if (process.env.ENABLE_MONGODB === 'true') providers.push('mongodb');
  if (process.env.ENABLE_SUPABASE === 'true') providers.push('supabase');
  if (process.env.ENABLE_COSMOSDB === 'true') providers.push('cosmosdb');
  if (process.env.ENABLE_SQLSERVER === 'true') providers.push('sqlserver');
  if (process.env.ENABLE_IBM === 'true') providers.push('ibmcloud');
  
  return providers;
}

/**
 * Parse command line arguments
 */
function parseArgs(): SeedOptions {
  const args = process.argv.slice(2);
  
  return {
    userCount: parseInt(process.env.SEED_USER_COUNT || '50'),
    accountCount: parseInt(process.env.SEED_ACCOUNT_COUNT || '100'),
    includeScenarios: process.env.SEED_SCENARIOS !== 'false',
    dryRun: args.includes('--dry-run') || args.includes('-d'),
  };
}

/**
 * Display help information
 */
function showHelp(): void {
  console.log(`
🌱 Seed All Enabled Providers

Usage:
  ts-node scripts/seed-all-providers.ts [options]

Options:
  --dry-run, -d    Show what would be seeded without actually seeding
  --help, -h       Show this help message

Environment Variables:
  ENABLE_POSTGRESQL=true     Enable PostgreSQL provider
  ENABLE_MONGODB=true        Enable MongoDB provider
  ENABLE_SUPABASE=true       Enable Supabase provider
  ENABLE_COSMOSDB=true       Enable CosmosDB provider
  ENABLE_SQLSERVER=true      Enable SQL Server provider
  ENABLE_IBM=true       Enable IBM Cloud DB2 provider
  SEED_USER_COUNT=50         Number of users to generate per provider
  SEED_ACCOUNT_COUNT=100     Number of accounts to generate per provider
  SEED_SCENARIOS=true        Include test scenarios (admin, new users, etc.)

Examples:
  # Seed all enabled providers
  pnpm run seed:all

  # Dry run to see what would be seeded
  pnpm run seed:all -- --dry-run

  # Seed with custom counts
  SEED_USER_COUNT=100 SEED_ACCOUNT_COUNT=200 pnpm run seed:all
`);
}

/**
 * Main execution function
 */
async function main(): Promise<void> {
  try {
    const args = process.argv.slice(2);
    
    if (args.includes('--help') || args.includes('-h')) {
      showHelp();
      process.exit(0);
    }
    
    const options = parseArgs();
    const enabledProviders = getEnabledProviders();
    
    if (enabledProviders.length === 0) {
      console.log('⚠️  No database providers are enabled.');
      console.log('   Enable providers in .env using: ENABLE_POSTGRESQL=true, ENABLE_MONGODB=true, etc.');
      console.log('   Available providers: postgresql, mongodb, supabase, cosmosdb, sqlserver, ibmcloud');
      process.exit(1);
    }
    
    console.log(`🌍 Seeding mock data for enabled providers: ${enabledProviders.join(', ')}\n`);
    
    if (options.dryRun) {
      console.log('🔍 DRY RUN MODE - No data will be created\n');
      
      for (const provider of enabledProviders) {
        console.log(`📋 Would seed ${provider}:`);
        console.log(`   - ${options.userCount} users`);
        console.log(`   - ${options.accountCount} accounts`);
        if (options.includeScenarios) {
          console.log(`   - Test scenarios (admins, new users, federated users, etc.)`);
        }
        console.log();
      }
      
      console.log('✅ Dry run complete. Use without --dry-run to actually seed data.');
      return;
    }
    
    // Seed all enabled providers
    let successCount = 0;
    let errorCount = 0;
    
    for (const provider of enabledProviders) {
      try {
        console.log(`🌱 Seeding ${provider}...`);
        
        await MockSeeder.seedAll(provider, {
          userCount: options.userCount,
          accountCount: options.accountCount,
          includeScenarios: options.includeScenarios,
        });
        
        successCount++;
        console.log(`✅ Successfully seeded ${provider}\n`);
        
      } catch (error) {
        errorCount++;
        console.error(`❌ Failed to seed ${provider}:`, error);
        console.log(`   Continuing with next provider...\n`);
      }
    }
    
    // Summary
    console.log(`🎊 Seeding complete!`);
    console.log(`   ✅ Successful: ${successCount}/${enabledProviders.length} providers`);
    if (errorCount > 0) {
      console.log(`   ❌ Failed: ${errorCount}/${enabledProviders.length} providers`);
    }
    console.log();
    
    // Display statistics
    try {
      console.log('📊 Final Statistics:');
      await MockSeeder.displayAllProviderStats();
    } catch (error) {
      console.warn('⚠️  Could not display statistics:', error);
    }
    
  } catch (error) {
    console.error('💥 Fatal error during seeding:', error);
    process.exit(1);
  }
}

// Execute main function
if (require.main === module) {
  main().catch((error) => {
    console.error('💥 Unhandled error:', error);
    process.exit(1);
  });
}

export { main, getEnabledProviders, parseArgs };
