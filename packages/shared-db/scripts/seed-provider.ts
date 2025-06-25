#!/usr/bin/env ts-node

/**
 * Seed mock data for a specific Prisma provider
 * Usage: ts-node scripts/seed-provider.ts <provider> [options]
 * 
 * Environment Variables:
 * - ENABLE_<PROVIDER>=true|false (must be enabled)
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
  force: boolean;
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
 * Validate provider name and check if enabled
 */
function validateProvider(provider: string): void {
  const validProviders = ['postgres', 'mongodb', 'supabase', 'cosmosdb', 'sqlserver', 'ibmcloud'];
  const enabledProviders = getEnabledProviders();
  
  if (!validProviders.includes(provider)) {
    throw new Error(
      `Invalid provider '${provider}'. ` +
      `Valid providers: ${validProviders.join(', ')}`
    );
  }
  
  if (!enabledProviders.includes(provider)) {
    throw new Error(
      `Provider '${provider}' is not enabled. ` +
      `Enable it in .env: ENABLE_${provider.toUpperCase()}=true`
    );
  }
}

/**
 * Parse command line arguments
 */
function parseArgs(): { provider: string; options: SeedOptions } {
  const args = process.argv.slice(2);
  
  if (args.length === 0) {
    throw new Error('Provider argument is required');
  }
  
  const provider = args[0].toLowerCase();
  
  return {
    provider,
    options: {
      userCount: parseInt(process.env.SEED_USER_COUNT || '50'),
      accountCount: parseInt(process.env.SEED_ACCOUNT_COUNT || '100'),
      includeScenarios: process.env.SEED_SCENARIOS !== 'false',
      dryRun: args.includes('--dry-run') || args.includes('-d'),
      force: args.includes('--force') || args.includes('-f'),
    },
  };
}

/**
 * Display help information
 */
function showHelp(): void {
  const enabledProviders = getEnabledProviders();
  
  console.log(`
🌱 Seed Specific Provider

Usage:
  ts-node scripts/seed-provider.ts <provider> [options]

Available Providers:
  postgres      PostgreSQL database
  mongodb       MongoDB database  
  supabase      Supabase PostgreSQL
  cosmosdb      Azure CosmosDB
  sqlserver     Microsoft SQL Server
  ibmcloud      IBM Cloud DB2

Currently Enabled: ${enabledProviders.length > 0 ? enabledProviders.join(', ') : 'None'}

Options:
  --dry-run, -d    Show what would be seeded without actually seeding
  --force, -f      Force seeding even if data already exists
  --help, -h       Show this help message

Environment Variables:
  ENABLE_<PROVIDER>=true     Must be true to use the provider
  SEED_USER_COUNT=50         Number of users to generate
  SEED_ACCOUNT_COUNT=100     Number of accounts to generate  
  SEED_SCENARIOS=true        Include test scenarios

Examples:
  # Seed PostgreSQL (if enabled)
  npm run seed:provider postgres

  # Dry run for MongoDB
  npm run seed:provider mongodb -- --dry-run

  # Force seed with custom counts
  SEED_USER_COUNT=100 npm run seed:provider supabase -- --force
`);
}

/**
 * Main execution function
 */
async function main(): Promise<void> {
  try {
    const args = process.argv.slice(2);
    
    if (args.includes('--help') || args.includes('-h') || args.length === 0) {
      showHelp();
      process.exit(args.length === 0 ? 1 : 0);
    }
    
    const { provider, options } = parseArgs();
    
    // Validate provider
    validateProvider(provider);
    
    console.log(`🌱 Seeding mock data for ${provider}...`);
    console.log(`   Users: ${options.userCount}`);
    console.log(`   Accounts: ${options.accountCount}`);
    console.log(`   Test Scenarios: ${options.includeScenarios ? 'Yes' : 'No'}`);
    
    if (options.dryRun) {
      console.log(`   Mode: DRY RUN (no data will be created)`);
      console.log();
      console.log(`✅ Dry run complete for ${provider}. Remove --dry-run to actually seed data.`);
      return;
    }
    
    console.log();
    
    // Check if data already exists (unless forced)
    if (!options.force) {
      try {
        const stats = await MockSeeder.getProviderStats(provider);
        if (stats.userCount > 0 || stats.accountCount > 0) {
          console.log(`⚠️  Provider ${provider} already contains data:`);
          console.log(`   Users: ${stats.userCount}`);
          console.log(`   Accounts: ${stats.accountCount}`);
          console.log();
          console.log(`   Use --force to seed anyway, or clear data first:`);
          console.log(`   npm run seed:clear:provider ${provider}`);
          process.exit(1);
        }
      } catch (error) {
        console.warn(`⚠️  Could not check existing data:`, error);
        console.log(`   Proceeding with seeding...`);
      }
    }
    
    // Perform seeding
    const startTime = Date.now();
    
    await MockSeeder.seedAll(provider, {
      userCount: options.userCount,
      accountCount: options.accountCount,
      includeScenarios: options.includeScenarios,
    });
    
    const endTime = Date.now();
    const duration = ((endTime - startTime) / 1000).toFixed(2);
    
    console.log(`✅ Successfully seeded ${provider} in ${duration}s`);
    
    // Display final statistics
    try {
      console.log();
      console.log('📊 Final Statistics:');
      const stats = await MockSeeder.getProviderStats(provider);
      console.log(`   Users: ${stats.userCount}`);
      console.log(`   Accounts: ${stats.accountCount}`);
      console.log(`   Database: ${provider}`);
    } catch (error) {
      console.warn('⚠️  Could not display final statistics:', error);
    }
    
  } catch (error) {
    if (error instanceof Error) {
      console.error('❌', error.message);
    } else {
      console.error('💥 Unexpected error:', error);
    }
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

export { main, validateProvider, getEnabledProviders };
