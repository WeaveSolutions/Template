#!/usr/bin/env ts-node

/**
 * Clear all mock data from a specific Prisma provider
 * Usage: ts-node scripts/clear-provider.ts <provider> [options]
 * 
 * Environment Variables:
 * - ENABLE_<PROVIDER>=true|false (must be enabled)
 * - NODE_ENV=development|production (safety check)
 * - FORCE_PRODUCTION_CLEAR=true (required for production clearing)
 */

import dotenv from 'dotenv';
import { MockSeeder } from '../src/mocks/MockSeeder';
import * as readline from 'readline';

dotenv.config();

interface ClearOptions {
  force: boolean;
  skipConfirmation: boolean;
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
 * Production safety check
 */
function checkProductionSafety(): void {
  const isProduction = process.env.NODE_ENV === 'production';
  const forceProductionClear = process.env.FORCE_PRODUCTION_CLEAR === 'true';
  
  if (isProduction && !forceProductionClear) {
    throw new Error(
      'Refusing to clear data in production environment. ' +
      'Set FORCE_PRODUCTION_CLEAR=true to override this safety check.'
    );
  }
  
  if (isProduction) {
    console.log('⚠️  PRODUCTION ENVIRONMENT DETECTED - Proceeding with forced clear');
  }
}

/**
 * Parse command line arguments
 */
function parseArgs(): { provider: string; options: ClearOptions } {
  const args = process.argv.slice(2);
  
  if (args.length === 0) {
    throw new Error('Provider argument is required');
  }
  
  const provider = args[0].toLowerCase();
  
  return {
    provider,
    options: {
      force: args.includes('--force') || args.includes('-f') || process.env.FORCE_CLEAR === 'true',
      skipConfirmation: args.includes('--yes') || args.includes('-y') || process.env.CI === 'true',
      dryRun: args.includes('--dry-run') || args.includes('-d'),
    },
  };
}

/**
 * Get user confirmation for dangerous operation
 */
async function getConfirmation(provider: string, stats: any): Promise<boolean> {
  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
  });
  
  return new Promise((resolve) => {
    console.log(`⚠️  This will permanently delete ALL data from ${provider}:`);
    console.log(`   - ${stats.userCount} users`);
    console.log(`   - ${stats.accountCount} accounts`);
    console.log();
    
    rl.question('Are you sure you want to continue? (type "yes" to confirm): ', (answer) => {
      rl.close();
      resolve(answer.toLowerCase() === 'yes');
    });
  });
}

/**
 * Display help information
 */
function showHelp(): void {
  const enabledProviders = getEnabledProviders();
  
  console.log(`
🧹 Clear Specific Provider

Usage:
  ts-node scripts/clear-provider.ts <provider> [options]

Available Providers:
  postgres      PostgreSQL database
  mongodb       MongoDB database
  supabase      Supabase PostgreSQL
  cosmosdb      Azure CosmosDB
  sqlserver     Microsoft SQL Server
  ibmcloud      IBM Cloud DB2

Currently Enabled: ${enabledProviders.length > 0 ? enabledProviders.join(', ') : 'None'}

Options:
  --force, -f         Force clear without safety checks
  --yes, -y          Skip confirmation prompt
  --dry-run, -d      Show what would be cleared without actually clearing
  --help, -h         Show this help message

Environment Variables:
  ENABLE_<PROVIDER>=true           Must be true to clear the provider
  NODE_ENV=development|production  Environment detection
  FORCE_PRODUCTION_CLEAR=true      Required to clear production data
  FORCE_CLEAR=true                 Skip all confirmations
  CI=true                         Skip confirmations in CI environment

Safety Features:
  - Production environment protection
  - Confirmation prompts showing exact data to be deleted
  - Statistics display before clearing
  - Dry run mode to preview operations
  - Provider enablement validation

Examples:
  # Interactive clear with confirmation
  npm run seed:clear:provider postgres

  # Force clear without prompts
  npm run seed:clear:provider mongodb -- --force --yes

  # Dry run to see what would be cleared
  npm run seed:clear:provider supabase -- --dry-run

  # Production clear (requires FORCE_PRODUCTION_CLEAR=true)
  FORCE_PRODUCTION_CLEAR=true npm run seed:clear:provider sqlserver
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
    
    // Production safety check
    if (!options.force) {
      checkProductionSafety();
    }
    
    console.log(`🧹 Clearing mock data from ${provider}...`);
    
    // Get current statistics
    let stats: any;
    try {
      stats = await MockSeeder.getProviderStats(provider);
      console.log(`📊 Current Data Statistics:`);
      console.log(`   Users: ${stats.userCount}`);
      console.log(`   Accounts: ${stats.accountCount}`);
      console.log();
      
      if (stats.userCount === 0 && stats.accountCount === 0) {
        console.log(`✅ Provider ${provider} is already empty. Nothing to clear.`);
        return;
      }
    } catch (error) {
      console.warn('⚠️  Could not retrieve current statistics:', error);
      stats = { userCount: 'unknown', accountCount: 'unknown' };
    }
    
    if (options.dryRun) {
      console.log('🔍 DRY RUN MODE - No data will be deleted\n');
      console.log(`📋 Would clear all data from ${provider}:`);
      console.log(`   - ${stats.userCount} users`);
      console.log(`   - ${stats.accountCount} accounts`);
      console.log();
      console.log('✅ Dry run complete. Remove --dry-run to actually clear data.');
      return;
    }
    
    // Get confirmation unless skipped
    if (!options.skipConfirmation && !options.force) {
      const confirmed = await getConfirmation(provider, stats);
      if (!confirmed) {
        console.log('❌ Operation cancelled by user.');
        process.exit(0);
      }
    }
    
    // Perform clearing
    const startTime = Date.now();
    
    await MockSeeder.clearAll(provider);
    
    const endTime = Date.now();
    const duration = ((endTime - startTime) / 1000).toFixed(2);
    
    console.log(`✅ Successfully cleared ${provider} in ${duration}s`);
    
    // Display final statistics
    try {
      console.log();
      console.log('📊 Final Statistics:');
      const finalStats = await MockSeeder.getProviderStats(provider);
      console.log(`   Users: ${finalStats.userCount}`);
      console.log(`   Accounts: ${finalStats.accountCount}`);
      console.log(`   Database: ${provider}`);
      
      if (finalStats.userCount === 0 && finalStats.accountCount === 0) {
        console.log(`🎉 Provider ${provider} is now completely empty.`);
      } else {
        console.log(`⚠️  Some data may remain in ${provider}.`);
      }
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

export { main, validateProvider, getEnabledProviders, checkProductionSafety };
