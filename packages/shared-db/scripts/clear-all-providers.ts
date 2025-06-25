#!/usr/bin/env ts-node

/**
 * Clear all mock data from enabled Prisma providers
 * Usage: ts-node scripts/clear-all-providers.ts [options]
 * 
 * Environment Variables:
 * - ENABLE_<PROVIDER>=true|false (determines which providers to clear)
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
function parseArgs(): ClearOptions {
  const args = process.argv.slice(2);
  
  return {
    force: args.includes('--force') || args.includes('-f') || process.env.FORCE_CLEAR === 'true',
    skipConfirmation: args.includes('--yes') || args.includes('-y') || process.env.CI === 'true',
    dryRun: args.includes('--dry-run') || args.includes('-d'),
  };
}

/**
 * Get user confirmation for dangerous operation
 */
async function getConfirmation(providers: string[]): Promise<boolean> {
  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
  });
  
  return new Promise((resolve) => {
    console.log(`⚠️  This will permanently delete ALL data from:`);
    providers.forEach(provider => console.log(`   - ${provider}`));
    console.log();
    
    rl.question('Are you sure you want to continue? (type "yes" to confirm): ', (answer) => {
      rl.close();
      resolve(answer.toLowerCase() === 'yes');
    });
  });
}

/**
 * Display current data statistics before clearing
 */
async function displayPreClearStats(providers: string[]): Promise<void> {
  console.log('📊 Current Data Statistics:');
  
  for (const provider of providers) {
    try {
      const stats = await MockSeeder.getStats(provider);
      console.log(`   ${provider}: ${stats.users} users, ${stats.accounts} accounts`);
    } catch (error) {
      console.log(`   ${provider}: Unable to retrieve stats (${error})`);
    }
  }
  
  console.log();
}

/**
 * Display help information
 */
function showHelp(): void {
  const enabledProviders = getEnabledProviders();
  
  console.log(`
🧹 Clear All Enabled Providers

Usage:
  ts-node scripts/clear-all-providers.ts [options]

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
  - Confirmation prompts for destructive operations
  - Statistics display before clearing
  - Dry run mode to preview operations

Examples:
  # Interactive clear with confirmation
  npm run seed:clear:all

  # Force clear without prompts  
  npm run seed:clear:all -- --force --yes

  # Dry run to see what would be cleared
  npm run seed:clear:all -- --dry-run

  # Production clear (requires FORCE_PRODUCTION_CLEAR=true)
  FORCE_PRODUCTION_CLEAR=true npm run seed:clear:all
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
      process.exit(1);
    }
    
    // Production safety check
    if (!options.force) {
      checkProductionSafety();
    }
    
    console.log(`🧹 Clearing mock data from enabled providers: ${enabledProviders.join(', ')}\n`);
    
    // Display current statistics
    try {
      await displayPreClearStats(enabledProviders);
    } catch (error) {
      console.warn('⚠️  Could not display pre-clear statistics:', error);
    }
    
    if (options.dryRun) {
      console.log('🔍 DRY RUN MODE - No data will be deleted\n');
      
      for (const provider of enabledProviders) {
        console.log(`📋 Would clear all data from ${provider}`);
      }
      
      console.log('\n✅ Dry run complete. Remove --dry-run to actually clear data.');
      return;
    }
    
    // Get confirmation unless skipped
    if (!options.skipConfirmation && !options.force) {
      const confirmed = await getConfirmation(enabledProviders);
      if (!confirmed) {
        console.log('❌ Operation cancelled by user.');
        process.exit(0);
      }
    }
    
    // Clear all enabled providers
    let successCount = 0;
    let errorCount = 0;
    const results: Array<{ provider: string; success: boolean; error?: Error }> = [];
    
    for (const provider of enabledProviders) {
      try {
        console.log(`🧹 Clearing ${provider}...`);
        
        await MockSeeder.clearAll(provider);
        
        successCount++;
        results.push({ provider, success: true });
        console.log(`✅ Successfully cleared ${provider}`);
        
      } catch (error) {
        errorCount++;
        results.push({ 
          provider, 
          success: false, 
          error: error instanceof Error ? error : new Error(String(error))
        });
        console.error(`❌ Failed to clear ${provider}:`, error);
        console.log(`   Continuing with next provider...`);
      }
    }
    
    // Summary
    console.log(`\n🎊 Clearing complete!`);
    console.log(`   ✅ Successful: ${successCount}/${enabledProviders.length} providers`);
    if (errorCount > 0) {
      console.log(`   ❌ Failed: ${errorCount}/${enabledProviders.length} providers`);
    }
    
    // Display errors
    const failedResults = results.filter(r => !r.success);
    if (failedResults.length > 0) {
      console.log('\n❌ Failed Operations:');
      failedResults.forEach(result => {
        console.log(`   ${result.provider}: ${result.error?.message || 'Unknown error'}`);
      });
    }
    
    // Display final statistics
    try {
      console.log('\n📊 Final Statistics:');
      for (const provider of enabledProviders) {
        try {
          const stats = await MockSeeder.getStats(provider);
          console.log(`   ${provider}: ${stats.users} users, ${stats.accounts} accounts, ${stats.deletedUsers} deleted`);
        } catch (error) {
          console.log(`   ${provider}: Unable to retrieve final stats (${error})`);
        }
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

export { main, getEnabledProviders, checkProductionSafety };
