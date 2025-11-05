#!/usr/bin/env ts-node

/**
 * Test all enabled database providers
 * Usage: ts-node scripts/test-all-providers.ts [options]
 * 
 * Options:
 *   --dry-run    Show which providers would be tested without running tests
 *   --continue   Continue testing other providers even if one fails
 *   --verbose    Show detailed output from each test
 *   --help       Show this help message
 * 
 * Environment Variables:
 * - ENABLE_POSTGRESQL=true
 * - ENABLE_MONGODB=true
 * - ENABLE_SUPABASE=true
 * - ENABLE_COSMOSDB=true
 * - ENABLE_SQLSERVER=true
 * - ENABLE_IBM=true
 */

import dotenv from 'dotenv';
import { spawn } from 'child_process';
import path from 'path';

dotenv.config();

interface TestResult {
  provider: string;
  success: boolean;
  duration: number;
  error?: string;
}

interface ProviderConfig {
  name: string;
  enabled: boolean;
  envVar: string;
  scriptPath: string;
  description: string;
}

/**
 * Get all available providers and their configuration
 */
function getProviderConfigs(): ProviderConfig[] {
  const scriptsDir = __dirname;
  
  return [
    {
      name: 'postgres',
      enabled: process.env.ENABLE_POSTGRESQL === 'true',
      envVar: 'ENABLE_POSTGRESQL',
      scriptPath: path.join(scriptsDir, 'test-postgres.ts'),
      description: 'PostgreSQL database'
    },
    {
      name: 'mongodb',
      enabled: process.env.ENABLE_MONGODB === 'true',
      envVar: 'ENABLE_MONGODB',
      scriptPath: path.join(scriptsDir, 'test-mongodb.ts'),
      description: 'MongoDB database'
    },
    {
      name: 'supabase',
      enabled: process.env.ENABLE_SUPABASE === 'true',
      envVar: 'ENABLE_SUPABASE',
      scriptPath: path.join(scriptsDir, 'test-supabase.ts'),
      description: 'Supabase (PostgreSQL)'
    },
    {
      name: 'cosmosdb',
      enabled: process.env.ENABLE_COSMOSDB === 'true',
      envVar: 'ENABLE_COSMOSDB',
      scriptPath: path.join(scriptsDir, 'test-cosmosdb.ts'),
      description: 'Azure CosmosDB (MongoDB API)'
    },
    {
      name: 'sqlserver',
      enabled: process.env.ENABLE_SQLSERVER === 'true',
      envVar: 'ENABLE_SQLSERVER',
      scriptPath: path.join(scriptsDir, 'test-sqlserver.ts'),
      description: 'Microsoft SQL Server'
    },
    {
      name: 'ibmcloud',
      enabled: process.env.ENABLE_IBM === 'true',
      envVar: 'ENABLE_IBM',
      scriptPath: path.join(scriptsDir, 'test-ibmcloud-provider.ts'),
      description: 'IBM Cloud DB2'
    }
  ];
}

/**
 * Parse command line arguments
 */
function parseArgs(): { dryRun: boolean; continueOnError: boolean; verbose: boolean; help: boolean } {
  const args = process.argv.slice(2);
  
  return {
    dryRun: args.includes('--dry-run'),
    continueOnError: args.includes('--continue'),
    verbose: args.includes('--verbose'),
    help: args.includes('--help') || args.includes('-h')
  };
}

/**
 * Show help message
 */
function showHelp(): void {
  console.log(`
🧪 Database Provider Test Suite

USAGE:
  ts-node scripts/test-all-providers.ts [options]

OPTIONS:
  --dry-run    Show which providers would be tested without running tests
  --continue   Continue testing other providers even if one fails
  --verbose    Show detailed output from each test
  --help       Show this help message

ENVIRONMENT VARIABLES:
  Set these to 'true' to enable testing for each provider:
  - ENABLE_POSTGRESQL
  - ENABLE_MONGODB  
  - ENABLE_SUPABASE
  - ENABLE_COSMOSDB
  - ENABLE_SQLSERVER
  - ENABLE_IBM

EXAMPLES:
  # Test all enabled providers
  ts-node scripts/test-all-providers.ts

  # Preview which tests would run
  ts-node scripts/test-all-providers.ts --dry-run

  # Continue testing even if one provider fails
  ts-node scripts/test-all-providers.ts --continue

  # Show verbose output
  ts-node scripts/test-all-providers.ts --verbose

INDIVIDUAL PROVIDER TESTS:
  ts-node scripts/test-postgres.ts
  ts-node scripts/test-mongodb.ts
  ts-node scripts/test-supabase.ts
  ts-node scripts/test-cosmosdb.ts
  ts-node scripts/test-sqlserver.ts
  ts-node scripts/test-ibmcloud-provider.ts
`);
}

/**
 * Run a single provider test
 */
async function runProviderTest(
  provider: ProviderConfig, 
  verbose: boolean
): Promise<TestResult> {
  return new Promise((resolve) => {
    const startTime = Date.now();
    
    console.log(`🧪 Testing ${provider.name} (${provider.description})...`);
    
    const child = spawn('ts-node', [provider.scriptPath], {
      stdio: verbose ? 'inherit' : 'pipe',
      env: process.env
    });
    
    let output = '';
    let errorOutput = '';
    
    if (!verbose) {
      child.stdout?.on('data', (data) => {
        output += data.toString();
      });
      
      child.stderr?.on('data', (data) => {
        errorOutput += data.toString();
      });
    }
    
    child.on('close', (code) => {
      const duration = Date.now() - startTime;
      const success = code === 0;
      
      if (success) {
        console.log(`✅ ${provider.name} test passed (${duration}ms)`);
      } else {
        console.log(`❌ ${provider.name} test failed (${duration}ms)`);
        if (!verbose && errorOutput) {
          console.log(`   Error: ${errorOutput.trim().split('\n')[0]}`);
        }
      }
      
      resolve({
        provider: provider.name,
        success,
        duration,
        error: success ? undefined : errorOutput || 'Test failed with unknown error'
      });
    });
    
    child.on('error', (error) => {
      const duration = Date.now() - startTime;
      console.log(`❌ ${provider.name} test failed to start (${duration}ms)`);
      console.log(`   Error: ${error.message}`);
      
      resolve({
        provider: provider.name,
        success: false,
        duration,
        error: error.message
      });
    });
  });
}

/**
 * Show dry run information
 */
function showDryRun(providers: ProviderConfig[]): void {
  console.log('🔍 Dry Run: Provider Test Plan\n');
  
  const enabledProviders = providers.filter(p => p.enabled);
  const disabledProviders = providers.filter(p => !p.enabled);
  
  if (enabledProviders.length > 0) {
    console.log(`✅ Enabled Providers (${enabledProviders.length}):`);
    enabledProviders.forEach(provider => {
      console.log(`   - ${provider.name.padEnd(12)} ${provider.description}`);
    });
  }
  
  if (disabledProviders.length > 0) {
    console.log(`\n⚠️  Disabled Providers (${disabledProviders.length}):`);
    disabledProviders.forEach(provider => {
      console.log(`   - ${provider.name.padEnd(12)} ${provider.description} (${provider.envVar}=false)`);
    });
  }
  
  if (enabledProviders.length === 0) {
    console.log('❌ No providers are enabled for testing.');
    console.log('\nTo enable providers, set environment variables:');
    providers.forEach(provider => {
      console.log(`   ${provider.envVar}=true`);
    });
  } else {
    console.log(`\n📋 Test execution order:`);
    enabledProviders.forEach((provider, index) => {
      console.log(`   ${index + 1}. ${provider.name}`);
    });
  }
}

/**
 * Show test results summary
 */
function showSummary(results: TestResult[]): void {
  console.log('\n📋 Test Results Summary');
  console.log('========================\n');
  
  const successful = results.filter(r => r.success);
  const failed = results.filter(r => !r.success);
  const totalDuration = results.reduce((sum, r) => sum + r.duration, 0);
  
  // Show individual results
  results.forEach(result => {
    const status = result.success ? '✅' : '❌';
    const duration = `${result.duration}ms`;
    console.log(`${status} ${result.provider.padEnd(12)} ${duration.padStart(8)}`);
  });
  
  // Show overall statistics
  console.log('\n📊 Statistics:');
  console.log(`   Total tests: ${results.length}`);
  console.log(`   Successful: ${successful.length}`);
  console.log(`   Failed: ${failed.length}`);
  console.log(`   Total duration: ${totalDuration}ms`);
  console.log(`   Average duration: ${Math.round(totalDuration / results.length)}ms`);
  
  // Show failed tests details
  if (failed.length > 0) {
    console.log('\n❌ Failed Tests:');
    failed.forEach(result => {
      console.log(`   ${result.provider}:`);
      if (result.error) {
        const errorLines = result.error.trim().split('\n');
        errorLines.slice(0, 3).forEach(line => {
          console.log(`     ${line}`);
        });
        if (errorLines.length > 3) {
          console.log(`     ... (${errorLines.length - 3} more lines)`);
        }
      }
    });
  }
  
  // Show recommendations
  if (failed.length > 0) {
    console.log('\n💡 Recommendations:');
    console.log('   - Check database connection strings in .env');
    console.log('   - Ensure database servers are running');
    console.log('   - Verify Prisma schemas are generated');
    console.log('   - Run individual tests for detailed error messages');
  }
}

/**
 * Main test execution function
 */
async function main(): Promise<void> {
  const args = parseArgs();
  
  if (args.help) {
    showHelp();
    return;
  }
  
  console.log('🧪 Database Provider Test Suite\n');
  
  const providers = getProviderConfigs();
  const enabledProviders = providers.filter(p => p.enabled);
  
  if (args.dryRun) {
    showDryRun(providers);
    return;
  }
  
  if (enabledProviders.length === 0) {
    console.log('❌ No providers are enabled for testing.');
    console.log('\nTo enable providers, set environment variables:');
    providers.forEach(provider => {
      console.log(`   ${provider.envVar}=true`);
    });
    process.exit(1);
  }
  
  console.log(`🚀 Running tests for ${enabledProviders.length} enabled providers...\n`);
  
  const results: TestResult[] = [];
  
  for (const provider of enabledProviders) {
    try {
      const result = await runProviderTest(provider, args.verbose);
      results.push(result);
      
      if (!result.success && !args.continueOnError) {
        console.log(`\n💥 Stopping due to test failure. Use --continue to test remaining providers.`);
        break;
      }
      
      // Add spacing between tests
      if (enabledProviders.indexOf(provider) < enabledProviders.length - 1) {
        console.log('');
      }
      
    } catch (error) {
      const result: TestResult = {
        provider: provider.name,
        success: false,
        duration: 0,
        error: error instanceof Error ? error.message : 'Unknown error'
      };
      results.push(result);
      
      if (!args.continueOnError) {
        console.log(`\n💥 Stopping due to unexpected error. Use --continue to test remaining providers.`);
        break;
      }
    }
  }
  
  showSummary(results);
  
  const failedCount = results.filter(r => !r.success).length;
  if (failedCount > 0) {
    console.log(`\n❌ ${failedCount} test(s) failed`);
    process.exit(1);
  } else {
    console.log('\n🎉 All tests passed successfully!');
  }
}

// Execute main function
if (require.main === module) {
  main().catch((error) => {
    console.error('💥 Unhandled error in test runner:', error);
    process.exit(1);
  });
}

export { main, getProviderConfigs, runProviderTest, showDryRun, showSummary };
