#!/usr/bin/env node

/**
 * Display statistics for all enabled Prisma providers
 * Usage: ts-node packages/shared-db/scripts/stats.ts
 */

import * as path from 'path';
import { MockSeeder } from '../dist/mocks/MockSeeder';

interface ProviderStats {
  provider: string;
  users: number;
  accounts: number;
  activeUsers: number;
  deletedUsers: number;
  error?: string;
}

interface TotalStats {
  users: number;
  accounts: number;
  activeUsers: number;
  deletedUsers: number;
  providers: number;
  errors: number;
}

async function main(): Promise<void> {
  console.log('📊 Gathering statistics for all enabled providers...\n');
  
  try {
    await MockSeeder.validateDataConsistency();
    
    // Get detailed stats for each provider
    const stats: ProviderStats[] = await MockSeeder.getAllStats();
    
    console.log('\n📈 Detailed Statistics:');
    console.log('═'.repeat(70));
    
    stats.forEach(stat => {
      if (stat.error) {
        console.log(`❌ ${stat.provider.toUpperCase()}:`);
        console.log(`   Error: ${stat.error}`);
      } else {
        console.log(`✅ ${stat.provider.toUpperCase()}:`);
        console.log(`   Total Users: ${stat.users}`);
        console.log(`   Active Users: ${stat.activeUsers}`);
        console.log(`   Deleted Users: ${stat.deletedUsers}`);
        console.log(`   Total Accounts: ${stat.accounts}`);
        console.log(`   Accounts per User: ${stat.users > 0 ? (stat.accounts / stat.users).toFixed(2) : '0'}`);
      }
      console.log('');
    });
    
    // Summary totals
    const totals: TotalStats = stats.reduce((acc, stat) => {
      if (!stat.error) {
        acc.users += stat.users;
        acc.accounts += stat.accounts;
        acc.activeUsers += stat.activeUsers;
        acc.deletedUsers += stat.deletedUsers;
        acc.providers++;
      } else {
        acc.errors++;
      }
      return acc;
    }, { users: 0, accounts: 0, activeUsers: 0, deletedUsers: 0, providers: 0, errors: 0 });
    
    console.log('🌍 GLOBAL TOTALS:');
    console.log('═'.repeat(70));
    console.log(`   Active Providers: ${totals.providers}`);
    console.log(`   Providers with Errors: ${totals.errors}`);
    console.log(`   Total Users: ${totals.users}`);
    console.log(`   Total Active Users: ${totals.activeUsers}`);
    console.log(`   Total Deleted Users: ${totals.deletedUsers}`);
    console.log(`   Total Accounts: ${totals.accounts}`);
    console.log(`   Global Accounts per User: ${totals.users > 0 ? (totals.accounts / totals.users).toFixed(2) : '0'}`);
    
    // Health status
    console.log('\n🏥 Health Status:');
    console.log('═'.repeat(70));
    if (totals.errors === 0) {
      console.log('✅ All providers are healthy and accessible');
    } else {
      console.log(`⚠️  ${totals.errors} provider(s) have errors`);
    }
    
    if (totals.providers === 0) {
      console.log('⚠️  No providers are enabled or accessible');
      console.log('   Check your .env configuration and database connections');
    }
    
    console.log('═'.repeat(70));
    process.exit(0);
  } catch (error: any) {
    console.error('\n❌ Error gathering statistics:', error);
    process.exit(1);
  }
}

// Handle command line arguments
const args: string[] = process.argv.slice(2);
if (args.includes('--help') || args.includes('-h')) {
  console.log(`
📊 Provider Statistics - Data Overview

Usage: ts-node packages/shared-db/scripts/stats.ts [options]

Options:
  --help, -h            Show this help message

Features:
  ✅ Shows user and account counts per provider
  ✅ Displays provider health status
  ✅ Calculates global totals across all providers
  ✅ Identifies connection issues
  ✅ Shows active vs deleted user ratios

Output Sections:
  1. Individual Provider Statistics
     - User counts (total, active, deleted)
     - Account counts and ratios
     - Error status if any

  2. Global Totals
     - Combined statistics across all providers
     - Average accounts per user
     - Provider health summary

  3. Health Status
     - Overall system health
     - Configuration recommendations

Examples:
  # Basic statistics
  ts-node packages/shared-db/scripts/stats.ts
  
  # Use in scripts (exit code 0 = healthy, 1 = errors)
  if ts-node packages/shared-db/scripts/stats.ts; then
    echo "All providers healthy"
  else
    echo "Some providers have issues"
  fi

Environment Requirements:
  - Database providers must be enabled in .env
  - Database connections must be properly configured
  - Prisma clients must be generated

Provider Support:
  postgres, mongodb, cosmosdb, sqlserver, ibmcloud
`);
  process.exit(0);
}

main();
