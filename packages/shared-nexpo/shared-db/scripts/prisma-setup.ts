#!/usr/bin/env node

/**
 * Prisma Setup Script
 * 
 * This script handles Prisma schema generation and setup for multi-provider configurations.
 * It can be run during postinstall or manually for specific providers.
 */

import { execSync } from 'child_process';
import * as fs from 'fs';
import * as path from 'path';

function main(): void {
  console.log('🔧 Running Prisma setup...');
  
  try {
    // Get provider from command line argument or use default
    const provider = process.argv[2] || 'postgres';
    
    console.log(`📦 Setting up Prisma for provider: ${provider}`);
    
    // Check if schema file exists for the provider
    const schemaPath = path.join(__dirname, '..', 'prisma', `schema-${provider}.prisma`);
    
    if (fs.existsSync(schemaPath)) {
      console.log(`✅ Found schema file for ${provider}`);
      
      // Generate Prisma client for the specific provider
      const env = { ...process.env, PRISMA_SCHEMA_PATH: schemaPath };
      
      try {
        execSync('npx prisma generate', { 
          stdio: 'inherit', 
          env,
          cwd: path.join(__dirname, '..')
        });
        console.log(`✅ Prisma client generated successfully for ${provider}`);
      } catch (generateError: any) {
        console.warn(`⚠️  Warning: Could not generate Prisma client for ${provider}:`, generateError.message);
        console.log('This is normal if database connection is not available during install.');
      }
    } else {
      console.log(`ℹ️  No specific schema found for ${provider}, using default setup`);
      
      // Try to generate with default schema
      try {
        execSync('npx prisma generate', { 
          stdio: 'inherit',
          cwd: path.join(__dirname, '..')
        });
        console.log('✅ Default Prisma client generated successfully');
      } catch (generateError: any) {
        console.warn('⚠️  Warning: Could not generate default Prisma client:', generateError.message);
        console.log('This is normal if database connection is not available during install.');
      }
    }
    
    console.log('🎉 Prisma setup completed');
    
  } catch (error: any) {
    console.error('❌ Error during Prisma setup:', error.message);
    // Don't exit with error during install as this might be normal
    console.log('Continuing with installation...');
  }
}

if (require.main === module) {
  main();
}

export { main };
