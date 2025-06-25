/**
 * Dynamic Prisma Schema Generator
 * 
 * This utility helps manage multiple Prisma schemas for different cloud providers.
 * It selects the appropriate schema based on environment variables or configuration.
 */

import fs from 'fs';
import path from 'path';

const PROVIDERS_DIR = path.join(__dirname, 'providers');

// List of supported database providers
const DATABASE_PROVIDERS = {
  POSTGRES: 'postgres',
  MONGODB: 'mongodb', 
  SQLSERVER: 'sqlserver',
  COSMOSDB: 'cosmosdb',
  SUPABASE: 'supabase'
};

/**
 * Get active provider based on environment variables
 * Prioritizes in the order: explicitly set DB_PROVIDER, then AWS, GCP, Azure, OCI
 */
function getActiveProvider(): string {
  // Check for explicitly set provider
  const explicitProvider = process.env.DB_PROVIDER;
  if (explicitProvider && 
      Object.values(DATABASE_PROVIDERS).includes(explicitProvider.toLowerCase())) {
    return explicitProvider.toLowerCase();
  }
  
  // Check for active cloud provider
  if (process.env.ENABLE_AWS?.toLowerCase() === 'true') {
    return DATABASE_PROVIDERS.POSTGRES; // Default AWS provider
  }
  if (process.env.ENABLE_SUPABASE?.toLowerCase() === 'true') {
    return DATABASE_PROVIDERS.SUPABASE;
  }
  if (process.env.ENABLE_GCP?.toLowerCase() === 'true') {
    return DATABASE_PROVIDERS.MONGODB; // Default GCP provider
  }
  if (process.env.ENABLE_AZURE?.toLowerCase() === 'true') {
    return DATABASE_PROVIDERS.COSMOSDB; // Default Azure provider
  }
  if (process.env.ENABLE_OCI?.toLowerCase() === 'true') {
    return DATABASE_PROVIDERS.POSTGRES; // Default OCI provider
  }
  
  // Default to PostgreSQL if no provider specified
  return DATABASE_PROVIDERS.POSTGRES;
}

/**
 * Generate schema.prisma file based on active provider
 */
export function generateSchema(): string {
  const activeProvider = getActiveProvider();
  const schemaPath = path.join(PROVIDERS_DIR, `${activeProvider}.prisma`);
  
  if (!fs.existsSync(schemaPath)) {
    throw new Error(`Schema file not found for provider: ${activeProvider}`);
  }
  
  const schema = fs.readFileSync(schemaPath, 'utf-8');
  const outputPath = path.join(__dirname, 'schema.prisma');
  
  fs.writeFileSync(outputPath, schema);
  console.log(`Generated schema.prisma for provider: ${activeProvider}`);
  
  return outputPath;
}

// If this script is run directly, generate the schema
if (require.main === module) {
  try {
    generateSchema();
    console.log('Schema generation successful');
  } catch (error) {
    console.error('Schema generation failed:', error);
    process.exit(1);
  }
}
