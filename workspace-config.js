#!/usr/bin/env node

/**
 * Dynamic workspace configuration based on feature flags
 * Usage: node workspace-config.js [--platform=nexpo|taurte|all]
 */

const fs = require('fs');
const path = require('path');

// Feature flags - can be set via environment variables or CLI args
const ENABLE_NEXPO = process.env.ENABLE_NEXPO !== 'false';
const ENABLE_TAURTE = process.env.ENABLE_TAURTE !== 'false';
const ENABLE_MICROSERVICES = process.env.ENABLE_MICROSERVICES !== 'false';

// Parse command line arguments
const args = process.argv.slice(2);
const platformArg = args.find(arg => arg.startsWith('--platform='));
const selectedPlatform = platformArg ? platformArg.split('=')[1] : null;

// Base workspace configuration
const baseWorkspaces = [
  "apps/desktop", // Always include desktop as it's cross-platform
  "packages/shared-nexpo/shared-components",
  "packages/shared-nexpo/shared-db",
  "packages/shared-nexpo/shared-fonts",
  "packages/shared-nexpo/shared-hooks", 
  "packages/shared-nexpo/shared-pages",
  "packages/shared-nexpo/shared-provider",
  "packages/shared-nexpo/shared-ui",
  "packages/shared-nexpo/shared-utils"
];

// Platform-specific workspaces
const nexpoWorkspaces = [
  "apps/nexpo/nextWeb",
  "apps/nexpo/expoMobile"
];

const taurteWorkspaces = [
  "apps/taurte/svelteWeb", 
  "apps/taurte/tauriMobile",
  "packages/shared-taurte"
];

const microserviceWorkspaces = [
  "microservices/api/api-typescript"
  // Excluded: api-scala, api-java, api-R, api-julia, api-cpp (require additional language runtimes)
];

// Generate workspaces based on feature flags
function generateWorkspaces() {
  let workspaces = [...baseWorkspaces];
  
  // Handle platform-specific selection
  if (selectedPlatform === 'nexpo') {
    workspaces.push(...nexpoWorkspaces);
  } else if (selectedPlatform === 'taurte') {
    workspaces.push(...taurteWorkspaces);
  } else if (selectedPlatform === 'all' || !selectedPlatform) {
    // Include based on feature flags
    if (ENABLE_NEXPO) {
      workspaces.push(...nexpoWorkspaces);
    }
    
    if (ENABLE_TAURTE) {
      workspaces.push(...taurteWorkspaces);
    }
  }
  
  // Add microservices if enabled
  if (ENABLE_MICROSERVICES && (selectedPlatform === 'all' || !selectedPlatform)) {
    workspaces.push(...microserviceWorkspaces);
  }
  
  return workspaces;
}

// Update package.json with generated workspaces
function updatePackageJson() {
  const packageJsonPath = path.join(__dirname, 'package.json');
  const packageJson = JSON.parse(fs.readFileSync(packageJsonPath, 'utf8'));
  
  packageJson.workspaces = generateWorkspaces();
  
  fs.writeFileSync(packageJsonPath, JSON.stringify(packageJson, null, 2));
  
  console.log(`✅ Updated workspace configuration:`);
  console.log(`   Platform: ${selectedPlatform || 'all'}`);
  console.log(`   Nexpo enabled: ${ENABLE_NEXPO}`);
  console.log(`   Taurte enabled: ${ENABLE_TAURTE}`);
  console.log(`   Microservices enabled: ${ENABLE_MICROSERVICES}`);
  console.log(`   Total workspaces: ${packageJson.workspaces.length}`);
}

// CLI interface
if (require.main === module) {
  updatePackageJson();
}

module.exports = {
  generateWorkspaces,
  updatePackageJson
};
