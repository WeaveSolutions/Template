#!/usr/bin/env node

/**
 * Test script to verify Svelte Web startup configuration
 */

const path = require('path');

// Load environment variables
try {
  require('dotenv').config({ path: path.join(__dirname, '..', '.env') });
} catch (error) {
  console.log('⚠️  dotenv not available, using process.env only');
}

console.log('='.repeat(60));
console.log('SVELTE WEB STARTUP DIAGNOSTIC');
console.log('='.repeat(60));
console.log('');

// Check flags
const ENABLE_TAURTE = process.env.ENABLE_TAURTE !== 'false';
const ENABLE_SVELTE_WEB = process.env.ENABLE_SVELTE_WEB !== 'false';
const TARGET_PLATFORM = process.env.TARGET_PLATFORM || 'all';

console.log('Environment Variables:');
console.log(`  ENABLE_TAURTE: ${process.env.ENABLE_TAURTE || '(not set, defaults to true)'} → ${ENABLE_TAURTE}`);
console.log(`  ENABLE_SVELTE_WEB: ${process.env.ENABLE_SVELTE_WEB || '(not set, defaults to true)'} → ${ENABLE_SVELTE_WEB}`);
console.log(`  TARGET_PLATFORM: ${process.env.TARGET_PLATFORM || '(not set, defaults to "all")'} → ${TARGET_PLATFORM}`);
console.log('');

// Check if Svelte Web should start
const shouldStartSvelte = (TARGET_PLATFORM === 'taurte' || TARGET_PLATFORM === 'all') 
                          && ENABLE_TAURTE 
                          && ENABLE_SVELTE_WEB;

console.log('Svelte Web Startup Check:');
console.log(`  Platform matches: ${TARGET_PLATFORM === 'taurte' || TARGET_PLATFORM === 'all'} (needs "taurte" or "all")`);
console.log(`  TAURTE enabled: ${ENABLE_TAURTE}`);
console.log(`  SVELTE_WEB enabled: ${ENABLE_SVELTE_WEB}`);
console.log('');

if (shouldStartSvelte) {
  console.log('✅ RESULT: Svelte Web SHOULD START with "pnpm dev"');
  console.log('   Port: 5173');
  console.log('   Command: pnpm dev');
  console.log('   Working directory: apps/taurte/svelteWeb');
} else {
  console.log('❌ RESULT: Svelte Web WILL NOT START with "pnpm dev"');
  console.log('');
  console.log('TO FIX:');
  if (!ENABLE_TAURTE) {
    console.log('  1. Set ENABLE_TAURTE=true in .env');
  }
  if (!ENABLE_SVELTE_WEB) {
    console.log('  2. Set ENABLE_SVELTE_WEB=true in .env');
  }
  if (TARGET_PLATFORM !== 'taurte' && TARGET_PLATFORM !== 'all') {
    console.log(`  3. Set TARGET_PLATFORM=all or TARGET_PLATFORM=taurte in .env (currently "${TARGET_PLATFORM}")`);
  }
}

console.log('');
console.log('='.repeat(60));
console.log('');

// Check Svelte Web directory
const svelteWebPath = path.join(__dirname, '..', 'apps', 'taurte', 'svelteWeb');
const fs = require('fs');

if (fs.existsSync(svelteWebPath)) {
  console.log('✅ Svelte Web directory exists');
  
  const packageJsonPath = path.join(svelteWebPath, 'package.json');
  if (fs.existsSync(packageJsonPath)) {
    console.log('✅ package.json exists');
    
    try {
      const packageJson = JSON.parse(fs.readFileSync(packageJsonPath, 'utf8'));
      if (packageJson.scripts && packageJson.scripts.dev) {
        console.log(`✅ "dev" script found: ${packageJson.scripts.dev}`);
      } else {
        console.log('❌ No "dev" script in package.json');
      }
    } catch (error) {
      console.log(`❌ Error reading package.json: ${error.message}`);
    }
  } else {
    console.log('❌ package.json not found');
  }
} else {
  console.log('❌ Svelte Web directory not found');
}

console.log('');
console.log('To manually start Svelte Web:');
console.log('  cd apps/taurte/svelteWeb');
console.log('  pnpm dev');
