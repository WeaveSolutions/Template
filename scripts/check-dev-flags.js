#!/usr/bin/env node

/**
 * Check current dev platform flags
 */

const path = require('path');

// Load environment variables
try {
  require('dotenv').config({ path: path.join(__dirname, '..', '.env') });
} catch (error) {
  // dotenv not available
}

const flags = {
  ENABLE_NEXPO: process.env.ENABLE_NEXPO || 'not set (defaults to true)',
  ENABLE_TAURTE: process.env.ENABLE_TAURTE || 'not set (defaults to true)',
  ENABLE_NEXTJS_WEB: process.env.ENABLE_NEXTJS_WEB || 'not set (defaults to true)',
  ENABLE_EXPO_MOBILE: process.env.ENABLE_EXPO_MOBILE || 'not set (defaults to true)',
  ENABLE_TAURI_DESKTOP: process.env.ENABLE_TAURI_DESKTOP || 'not set (defaults to true)',
  ENABLE_SVELTE_WEB: process.env.ENABLE_SVELTE_WEB || 'not set (defaults to true)',
  TARGET_PLATFORM: process.env.TARGET_PLATFORM || 'not set (defaults to "all")'
};

console.log('Current Dev Platform Flags:');
console.log('==========================================');
Object.entries(flags).forEach(([key, value]) => {
  const isDisabled = value === 'false';
  const symbol = isDisabled ? '❌' : '✅';
  console.log(`${symbol} ${key}: ${value}`);
});
console.log('==========================================');
console.log('\nPlatforms that will start with "pnpm dev":');

const ENABLE_TAURTE = process.env.ENABLE_TAURTE !== 'false';
const ENABLE_SVELTE_WEB = process.env.ENABLE_SVELTE_WEB !== 'false';
const TARGET_PLATFORM = process.env.TARGET_PLATFORM || 'all';

if ((TARGET_PLATFORM === 'taurte' || TARGET_PLATFORM === 'all') && ENABLE_TAURTE && ENABLE_SVELTE_WEB) {
  console.log('✅ Svelte Web (port 5173) - WILL START');
} else {
  console.log('❌ Svelte Web (port 5173) - WILL NOT START');
  if (!ENABLE_TAURTE) console.log('   Reason: ENABLE_TAURTE is false');
  if (!ENABLE_SVELTE_WEB) console.log('   Reason: ENABLE_SVELTE_WEB is false');
  if (TARGET_PLATFORM !== 'taurte' && TARGET_PLATFORM !== 'all') {
    console.log(`   Reason: TARGET_PLATFORM is "${TARGET_PLATFORM}" (needs "taurte" or "all")`);
  }
}

const ENABLE_NEXPO = process.env.ENABLE_NEXPO !== 'false';
const ENABLE_NEXTJS_WEB = process.env.ENABLE_NEXTJS_WEB !== 'false';

if ((TARGET_PLATFORM === 'nexpo' || TARGET_PLATFORM === 'all') && ENABLE_NEXPO && ENABLE_NEXTJS_WEB) {
  console.log('✅ Next.js Web (port 3000) - WILL START');
} else {
  console.log('❌ Next.js Web (port 3000) - WILL NOT START');
}

const ENABLE_TAURI_DESKTOP = process.env.ENABLE_TAURI_DESKTOP !== 'false';

if ((TARGET_PLATFORM === 'taurte' || TARGET_PLATFORM === 'all') && ENABLE_TAURTE && ENABLE_TAURI_DESKTOP) {
  console.log('✅ Tauri Desktop (port 1420) - WILL START');
} else {
  console.log('❌ Tauri Desktop (port 1420) - WILL NOT START');
}

console.log('\n💡 To enable/disable platforms, edit your .env file');
