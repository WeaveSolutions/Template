#!/usr/bin/env node

/**
 * Build all enabled platforms based on feature flags
 * Reads from environment variables to determine which platforms to build
 */

const { spawn } = require('child_process');
const path = require('path');
const fs = require('fs');

// Load environment variables from .env if it exists
try {
  require('dotenv').config({ path: path.join(__dirname, '..', '.env') });
} catch (error) {
  // dotenv not available, continue with process.env
}

// Feature flags from environment with defaults
const ENABLE_NEXPO = process.env.ENABLE_NEXPO !== 'false';
const ENABLE_TAURTE = process.env.ENABLE_TAURTE !== 'false';
const ENABLE_NEXTJS_WEB = process.env.ENABLE_NEXTJS_WEB !== 'false';
const ENABLE_EXPO_MOBILE = process.env.ENABLE_EXPO_MOBILE !== 'false';
const ENABLE_TAURI_DESKTOP = process.env.ENABLE_TAURI_DESKTOP !== 'false';
const ENABLE_SVELTE_WEB = process.env.ENABLE_SVELTE_WEB !== 'false';
const ENABLE_MICROSERVICES = process.env.ENABLE_MICROSERVICES !== 'false';
const TARGET_PLATFORM = process.env.TARGET_PLATFORM || 'all';

class BuildPlatformRunner {
  constructor() {
    this.buildResults = [];
  }

  log(message, prefix = 'BuildRunner') {
    console.log(`[${prefix}] ${message}`);
  }

  error(message, prefix = 'BuildRunner') {
    console.error(`[${prefix}] ERROR: ${message}`);
  }

  success(message, prefix = 'BuildRunner') {
    console.log(`[${prefix}] ✅ ${message}`);
  }

  async runCommand(name, command, args, cwd) {
    return new Promise((resolve, reject) => {
      this.log(`Building ${name}...`);
      
      const workingDir = path.join(__dirname, '..', cwd);
      
      // Check if directory exists
      if (!fs.existsSync(workingDir)) {
        this.error(`Directory does not exist: ${workingDir}`);
        resolve({ success: false, error: 'Directory not found' });
        return;
      }

      const process = spawn(command, args, {
        cwd: workingDir,
        stdio: 'inherit',
        shell: true
      });

      process.on('error', (error) => {
        this.error(`Failed to build ${name}: ${error.message}`);
        resolve({ success: false, error: error.message });
      });

      process.on('close', (code) => {
        if (code === 0) {
          this.success(`${name} built successfully`);
          resolve({ success: true });
        } else {
          this.error(`${name} build failed with exit code ${code}`);
          resolve({ success: false, error: `Exit code ${code}` });
        }
      });
    });
  }

  async buildEnabledPlatforms() {
    this.log('Building platforms based on feature flags...');
    this.log(`Target platform: ${TARGET_PLATFORM}`);
    this.log(`Feature flags - NEXPO: ${ENABLE_NEXPO}, TAURTE: ${ENABLE_TAURTE}, MICROSERVICES: ${ENABLE_MICROSERVICES}`);
    
    const buildTasks = [];

    // Determine which platforms to build based on feature flags
    if (TARGET_PLATFORM === 'nexpo' || TARGET_PLATFORM === 'all') {
      if (ENABLE_NEXPO && ENABLE_NEXTJS_WEB) {
        buildTasks.push({
          name: 'Next.js Web',
          command: 'pnpm',
          args: ['build'],
          cwd: 'apps/nexpo/nextWeb'
        });
      }
      
      if (ENABLE_NEXPO && ENABLE_EXPO_MOBILE) {
        buildTasks.push({
          name: 'Expo Mobile',
          command: 'npx',
          args: ['expo', 'build', '--no-wait'],
          cwd: 'apps/nexpo/expoMobile'
        });
      }
    }

    if (TARGET_PLATFORM === 'taurte' || TARGET_PLATFORM === 'all') {
      if (ENABLE_TAURTE && ENABLE_TAURI_DESKTOP) {
        buildTasks.push({
          name: 'Tauri Desktop',
          command: 'pnpm',
          args: ['run', 'build'],
          cwd: 'apps/desktop'
        });
      }
      
      if (ENABLE_TAURTE && ENABLE_SVELTE_WEB) {
        buildTasks.push({
          name: 'Svelte Web',
          command: 'pnpm',
          args: ['build'],
          cwd: 'apps/taurte/svelteWeb'
        });
      }
    }

    // Always build microservices if enabled
    if (ENABLE_MICROSERVICES) {
      buildTasks.push({
        name: 'API Services',
        command: 'pnpm',
        args: ['build'],
        cwd: 'microservices/api/api-typescript'
      });
    }

    if (buildTasks.length === 0) {
      this.log('No platforms enabled for building. Check your feature flags in .env file.');
      this.log('Available flags: ENABLE_NEXPO, ENABLE_TAURTE, ENABLE_NEXTJS_WEB, ENABLE_EXPO_MOBILE, ENABLE_TAURI_DESKTOP, ENABLE_SVELTE_WEB, ENABLE_MICROSERVICES');
      this.log('Set TARGET_PLATFORM=nexpo|taurte|all to control which platforms to include.');
      return { success: true, skipped: true };
    }

    this.log(`Building ${buildTasks.length} platform(s):`);
    buildTasks.forEach(task => this.log(`  - ${task.name}`));

    // Build all platforms sequentially for better resource management
    let successCount = 0;
    let failureCount = 0;

    for (const task of buildTasks) {
      const result = await this.runCommand(task.name, task.command, task.args, task.cwd);
      this.buildResults.push({ name: task.name, ...result });
      
      if (result.success) {
        successCount++;
      } else {
        failureCount++;
      }
    }

    // Summary
    this.log('\n' + '='.repeat(50));
    this.log(`BUILD SUMMARY`);
    this.log('='.repeat(50));
    this.log(`Total builds: ${buildTasks.length}`);
    this.log(`Successful: ${successCount}`);
    this.log(`Failed: ${failureCount}`);

    if (failureCount > 0) {
      this.log('\nFailed builds:');
      this.buildResults
        .filter(result => !result.success)
        .forEach(result => {
          this.error(`${result.name}: ${result.error}`);
        });
    }

    return { 
      success: failureCount === 0, 
      totalBuilds: buildTasks.length,
      successCount,
      failureCount 
    };
  }

  async run() {
    try {
      const startTime = Date.now();
      const result = await this.buildEnabledPlatforms();
      const duration = Math.round((Date.now() - startTime) / 1000);

      if (result.skipped) {
        this.log('Build skipped - no platforms enabled.');
        process.exit(0);
      }

      if (result.success) {
        this.success(`All platforms built successfully in ${duration}s!`);
        process.exit(0);
      } else {
        this.error(`Build completed with errors in ${duration}s. ${result.failureCount}/${result.totalBuilds} builds failed.`);
        process.exit(1);
      }

    } catch (error) {
      this.error(`Build process failed: ${error.message}`);
      process.exit(1);
    }
  }

  // Helper method to build specific platform
  async buildSpecificPlatform(platform) {
    switch (platform.toLowerCase()) {
      case 'next':
      case 'nextjs':
        return await this.runCommand('Next.js Web', 'pnpm', ['build'], 'apps/nexpo/nextWeb');
      
      case 'expo':
      case 'mobile':
        return await this.runCommand('Expo Mobile', 'npx', ['expo', 'build', '--no-wait'], 'apps/nexpo/expoMobile');
      
      case 'tauri':
      case 'desktop':
        return await this.runCommand('Tauri Desktop', 'pnpm', ['run', 'build'], 'apps/desktop');
      
      case 'svelte':
      case 'svelte-web':
        return await this.runCommand('Svelte Web', 'pnpm', ['build'], 'apps/taurte/svelteWeb');
      
      case 'api':
      case 'microservices':
        return await this.runCommand('API Services', 'pnpm', ['build'], 'microservices/api/api-typescript');
      
      default:
        throw new Error(`Unknown platform: ${platform}`);
    }
  }
}

// CLI interface
if (require.main === module) {
  const runner = new BuildPlatformRunner();
  
  // Check if specific platform build was requested
  const args = process.argv.slice(2);
  if (args.length > 0 && args[0] === '--platform') {
    const platform = args[1];
    if (!platform) {
      console.error('Please specify a platform after --platform flag');
      process.exit(1);
    }
    
    runner.buildSpecificPlatform(platform)
      .then(result => {
        if (result.success) {
          process.exit(0);
        } else {
          process.exit(1);
        }
      })
      .catch(error => {
        console.error(`Build failed: ${error.message}`);
        process.exit(1);
      });
  } else {
    runner.run();
  }
}

module.exports = BuildPlatformRunner;
