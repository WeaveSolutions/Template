#!/usr/bin/env node

/**
 * Start development servers for enabled platforms based on feature flags
 * Reads from environment variables to determine which platforms to start
 */

const { spawn } = require('child_process');
const path = require('path');

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
const TARGET_PLATFORM = process.env.TARGET_PLATFORM || 'all';

class DevPlatformRunner {
  constructor() {
    this.processes = [];
  }

  log(message, prefix = 'DevRunner') {
    console.log(`[${prefix}] ${message}`);
  }

  error(message, prefix = 'DevRunner') {
    console.error(`[${prefix}] ERROR: ${message}`);
  }

  async startPlatform(name, script, cwd = null) {
    this.log(`Starting ${name}...`);
    
    try {
      const workingDir = cwd ? path.join(__dirname, '..', cwd) : path.join(__dirname, '..');
      
      const process = spawn('pnpm', ['run', script], {
        cwd: workingDir,
        stdio: 'inherit',
        shell: true
      });

      process.on('error', (error) => {
        this.error(`Failed to start ${name}: ${error.message}`);
      });

      process.on('close', (code) => {
        if (code !== 0) {
          this.error(`${name} exited with code ${code}`);
        }
      });

      this.processes.push({ name, process });
      
      // Small delay between starts
      await new Promise(resolve => setTimeout(resolve, 500));
      
    } catch (error) {
      this.error(`Error starting ${name}: ${error.message}`);
    }
  }

  async startDirectCommand(name, command, args, cwd) {
    this.log(`Starting ${name}...`);
    
    try {
      const workingDir = path.join(__dirname, '..', cwd);
      
      const process = spawn(command, args, {
        cwd: workingDir,
        stdio: 'inherit',
        shell: true
      });

      process.on('error', (error) => {
        this.error(`Failed to start ${name}: ${error.message}`);
      });

      process.on('close', (code) => {
        if (code !== 0) {
          this.error(`${name} exited with code ${code}`);
        }
      });

      this.processes.push({ name, process });
      
      // Small delay between starts
      await new Promise(resolve => setTimeout(resolve, 500));
      
    } catch (error) {
      this.error(`Error starting ${name}: ${error.message}`);
    }
  }

  async startEnabledPlatforms() {
    this.log('Starting development servers based on feature flags...');
    this.log(`Target platform: ${TARGET_PLATFORM}`);
    this.log(`Feature flags - NEXPO: ${ENABLE_NEXPO}, TAURTE: ${ENABLE_TAURTE}`);
    
    const platformsToStart = [];

    // Determine which platforms to start based on feature flags
    if (TARGET_PLATFORM === 'nexpo' || TARGET_PLATFORM === 'all') {
      if (ENABLE_NEXPO && ENABLE_NEXTJS_WEB) {
        platformsToStart.push({
          name: 'Next.js Web',
          type: 'direct',
          command: 'pnpm',
          args: ['dev'],
          cwd: 'apps/nexpo/nextWeb'
        });
      }
      
      if (ENABLE_NEXPO && ENABLE_EXPO_MOBILE) {
        platformsToStart.push({
          name: 'Expo Mobile',
          type: 'direct',
          command: 'pnpm',
          args: ['exec', 'expo', 'start'],
          cwd: 'apps/nexpo/expoMobile'
        });
      }
    }

    if (TARGET_PLATFORM === 'taurte' || TARGET_PLATFORM === 'all') {
      if (ENABLE_TAURTE && ENABLE_TAURI_DESKTOP) {
        platformsToStart.push({
          name: 'Tauri Desktop',
          type: 'direct',
          command: 'pnpm',
          args: ['run', 'dev'],
          cwd: 'apps/desktop'
        });
      }
      
      if (ENABLE_TAURTE && ENABLE_SVELTE_WEB) {
        platformsToStart.push({
          name: 'Svelte Web',
          type: 'direct',
          command: 'pnpm',
          args: ['dev'],
          cwd: 'apps/taurte/svelteWeb'
        });
      }
    }

    if (platformsToStart.length === 0) {
      this.log('No platforms enabled. Check your feature flags in .env file.');
      this.log('Available flags: ENABLE_NEXPO, ENABLE_TAURTE, ENABLE_NEXTJS_WEB, ENABLE_EXPO_MOBILE, ENABLE_TAURI_DESKTOP, ENABLE_SVELTE_WEB');
      this.log('Set TARGET_PLATFORM=nexpo|taurte|all to control which platforms to include.');
      return;
    }

    this.log(`Starting ${platformsToStart.length} platform(s):`);
    platformsToStart.forEach(p => this.log(`  - ${p.name}`));

    // Start all enabled platforms
    for (const platform of platformsToStart) {
      if (platform.type === 'direct') {
        await this.startDirectCommand(platform.name, platform.command, platform.args, platform.cwd);
      } else {
        await this.startPlatform(platform.name, platform.script, platform.cwd);
      }
    }

    this.log('All enabled platforms started successfully!');
  }

  async run() {
    try {
      await this.startEnabledPlatforms();
      
      // Handle graceful shutdown
      process.on('SIGINT', () => {
        this.log('Shutting down all platforms...');
        this.processes.forEach(({ name, process }) => {
          this.log(`Stopping ${name}...`);
          process.kill('SIGINT');
        });
        process.exit(0);
      });

      // Keep the process running
      process.stdin.resume();

    } catch (error) {
      this.error(`Failed to start platforms: ${error.message}`);
      process.exit(1);
    }
  }
}

// CLI interface
if (require.main === module) {
  const runner = new DevPlatformRunner();
  runner.run();
}

module.exports = DevPlatformRunner;
