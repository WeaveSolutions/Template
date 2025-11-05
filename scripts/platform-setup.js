#!/usr/bin/env node

/**
 * Platform-specific setup script for Nexpo monorepo
 * Handles conditional installation and configuration based on target platform
 */

const { execSync } = require('child_process');
const fs = require('fs');
const path = require('path');

const PLATFORMS = {
  NEXPO: 'nexpo',
  TAURTE: 'taurte', 
  ALL: 'all'
};

class PlatformSetup {
  constructor() {
    this.selectedPlatform = process.env.TARGET_PLATFORM || PLATFORMS.ALL;
    this.enableDev = process.env.NODE_ENV !== 'production';
  }

  log(message) {
    console.log(`[PlatformSetup] ${message}`);
  }

  error(message) {
    console.error(`[PlatformSetup] ERROR: ${message}`);
  }

  async setupWorkspaces() {
    this.log(`Setting up workspaces for platform: ${this.selectedPlatform}`);
    
    try {
      // Use our workspace-config.js to update package.json
      execSync(`node workspace-config.js --platform=${this.selectedPlatform}`, {
        stdio: 'inherit',
        cwd: path.join(__dirname, '..')
      });
      
      this.log('✅ Workspace configuration updated');
    } catch (error) {
      this.error(`Failed to update workspace configuration: ${error.message}`);
      throw error;
    }
  }

  async installDependencies() {
    this.log('Installing dependencies with pnpm...');
    
    try {
      // Clear any cached/stale data first
      execSync('pnpm store prune', { stdio: 'inherit' });
      
      // Install with workspace resolution
      execSync('pnpm install --frozen-lockfile=false', { 
        stdio: 'inherit',
        env: {
          ...process.env,
          PNPM_LINK_WORKSPACE_PACKAGES: 'true'
        }
      });
      
      this.log('✅ Dependencies installed successfully');
    } catch (error) {
      this.error(`Failed to install dependencies: ${error.message}`);
      throw error;
    }
  }

  async setupPlatformSpecific() {
    switch (this.selectedPlatform) {
      case PLATFORMS.NEXPO:
        await this.setupNexpo();
        break;
      case PLATFORMS.TAURTE:
        await this.setupTaurte();
        break;
      case PLATFORMS.ALL:
        await this.setupNexpo();
        await this.setupTaurte();
        break;
      default:
        this.error(`Unknown platform: ${this.selectedPlatform}`);
        return;
    }
  }

  async setupNexpo() {
    this.log('Setting up Nexpo platform...');
    
    // Add any Nexpo-specific setup here
    // e.g., Next.js configuration, Expo setup, etc.
    
    this.log('✅ Nexpo platform setup complete');
  }

  async setupTaurte() {
    this.log('Setting up Taurte platform...');
    
    // Add any Taurte-specific setup here  
    // e.g., Tauri configuration, Svelte setup, etc.
    
    this.log('✅ Taurte platform setup complete');
  }

  async run() {
    try {
      this.log(`Starting platform setup for: ${this.selectedPlatform}`);
      
      await this.setupWorkspaces();
      await this.installDependencies();
      await this.setupPlatformSpecific();
      
      this.log('🎉 Platform setup completed successfully!');
      
      // Display workspace info
      execSync('pnpm -r list --depth=0', { stdio: 'inherit' });
      
    } catch (error) {
      this.error(`Platform setup failed: ${error.message}`);
      process.exit(1);
    }
  }
}

// CLI interface
if (require.main === module) {
  const setup = new PlatformSetup();
  setup.run();
}

module.exports = PlatformSetup;
