#!/usr/bin/env node

/**
 * MindsDB Startup Script
 * Handles MindsDB Docker container initialization with proper environment configuration
 */

const { execSync, spawn } = require('child_process');
const path = require('path');
const fs = require('fs');

class MindsDBStarter {
  constructor() {
    this.projectRoot = path.resolve(__dirname, '..');
    this.mindsdbPath = path.join(this.projectRoot, 'mindsdb-main');
    this.envPath = path.join(this.projectRoot, '.env');
  }

  log(message, type = 'info') {
    const timestamp = new Date().toISOString();
    const colors = {
      info: '\x1b[36m',    // Cyan
      success: '\x1b[32m', // Green
      warning: '\x1b[33m', // Yellow
      error: '\x1b[31m',   // Red
      reset: '\x1b[0m'     // Reset
    };
    
    console.log(`${colors[type]}[${timestamp}] ${message}${colors.reset}`);
  }

  checkPrerequisites() {
    this.log('Checking prerequisites...', 'info');
    
    try {
      // Check if Docker is installed and running
      execSync('docker --version', { stdio: 'pipe' });
      execSync('docker-compose --version', { stdio: 'pipe' });
      this.log('✅ Docker and Docker Compose are available', 'success');
    } catch (error) {
      this.log('❌ Docker or Docker Compose not found. Please install Docker Desktop.', 'error');
      process.exit(1);
    }

    // Check if .env file exists
    if (!fs.existsSync(this.envPath)) {
      this.log('⚠️ .env file not found. Copying from .env.example...', 'warning');
      try {
        fs.copyFileSync(path.join(this.projectRoot, '.env.example'), this.envPath);
        this.log('✅ Created .env file from template', 'success');
      } catch (error) {
        this.log('❌ Failed to create .env file', 'error');
        process.exit(1);
      }
    }

    // Check if mindsdb-main directory exists
    if (!fs.existsSync(this.mindsdbPath)) {
      this.log('❌ mindsdb-main directory not found', 'error');
      process.exit(1);
    }

    this.log('✅ All prerequisites met', 'success');
  }

  async startMindsDB() {
    this.log('Starting MindsDB services...', 'info');
    
    try {
      // Change to mindsdb-main directory
      process.chdir(this.mindsdbPath);
      
      // Load environment variables
      require('dotenv').config({ path: this.envPath });
      
      // Pull latest MindsDB image
      this.log('Pulling latest MindsDB image...', 'info');
      execSync('docker-compose pull mindsdb', { stdio: 'inherit' });
      
      // Start services
      this.log('Starting MindsDB container...', 'info');
      execSync('docker-compose up -d mindsdb', { stdio: 'inherit' });
      
      // Wait for health check
      await this.waitForHealthCheck();
      
      this.log('🚀 MindsDB is ready!', 'success');
      this.printAccessInfo();
      
    } catch (error) {
      this.log(`❌ Failed to start MindsDB: ${error.message}`, 'error');
      process.exit(1);
    }
  }

  async waitForHealthCheck(maxRetries = 30, retryInterval = 2000) {
    this.log('Waiting for MindsDB to be ready...', 'info');
    
    const httpPort = process.env.MINDSDB_HTTP_PORT || '47334';
    
    for (let i = 0; i < maxRetries; i++) {
      try {
        const { execSync } = require('child_process');
        execSync(`curl -f http://localhost:${httpPort}/api/status`, { stdio: 'pipe' });
        this.log('✅ MindsDB health check passed', 'success');
        return;
      } catch (error) {
        this.log(`⏳ Health check attempt ${i + 1}/${maxRetries}...`, 'info');
        await new Promise(resolve => setTimeout(resolve, retryInterval));
      }
    }
    
    throw new Error('MindsDB failed to start within the timeout period');
  }

  printAccessInfo() {
    const httpPort = process.env.MINDSDB_HTTP_PORT || '47334';
    const mysqlPort = process.env.MINDSDB_MYSQL_PORT || '47335';
    const mongoPort = process.env.MINDSDB_MONGODB_PORT || '47336';
    
    console.log('\n' + '='.repeat(60));
    console.log('🎉 MindsDB is now running!');
    console.log('='.repeat(60));
    console.log(`📊 Web UI:      http://localhost:${httpPort}`);
    console.log(`🔌 HTTP API:    http://localhost:${httpPort}/api`);
    console.log(`🐬 MySQL API:   localhost:${mysqlPort}`);
    console.log(`🍃 MongoDB API: localhost:${mongoPort}`);
    console.log('='.repeat(60));
    console.log('\n💡 To stop MindsDB: pnpm run stop:mindsdb');
    console.log('💡 To view logs: pnpm run logs:mindsdb\n');
  }

  async stop() {
    this.log('Stopping MindsDB services...', 'info');
    try {
      process.chdir(this.mindsdbPath);
      execSync('docker-compose down', { stdio: 'inherit' });
      this.log('✅ MindsDB stopped successfully', 'success');
    } catch (error) {
      this.log(`❌ Failed to stop MindsDB: ${error.message}`, 'error');
      process.exit(1);
    }
  }

  async logs() {
    try {
      process.chdir(this.mindsdbPath);
      spawn('docker-compose', ['logs', '-f', 'mindsdb'], { stdio: 'inherit' });
    } catch (error) {
      this.log(`❌ Failed to show logs: ${error.message}`, 'error');
      process.exit(1);
    }
  }
}

// CLI Interface
async function main() {
  const starter = new MindsDBStarter();
  const command = process.argv[2] || 'start';

  switch (command) {
    case 'start':
      starter.checkPrerequisites();
      await starter.startMindsDB();
      break;
    case 'stop':
      await starter.stop();
      break;
    case 'logs':
      await starter.logs();
      break;
    case 'restart':
      await starter.stop();
      setTimeout(async () => {
        starter.checkPrerequisites();
        await starter.startMindsDB();
      }, 2000);
      break;
    default:
      console.log('Usage: node start-mindsdb.js [start|stop|logs|restart]');
      process.exit(1);
  }
}

if (require.main === module) {
  main().catch(error => {
    console.error('❌ Script failed:', error.message);
    process.exit(1);
  });
}

module.exports = MindsDBStarter;
