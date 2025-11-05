#!/usr/bin/env node

/**
 * Nexpo Polyglot Backend Development Script
 * Starts enabled API backends based on feature flags in .env
 */

const fs = require('fs');
const path = require('path');
const { spawn } = require('child_process');
const dotenv = require('dotenv');

// Load environment variables
dotenv.config();

// Backend configurations
const BACKENDS = {
  typescript: {
    name: 'TypeScript Express',
    directory: 'microservices/api/api-typescript',
    port: 7000,
    command: 'pnpm',
    args: ['dev'],
    envFlag: 'ENABLE_API_TYPESCRIPT',
    color: '\x1b[36m' // Cyan
  },
  python: {
    name: 'Python FastAPI',
    directory: 'microservices/api/api-python',
    port: 7010,
    command: 'python',
    args: ['-m', 'uvicorn', 'main:app', '--reload', '--port', '7010'],
    envFlag: 'ENABLE_API_PYTHON',
    color: '\x1b[33m' // Yellow
  },
  go: {
    name: 'Go Beego',
    directory: 'microservices/api/api-go',
    port: 7020,
    command: 'go',
    args: ['run', 'main.go'],
    envFlag: 'ENABLE_API_GO',
    color: '\x1b[34m' // Blue
  },
  rust: {
    name: 'Rust Actix',
    directory: 'microservices/api/api-rust',
    port: 7030,
    command: 'cargo',
    args: ['run'],
    envFlag: 'ENABLE_API_RUST',
    color: '\x1b[31m' // Red
  },
  scala: {
    name: 'Scala Play',
    directory: 'microservices/api/api-scala',
    port: 7040,
    command: 'sbt',
    args: ['run'],
    envFlag: 'ENABLE_API_SCALA',
    color: '\x1b[35m' // Magenta
  },
  java: {
    name: 'Java Play',
    directory: 'microservices/api/api-java',
    port: 7050,
    command: 'sbt',
    args: ['run'],
    envFlag: 'ENABLE_API_JAVA',
    color: '\x1b[32m' // Green
  },
  r: {
    name: 'R Plumber',
    directory: 'microservices/api/api-R',
    port: 7060,
    command: 'Rscript',
    args: ['app.R'],
    envFlag: 'ENABLE_API_R',
    color: '\x1b[94m' // Light Blue
  },
  julia: {
    name: 'Julia Genie',
    directory: 'microservices/api/api-julia',
    port: 7070,
    command: 'julia',
    args: ['app.jl'],
    envFlag: 'ENABLE_API_JULIA',
    color: '\x1b[95m' // Light Magenta
  },
  php: {
    name: 'PHP Laravel',
    directory: 'microservices/api/api-php',
    port: 7080,
    command: 'php',
    args: ['artisan', 'serve', '--host=0.0.0.0', '--port=7080'],
    envFlag: 'ENABLE_API_PHP',
    color: '\x1b[96m' // Light Cyan
  },
  cpp: {
    name: 'C++ Drogon',
    directory: 'microservices/api/api-cpp',
    port: 7090,
    command: 'pnpm',
    args: ['run', process.platform === 'win32' ? 'start' : 'start:linux'],
    envFlag: 'ENABLE_API_CPP',
    color: '\x1b[91m' // Light Red
  },
  mindsdb: {
    name: 'MindsDB AI/ML',
    directory: '.',
    port: 47334,
    command: 'pnpm',
    args: ['start:mindsdb'],
    envFlag: 'ENABLE_MINDSDB',
    color: '\x1b[92m' // Bright Green
  }
};

const RESET_COLOR = '\x1b[0m';

class BackendManager {
  constructor() {
    this.processes = new Map();
    this.enabledBackends = [];
  }

  checkEnvironmentFlags() {
    console.log('🔍 Checking backend feature flags...\n');
    
    for (const [key, backend] of Object.entries(BACKENDS)) {
      const isEnabled = process.env[backend.envFlag] === 'true';
      
      if (isEnabled) {
        // Check if directory exists
        const dirPath = path.join(process.cwd(), backend.directory);
        if (fs.existsSync(dirPath)) {
          this.enabledBackends.push({ key, ...backend });
          console.log(`${backend.color}✅ ${backend.name}${RESET_COLOR} - Port ${backend.port}`);
        } else {
          console.log(`${backend.color}⚠️  ${backend.name}${RESET_COLOR} - Directory not found: ${backend.directory}`);
        }
      } else {
        console.log(`⏸️  ${backend.name} - Disabled`);
      }
    }

    if (this.enabledBackends.length === 0) {
      console.log('\n❌ No backends enabled. Update your .env file to enable backends.');
      console.log('Example: ENABLE_API_TYPESCRIPT=true');
      process.exit(1);
    }

    console.log(`\n🚀 Starting ${this.enabledBackends.length} enabled backend(s)...\n`);
  }

  async startBackend(backend) {
    return new Promise((resolve, reject) => {
      const { key, name, directory, command, args, port, color } = backend;
      
      console.log(`${color}🔧 Starting ${name}...${RESET_COLOR}`);
      
      const process = spawn(command, args, {
        cwd: path.join(process.cwd(), directory),
        stdio: 'pipe'
      });

      process.stdout.on('data', (data) => {
        const output = data.toString().trim();
        if (output) {
          console.log(`${color}[${name}]${RESET_COLOR} ${output}`);
        }
      });

      process.stderr.on('data', (data) => {
        const output = data.toString().trim();
        if (output) {
          console.log(`${color}[${name} ERROR]${RESET_COLOR} ${output}`);
        }
      });

      process.on('close', (code) => {
        console.log(`${color}[${name}]${RESET_COLOR} Process exited with code ${code}`);
        this.processes.delete(key);
      });

      process.on('error', (error) => {
        console.error(`${color}[${name} ERROR]${RESET_COLOR} Failed to start: ${error.message}`);
        reject(error);
      });

      this.processes.set(key, process);
      
      // Give process time to start
      setTimeout(() => {
        console.log(`${color}✅ ${name} started on port ${port}${RESET_COLOR}`);
        resolve();
      }, 2000);
    });
  }

  async startAllBackends() {
    const startPromises = this.enabledBackends.map(backend => 
      this.startBackend(backend).catch(error => {
        console.error(`Failed to start ${backend.name}:`, error.message);
      })
    );

    await Promise.allSettled(startPromises);
    
    console.log('\n🎉 All enabled backends started!');
    console.log('\n📊 Backend Status:');
    this.enabledBackends.forEach(backend => {
      console.log(`  • ${backend.color}${backend.name}${RESET_COLOR}: http://localhost:${backend.port}`);
    });
    
    console.log('\n💡 Press Ctrl+C to stop all backends\n');
  }

  setupGracefulShutdown() {
    const shutdown = () => {
      console.log('\n🛑 Shutting down backends...');
      
      for (const [key, process] of this.processes) {
        console.log(`Stopping ${key}...`);
        process.kill('SIGTERM');
      }
      
      setTimeout(() => {
        console.log('👋 All backends stopped');
        process.exit(0);
      }, 2000);
    };

    process.on('SIGINT', shutdown);
    process.on('SIGTERM', shutdown);
  }

  async run() {
    console.log('🏗️  Nexpo Polyglot Backend Manager\n');
    
    this.checkEnvironmentFlags();
    this.setupGracefulShutdown();
    await this.startAllBackends();
    
    // Keep process alive
    setInterval(() => {}, 1000);
  }
}

// Run the manager
if (require.main === module) {
  const manager = new BackendManager();
  manager.run().catch(error => {
    console.error('❌ Backend manager failed:', error);
    process.exit(1);
  });
}

module.exports = BackendManager;
