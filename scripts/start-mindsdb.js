#!/usr/bin/env node

/**
 * MindsDB Startup Script for Nexpo
 * Starts MindsDB server for AI/ML integration with backend APIs
 */

const { spawn, exec } = require('child_process');
const path = require('path');
const fs = require('fs');
const os = require('os');
const yaml = require('js-yaml');

// Configuration
const MINDSDB_CONFIG = {
  port: process.env.MINDSDB_PORT || 47334,
  host: process.env.MINDSDB_HOST || '127.0.0.1',
  httpPort: process.env.MINDSDB_HTTP_PORT || 47335,
  mysqlPort: process.env.MINDSDB_MYSQL_PORT || 47336,
  mongoPort: process.env.MINDSDB_MONGO_PORT || 47337
};

console.log('🧠 Starting MindsDB for Nexpo...');
console.log('Configuration:', MINDSDB_CONFIG);

/**
 * Check if MindsDB is installed
 */
function checkMindsDBInstallation() {
  return new Promise((resolve, reject) => {
    exec('pip show mindsdb', (error, stdout, stderr) => {
      if (error) {
        console.log('❌ MindsDB not found. Installing...');
        installMindsDB().then(resolve).catch(reject);
      } else {
        console.log('✅ MindsDB is installed');
        resolve();
      }
    });
  });
}

/**
 * Install MindsDB via pip
 */
function installMindsDB() {
  return new Promise((resolve, reject) => {
    console.log('📦 Installing MindsDB...');
    const installProcess = spawn('pip', ['install', 'mindsdb'], {
      stdio: 'inherit',
      shell: true
    });

    installProcess.on('close', (code) => {
      if (code === 0) {
        console.log('✅ MindsDB installed successfully');
        resolve();
      } else {
        console.error('❌ Failed to install MindsDB');
        reject(new Error(`Installation failed with code ${code}`));
      }
    });
  });
}

/**
 * Start MindsDB server
 */
function startMindsDB() {
  console.log('🚀 Starting MindsDB server...');
  
  const configObject = {
    api: {
      http: {
        host: MINDSDB_CONFIG.host,
        port: MINDSDB_CONFIG.httpPort,
      },
      mysql: {
        host: MINDSDB_CONFIG.host,
        port: MINDSDB_CONFIG.mysqlPort,
      },
    },
  };

  const tmpDir = path.join(os.tmpdir(), 'mindsdb-nexpo');
  try { fs.mkdirSync(tmpDir, { recursive: true }); } catch {}
  const configPath = path.join(tmpDir, `config-${process.pid}.json`);
  fs.writeFileSync(configPath, JSON.stringify(configObject, null, 2), 'utf8');

  // Use PowerShell wrapper to completely isolate environment
  // This is necessary because shell:true inherits all parent env vars
  const wrapperScript = path.join(__dirname, 'start-mindsdb-wrapper.ps1');
  
  const mindsdbProcess = spawn('powershell', [
    '-ExecutionPolicy', 'Bypass',
    '-File', wrapperScript,
    configPath
  ], {
    stdio: 'inherit',
    shell: false, // Don't use shell to avoid env inheritance
    env: {
      PATH: process.env.PATH,
      SYSTEMROOT: process.env.SYSTEMROOT,
      SystemRoot: process.env.SystemRoot,
      TEMP: process.env.TEMP,
      TMP: process.env.TMP,
      HOME: process.env.HOME,
      USERPROFILE: process.env.USERPROFILE,
      MINDSDB_STORAGE_PATH: path.join(__dirname, '..', 'data', 'mindsdb'),
      MINDSDB_CACHE_PATH: path.join(__dirname, '..', 'data', 'mindsdb_cache')
    }
  });

  mindsdbProcess.on('error', (error) => {
    console.error('❌ Failed to start MindsDB:', error.message);
    process.exit(1);
  });

  mindsdbProcess.on('close', (code) => {
    if (code !== 0) {
      console.error(`❌ MindsDB exited with code ${code}`);
      process.exit(code);
    }
  });

  // Handle process termination
  process.on('SIGINT', () => {
    console.log('\n🛑 Shutting down MindsDB...');
    mindsdbProcess.kill('SIGINT');
    setTimeout(() => process.exit(0), 2000);
  });

  process.on('SIGTERM', () => {
    console.log('\n🛑 Terminating MindsDB...');
    mindsdbProcess.kill('SIGTERM');
    setTimeout(() => process.exit(0), 2000);
  });

  console.log(`✅ MindsDB started successfully!`);
  console.log(`📊 HTTP API: http://${MINDSDB_CONFIG.host}:${MINDSDB_CONFIG.httpPort}`);
  console.log(`🗄️  MySQL API: mysql://${MINDSDB_CONFIG.host}:${MINDSDB_CONFIG.mysqlPort}`);
  console.log('💡 Use Ctrl+C to stop the server');
}

/**
 * Main execution
 */
async function main() {
  try {
    await checkMindsDBInstallation();
    startMindsDB();
  } catch (error) {
    console.error('❌ Failed to start MindsDB:', error.message);
    process.exit(1);
  }
}

// Run if called directly
if (require.main === module) {
  main();
}

module.exports = {
  startMindsDB,
  checkMindsDBInstallation,
  MINDSDB_CONFIG
};
