#!/usr/bin/env node
/**
 * MindsDB Multi-Cloud Integration Script
 * 
 * This script helps set up MindsDB with your multi-cloud Terraform infrastructure by:
 * 1. Detecting active cloud providers from terraform.tfvars
 * 2. Setting up appropriate environment variables
 * 3. Generating connection configurations for each provider
 * 4. Creating a docker-compose override with the right settings
 */

const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process');
const readline = require('readline');

// Paths
const PROJECT_ROOT = path.resolve(__dirname, '../..');
const TERRAFORM_DIR = path.join(PROJECT_ROOT, 'terraform');
const MINDSDB_DIR = path.join(PROJECT_ROOT, 'mindsdb-main');
const DOCKER_COMPOSE_PATH = path.join(MINDSDB_DIR, 'docker-compose.yml');
const ENV_TEMPLATE_PATH = path.join(MINDSDB_DIR, '.env.example');
const ENV_PATH = path.join(MINDSDB_DIR, '.env');

// Provider settings
const PROVIDERS = {
  aws: {
    name: 'AWS',
    envPrefix: 'AWS_',
    dataSourceType: 's3',
    defaultRegion: 'us-west-2',
  },
  gcp: {
    name: 'Google Cloud',
    envPrefix: 'GCP_',
    dataSourceType: 'bigquery',
    defaultRegion: 'us-central1',
  },
  azure: {
    name: 'Azure',
    envPrefix: 'AZURE_',
    dataSourceType: 'azureblob',
    defaultRegion: 'eastus',
  },
  oci: {
    name: 'Oracle Cloud',
    envPrefix: 'OCI_',
    dataSourceType: 'objectstorage',
    defaultRegion: 'us-ashburn-1',
  },
  ibm: {
    name: 'IBM Cloud',
    envPrefix: 'IBMCLOUD_',
    dataSourceType: 'cos',
    defaultRegion: 'us-south',
  },
  cloudflare: {
    name: 'Cloudflare',
    envPrefix: 'CLOUDFLARE_',
    dataSourceType: 'r2',
    defaultRegion: 'auto',
  }
};

/**
 * Read terraform.tfvars to determine enabled cloud providers
 */
function detectEnabledProviders() {
  console.log('🔍 Detecting enabled cloud providers...');
  
  try {
    const tfvarsPath = path.join(TERRAFORM_DIR, 'terraform.tfvars');
    if (!fs.existsSync(tfvarsPath)) {
      console.log('⚠️  terraform.tfvars not found. Using example file...');
      const examplePath = path.join(TERRAFORM_DIR, 'terraform.tfvars.example');
      if (fs.existsSync(examplePath)) {
        fs.copyFileSync(examplePath, tfvarsPath);
      } else {
        console.error('❌ No terraform.tfvars or example file found.');
        return {};
      }
    }
    
    const tfvarsContent = fs.readFileSync(tfvarsPath, 'utf-8');
    const enabledProviders = {};
    
    // Check for each provider
    for (const provider of Object.keys(PROVIDERS)) {
      const regex = new RegExp(`enable_${provider}\\s*=\\s*true`, 'i');
      enabledProviders[provider] = regex.test(tfvarsContent);
    }
    
    console.log('✅ Detected provider settings:');
    for (const [provider, enabled] of Object.entries(enabledProviders)) {
      console.log(`   ${PROVIDERS[provider].name}: ${enabled ? '✅ Enabled' : '❌ Disabled'}`);
    }
    
    return enabledProviders;
  } catch (error) {
    console.error('❌ Error detecting enabled providers:', error);
    return {};
  }
}

/**
 * Create .env file for MindsDB with appropriate settings
 */
function generateEnvFile(enabledProviders) {
  console.log('\n📝 Generating MindsDB environment configuration...');
  
  try {
    let envTemplate = '';
    if (fs.existsSync(ENV_TEMPLATE_PATH)) {
      envTemplate = fs.readFileSync(ENV_TEMPLATE_PATH, 'utf-8');
    }
    
    // Add provider environment flags
    let envContent = envTemplate + '\n\n# Cloud Provider Settings\n';
    
    for (const [provider, enabled] of Object.entries(enabledProviders)) {
      envContent += `ENABLE_${provider.toUpperCase()}=${enabled ? 'true' : 'false'}\n`;
    }
    
    // Add MindsDB default settings
    envContent += '\n# MindsDB Settings\n';
    envContent += 'MINDSDB_USERNAME=mindsdb\n';
    envContent += 'MINDSDB_PASSWORD=mindsdb123\n';
    envContent += 'MINDSDB_APIS=mysql,http,mongodb\n';
    envContent += `MINDSDB_STORAGE_DIR=${path.join(MINDSDB_DIR, 'storage').replace(/\\/g, '/')}\n`;
    
    fs.writeFileSync(ENV_PATH, envContent);
    console.log('✅ Generated .env file at:', ENV_PATH);
  } catch (error) {
    console.error('❌ Error generating environment file:', error);
  }
}

/**
 * Generate a docker-compose override with appropriate volumes and settings
 */
function generateDockerComposeOverride(enabledProviders) {
  console.log('\n🐳 Setting up Docker configuration...');
  
  const overridePath = path.join(MINDSDB_DIR, 'docker-compose.override.yml');
  
  try {
    // Start with base override structure
    const override = {
      version: '3',
      services: {
        mindsdb: {
          environment: [
            'MINDSDB_USERNAME=${MINDSDB_USERNAME}',
            'MINDSDB_PASSWORD=${MINDSDB_PASSWORD}',
            'MINDSDB_APIS=${MINDSDB_APIS}'
          ],
          volumes: [
            './storage:/root/mdb_storage'
          ]
        }
      }
    };
    
    // Add provider-specific volumes and settings
    if (enabledProviders.aws) {
      override.services.mindsdb.environment.push('AWS_ACCESS_KEY_ID=${AWS_ACCESS_KEY_ID}');
      override.services.mindsdb.environment.push('AWS_SECRET_ACCESS_KEY=${AWS_SECRET_ACCESS_KEY}');
      override.services.mindsdb.environment.push('AWS_REGION=${AWS_REGION:-us-west-2}');
      // Add AWS credentials mount if exists
      const awsCredentialsPath = path.join(process.env.USERPROFILE || process.env.HOME, '.aws');
      if (fs.existsSync(awsCredentialsPath)) {
        override.services.mindsdb.volumes.push(`${awsCredentialsPath}:/root/.aws:ro`);
      }
    }
    
    if (enabledProviders.gcp) {
      override.services.mindsdb.environment.push('GOOGLE_APPLICATION_CREDENTIALS=/root/gcp-credentials.json');
      // Add GCP credentials mount if exists
      const gcpCredentialsPath = process.env.GOOGLE_APPLICATION_CREDENTIALS;
      if (gcpCredentialsPath && fs.existsSync(gcpCredentialsPath)) {
        override.services.mindsdb.volumes.push(`${gcpCredentialsPath}:/root/gcp-credentials.json:ro`);
      }
    }
    
    if (enabledProviders.azure) {
      override.services.mindsdb.environment.push('AZURE_STORAGE_CONNECTION_STRING=${AZURE_STORAGE_CONNECTION_STRING}');
      override.services.mindsdb.environment.push('AZURE_CLIENT_ID=${AZURE_CLIENT_ID}');
      override.services.mindsdb.environment.push('AZURE_CLIENT_SECRET=${AZURE_CLIENT_SECRET}');
      override.services.mindsdb.environment.push('AZURE_TENANT_ID=${AZURE_TENANT_ID}');
    }
    
    if (enabledProviders.oci) {
      override.services.mindsdb.environment.push('OCI_CONFIG_PROFILE=${OCI_CONFIG_PROFILE:-DEFAULT}');
      // Add OCI config mount if exists
      const ociConfigPath = path.join(process.env.USERPROFILE || process.env.HOME, '.oci');
      if (fs.existsSync(ociConfigPath)) {
        override.services.mindsdb.volumes.push(`${ociConfigPath}:/root/.oci:ro`);
      }
    }
    
    if (enabledProviders.ibm) {
      override.services.mindsdb.environment.push('IBMCLOUD_API_KEY=${IBMCLOUD_API_KEY}');
      override.services.mindsdb.environment.push('IBMCLOUD_REGION=${IBMCLOUD_REGION}');
      override.services.mindsdb.environment.push('IBMCLOUD_RESOURCE_GROUP=${IBMCLOUD_RESOURCE_GROUP:-default}');
      
      // Add IBM Cloud config directory if it exists
      const ibmConfigPath = path.join(process.env.USERPROFILE || process.env.HOME, '.bluemix');
      if (fs.existsSync(ibmConfigPath)) {
        override.services.mindsdb.volumes.push(`${ibmConfigPath}:/root/.bluemix:ro`);
      }
    }
    
    if (enabledProviders.cloudflare) {
      override.services.mindsdb.environment.push('CLOUDFLARE_API_TOKEN=${CLOUDFLARE_API_TOKEN}');
      override.services.mindsdb.environment.push('CLOUDFLARE_ACCOUNT_ID=${CLOUDFLARE_ACCOUNT_ID}');
    }
    
    // Write the override file
    fs.writeFileSync(overridePath, JSON.stringify(override, null, 2));
    console.log('✅ Generated docker-compose.override.yml');
    
  } catch (error) {
    console.error('❌ Error generating Docker Compose override:', error);
  }
}

/**
 * Install required npm packages
 */
function installDependencies() {
  console.log('\n📦 Installing dependencies...');
  try {
    execSync('pnpm install dotenv --no-save', { 
      cwd: MINDSDB_DIR,
      stdio: 'inherit' 
    });
    console.log('✅ Dependencies installed');
  } catch (error) {
    console.error('❌ Error installing dependencies:', error);
  }
}

/**
 * Main function to run all setup steps
 */
async function main() {
  console.log('🧠 MindsDB Multi-Cloud Integration Setup');
  console.log('======================================\n');
  
  // Create storage directory
  const storageDir = path.join(MINDSDB_DIR, 'storage');
  if (!fs.existsSync(storageDir)) {
    fs.mkdirSync(storageDir, { recursive: true });
  }
  
  // Install required dependencies
  installDependencies();
  
  // Detect enabled providers
  const enabledProviders = detectEnabledProviders();
  
  // Generate environment file
  generateEnvFile(enabledProviders);
  
  // Generate docker-compose override
  generateDockerComposeOverride(enabledProviders);
  
  console.log('\n✨ Setup completed successfully!');
  console.log('\nNext steps:');
  console.log('1. Review .env file and update credentials as needed');
  console.log('2. Run MindsDB using:');
  console.log('   - Windows: .\\install_mindsdb.ps1');
  console.log('   - Mac/Linux: ./install_mindsdb.sh');
  console.log('\nHappy machine learning with MindsDB and Terraform! 🚀');
}

// Run the main function
main().catch(error => {
  console.error('❌ Error during setup:', error);
  process.exit(1);
});
