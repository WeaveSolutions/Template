// Learn more https://docs.expo.io/guides/customizing-metro
const { getDefaultConfig } = require('expo/metro-config');
const path = require('path');
const os = require('os');

// Find the project and workspace directories
const projectRoot = __dirname;
// This can be replaced with `find-yarn-workspace-root`
const workspaceRoot = path.resolve(projectRoot, '../..');

// Get local IP address for better device connectivity
function getLocalIPAddress() {
  const interfaces = os.networkInterfaces();
  for (const interfaceName in interfaces) {
    const addresses = interfaces[interfaceName];
    for (const address of addresses) {
      if (address.family === 'IPv4' && !address.internal) {
        return address.address;
      }
    }
  }
  return '127.0.0.1'; // Fallback to localhost
}

const config = getDefaultConfig(projectRoot);

// Set server IP address for better device connectivity
config.server = {
  host: getLocalIPAddress(),
};

// 1. Watch all files within the monorepo
config.watchFolders = [workspaceRoot];
// 2. Let Metro know where to resolve packages and resolve modules
config.resolver.nodeModulesPaths = [
  path.resolve(projectRoot, 'node_modules'),
  path.resolve(workspaceRoot, 'node_modules'),
];
// 3. Force Metro to resolve (sub)dependencies only from the `nodeModulesPaths`
config.resolver.disableHierarchicalLookup = true;

module.exports = config; 