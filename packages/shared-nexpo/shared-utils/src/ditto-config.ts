/**
 * Ditto Configuration Utility
 * 
 * This module provides configuration helpers for setting up Ditto with
 * various cloud providers, including IBM Cloud DB2.
 */

import { DittoConfig } from './ditto-service';
import { CloudSyncConfig } from './ditto-cloud-sync';

/**
 * Gets the Ditto configuration from environment variables
 */
export function getDittoConfig(): DittoConfig {
  // Check if Ditto is enabled
  const enabled = process.env.ENABLE_DITTO === 'true';
  
  if (!enabled) {
    return {
      enabled: false,
      appId: '',
      token: '',
    };
  }
  
  // Get required configuration
  const appId = process.env.DITTO_APP_ID || '';
  const token = process.env.DITTO_TOKEN || '';
  
  // Get optional peer-to-peer configuration
  const syncWithPeers = process.env.DITTO_SYNC_WITH_PEERS === 'true';
  
  // Return the configuration
  return {
    enabled,
    appId,
    token,
    syncWithPeers,
  };
}

/**
 * Gets the Ditto cloud sync configuration from environment variables
 */
export function getCloudSyncConfig(): CloudSyncConfig {
  // Check if cloud sync is enabled
  const enabled = process.env.DITTO_CLOUD_SYNC_ENABLED === 'true';
  
  if (!enabled) {
    return {
      enabled: false,
      syncUrl: '',
      syncInterval: 0,
      batchSize: 50,
      providers: [],
    };
  }
  
  // Get required configuration
  const syncUrl = process.env.DITTO_CLOUD_SYNC_URL || '';
  const syncInterval = parseInt(process.env.DITTO_CLOUD_SYNC_INTERVAL || '300000', 10);
  const batchSize = parseInt(process.env.DITTO_CLOUD_SYNC_BATCH_SIZE || '50', 10);
  
  // Get enabled providers
  const providers = [];
  
  if (process.env.DITTO_SYNC_POSTGRES === 'true') {
    providers.push('postgres');
  }
  
  if (process.env.DITTO_SYNC_MONGODB === 'true') {
    providers.push('mongodb');
  }
  
  if (process.env.DITTO_SYNC_COSMOSDB === 'true') {
    providers.push('cosmosdb');
  }
  
  if (process.env.DITTO_SYNC_SQLSERVER === 'true') {
    providers.push('sqlserver');
  }
  
  if (process.env.DITTO_SYNC_IBMCLOUD === 'true') {
    providers.push('ibmcloud');
  }
  
  // Return the configuration
  return {
    enabled,
    syncUrl,
    syncInterval,
    batchSize,
    providers,
  };
}

/**
 * Creates an Expo compatible configuration object from environment variables
 * This is needed because Expo uses a different environment variable naming convention
 */
export function getExpoDittoConfig(): DittoConfig {
  // For Expo, we use EXPO_PUBLIC_ prefixed env vars
  // In a real implementation, you would use the Expo Constants or process.env
  
  // Check if Ditto is enabled
  const enabled = process.env.EXPO_PUBLIC_ENABLE_DITTO === 'true';
  
  if (!enabled) {
    return {
      enabled: false,
      appId: '',
      token: '',
    };
  }
  
  // Get required configuration
  const appId = process.env.EXPO_PUBLIC_DITTO_APP_ID || '';
  const token = process.env.EXPO_PUBLIC_DITTO_TOKEN || '';
  
  // Get optional peer-to-peer configuration
  const syncWithPeers = process.env.EXPO_PUBLIC_DITTO_SYNC_WITH_PEERS === 'true';
  
  // Return the configuration
  return {
    enabled,
    appId,
    token,
    syncWithPeers,
  };
}

/**
 * Gets the Ditto cloud sync configuration for Expo from environment variables
 */
export function getExpoCloudSyncConfig(): CloudSyncConfig {
  // Check if cloud sync is enabled
  const enabled = process.env.EXPO_PUBLIC_DITTO_CLOUD_SYNC_ENABLED === 'true';
  
  if (!enabled) {
    return {
      enabled: false,
      syncUrl: '',
      syncInterval: 0,
      batchSize: 50,
      providers: [],
    };
  }
  
  // Get required configuration
  const syncUrl = process.env.EXPO_PUBLIC_DITTO_CLOUD_SYNC_URL || '';
  const syncInterval = parseInt(process.env.EXPO_PUBLIC_DITTO_CLOUD_SYNC_INTERVAL || '300000', 10);
  const batchSize = parseInt(process.env.EXPO_PUBLIC_DITTO_CLOUD_SYNC_BATCH_SIZE || '50', 10);
  
  // Get enabled providers
  const providers = [];
  
  if (process.env.EXPO_PUBLIC_DITTO_SYNC_POSTGRES === 'true') {
    providers.push('postgres');
  }
  
  if (process.env.EXPO_PUBLIC_DITTO_SYNC_MONGODB === 'true') {
    providers.push('mongodb');
  }
  
  if (process.env.EXPO_PUBLIC_DITTO_SYNC_COSMOSDB === 'true') {
    providers.push('cosmosdb');
  }
  
  if (process.env.EXPO_PUBLIC_DITTO_SYNC_SQLSERVER === 'true') {
    providers.push('sqlserver');
  }
  
  if (process.env.EXPO_PUBLIC_DITTO_SYNC_IBMCLOUD === 'true') {
    providers.push('ibmcloud');
  }
  
  // Return the configuration
  return {
    enabled,
    syncUrl,
    syncInterval,
    batchSize,
    providers,
  };
}
