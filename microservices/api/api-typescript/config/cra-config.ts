/**
 * Central Rank Authority (CRA) Configuration
 * Handles platform-specific configurations for Nexpo and Taurte
 */

export interface CRAConfig {
  platform: 'nexpo' | 'taurte' | 'both';
  nexpo?: {
    enabled: boolean;
    nextWebUrl: string;
    expoMobileUrl: string;
    apiUrl: string;
    auth0Domain: string;
    auth0ClientId: string;
    auth0Audience: string;
  };
  taurte?: {
    enabled: boolean;
    svelteWebUrl: string;
    tauriDesktopMode: 'svelte' | 'nextjs-export';
    tauriMobileUrl: string;
    apiUrl: string;
    auth0Domain: string;
    auth0ClientId: string;
    auth0Audience: string;
  };
  shared: {
    apiBaseUrl: string;
    corsOrigins: string[];
    rateLimiting: {
      windowMs: number;
      max: number;
    };
    security: {
      jwtSecret: string;
      sessionSecret: string;
    };
  };
}

/**
 * Get CRA configuration based on environment variables and feature flags
 */
export function getCRAConfig(): CRAConfig {
  // Platform feature flags
  const enableNextjs = process.env.ENABLE_NEXTJS_PLATFORM === 'true';
  const enableExpo = process.env.ENABLE_EXPO_PLATFORM === 'true';
  const enableTauri = process.env.ENABLE_TAURI_PLATFORM === 'true';
  const enableSvelte = process.env.ENABLE_SVELTE_PLATFORM !== 'false'; // Default to true

  // Determine platform mode
  const nexpoEnabled = enableNextjs || enableExpo;
  const taurteEnabled = enableTauri || enableSvelte;
  
  let platform: 'nexpo' | 'taurte' | 'both';
  if (nexpoEnabled && taurteEnabled) {
    platform = 'both';
  } else if (nexpoEnabled) {
    platform = 'nexpo';
  } else if (taurteEnabled) {
    platform = 'taurte';
  } else {
    throw new Error('At least one platform (Nexpo or Taurte) must be enabled');
  }

  // Base URLs
  const baseApiUrl = process.env.API_BASE_URL || 'http://localhost:8010';
  const nextWebUrl = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:3000';
  const expoMobileUrl = process.env.EXPO_PUBLIC_API_URL || 'http://localhost:19000';
  const svelteWebUrl = process.env.SVELTE_PUBLIC_API_URL || 'http://localhost:5173';
  const tauriMobileUrl = process.env.TAURI_PUBLIC_API_URL || 'http://localhost:1420';

  // Auth0 configuration
  const auth0Domain = process.env.AUTH0_ISSUER_BASE_URL?.replace('https://', '') || '';
  const auth0ClientId = process.env.AUTH0_CLIENT_ID || '';
  const auth0Audience = process.env.AUTH0_AUDIENCE || '';

  // Tauri desktop mode - default to Svelte, allow Next.js export
  const tauriDesktopMode = process.env.TAURI_DESKTOP_MODE === 'nextjs-export' ? 'nextjs-export' : 'svelte';

  const config: CRAConfig = {
    platform,
    shared: {
      apiBaseUrl: baseApiUrl,
      corsOrigins: [
        nextWebUrl,
        expoMobileUrl,
        svelteWebUrl,
        tauriMobileUrl,
        'http://localhost:3000',
        'http://localhost:5173',
        'http://localhost:19000',
        'http://localhost:1420',
        'tauri://localhost',
        'https://tauri.localhost'
      ],
      rateLimiting: {
        windowMs: 15 * 60 * 1000, // 15 minutes
        max: 100 // limit each IP to 100 requests per windowMs
      },
      security: {
        jwtSecret: process.env.JWT_SECRET || 'your-jwt-secret-key',
        sessionSecret: process.env.SESSION_SECRET || 'your-session-secret-key'
      }
    }
  };

  // Configure Nexpo if enabled
  if (nexpoEnabled) {
    config.nexpo = {
      enabled: true,
      nextWebUrl,
      expoMobileUrl,
      apiUrl: `${baseApiUrl}/api/nexpo`,
      auth0Domain,
      auth0ClientId,
      auth0Audience
    };
  }

  // Configure Taurte if enabled
  if (taurteEnabled) {
    config.taurte = {
      enabled: true,
      svelteWebUrl,
      tauriDesktopMode,
      tauriMobileUrl,
      apiUrl: `${baseApiUrl}/api/taurte`,
      auth0Domain,
      auth0ClientId,
      auth0Audience
    };
  }

  return config;
}

/**
 * Validate CRA configuration
 */
export function validateCRAConfig(config: CRAConfig): void {
  if (config.platform === 'nexpo' && !config.nexpo?.enabled) {
    throw new Error('Nexpo configuration is required when platform is set to nexpo');
  }
  
  if (config.platform === 'taurte' && !config.taurte?.enabled) {
    throw new Error('Taurte configuration is required when platform is set to taurte');
  }
  
  if (config.platform === 'both' && (!config.nexpo?.enabled || !config.taurte?.enabled)) {
    throw new Error('Both Nexpo and Taurte configurations are required when platform is set to both');
  }

  // Validate Auth0 configuration in production
  if (process.env.NODE_ENV === 'production') {
    if (config.nexpo?.enabled && (!config.nexpo.auth0Domain || !config.nexpo.auth0ClientId)) {
      throw new Error('Auth0 configuration is required for Nexpo in production');
    }
    
    if (config.taurte?.enabled && (!config.taurte.auth0Domain || !config.taurte.auth0ClientId)) {
      throw new Error('Auth0 configuration is required for Taurte in production');
    }
  }
}

/**
 * Get platform-specific API endpoints
 */
export function getApiEndpoints(config: CRAConfig) {
  const endpoints: Record<string, string> = {};

  if (config.nexpo?.enabled) {
    endpoints.nexpo = config.nexpo.apiUrl;
  }

  if (config.taurte?.enabled) {
    endpoints.taurte = config.taurte.apiUrl;
  }

  return endpoints;
}

/**
 * Get CORS origins for all enabled platforms
 */
export function getCorsOrigins(config: CRAConfig): string[] {
  return config.shared.corsOrigins;
}
