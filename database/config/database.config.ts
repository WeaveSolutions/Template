/**
 * Database Configuration for Weave BI Dashboard
 * PostgreSQL connection settings
 */

export interface DatabaseConfig {
  host: string;
  port: number;
  database: string;
  user: string;
  password: string;
  ssl?: boolean;
  max?: number; // Maximum number of clients in the pool
  idleTimeoutMillis?: number;
  connectionTimeoutMillis?: number;
}

/**
 * Development database configuration
 * ⚠️ NEVER commit real credentials to version control!
 * Use environment variables in production.
 */
export const developmentConfig: DatabaseConfig = {
  host: process.env.DB_HOST || 'localhost',
  port: parseInt(process.env.DB_PORT || '5432'),
  database: process.env.DB_NAME || 'weave_bi',
  user: process.env.DB_USER || 'weave_admin',
  password: process.env.DB_PASSWORD || 'weave_secure_pass_2024',
  ssl: false,
  max: 20,
  idleTimeoutMillis: 30000,
  connectionTimeoutMillis: 2000,
};

/**
 * Production database configuration
 * All values MUST come from environment variables
 */
export const productionConfig: DatabaseConfig = {
  host: process.env.DB_HOST!,
  port: parseInt(process.env.DB_PORT || '5432'),
  database: process.env.DB_NAME!,
  user: process.env.DB_USER!,
  password: process.env.DB_PASSWORD!,
  ssl: true,
  max: 50,
  idleTimeoutMillis: 30000,
  connectionTimeoutMillis: 5000,
};

/**
 * Get database configuration based on environment
 */
export function getDatabaseConfig(): DatabaseConfig {
  const env = process.env.NODE_ENV || 'development';
  
  if (env === 'production') {
    // Validate required env vars
    const required = ['DB_HOST', 'DB_NAME', 'DB_USER', 'DB_PASSWORD'];
    const missing = required.filter(key => !process.env[key]);
    
    if (missing.length > 0) {
      throw new Error(
        `Missing required database environment variables: ${missing.join(', ')}`
      );
    }
    
    return productionConfig;
  }
  
  return developmentConfig;
}

/**
 * PostgreSQL connection string format
 * Format: postgresql://[user[:password]@][host][:port][/database]
 */
export function getConnectionString(): string {
  const config = getDatabaseConfig();
  return `postgresql://${config.user}:${config.password}@${config.host}:${config.port}/${config.database}`;
}

/**
 * Connection string without password (for logging)
 */
export function getSafeConnectionString(): string {
  const config = getDatabaseConfig();
  return `postgresql://${config.user}:****@${config.host}:${config.port}/${config.database}`;
}
