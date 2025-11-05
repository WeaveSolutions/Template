// Database configuration types
export interface DatabaseConfig {
  connectionString: string;
  provider: string;
  name?: string;
  options?: Record<string, any>;
}

// AWS RDS specific configuration
export interface AwsRdsConfig extends DatabaseConfig {
  region: string;
  accessKeyId?: string;
  secretAccessKey?: string;
  endpoint?: string;
  port?: number;
  ssl?: boolean;
}

// AWS S3 specific configuration
export interface AwsS3Config {
  region: string;
  bucket: string;
  accessKeyId?: string;
  secretAccessKey?: string;
  endpoint?: string;
}

// Firebase specific configuration
export interface FirebaseConfig {
  apiKey: string;
  authDomain: string;
  projectId: string;
  storageBucket?: string;
  messagingSenderId?: string;
  appId?: string;
}

// Common database connection interface
export interface DatabaseConnection {
  isConnected: boolean;
  connect(): Promise<void>;
  disconnect(): Promise<void>;
  healthCheck(): Promise<boolean>;
}

// Re-export global types
export * from './global';
