// Global type declarations for Node.js
declare namespace NodeJS {
  interface ProcessEnv {
    NODE_ENV: 'development' | 'production' | 'test';
    PRISMA_LOG_QUERIES?: string;
    PRISMA_LOG_LEVEL?: string;
    
    // Database feature flags
    ENABLE_POSTGRES?: string;
    ENABLE_MONGODB?: string;
    ENABLE_SUPABASE?: string;
    ENABLE_COSMOSDB?: string;
    ENABLE_SQLSERVER?: string;
    
    // Database URLs
    POSTGRES_PRISMA_URL?: string;
    POSTGRES_URL_NON_POOLING?: string;
    MONGODB_URI?: string;
    SUPABASE_DATABASE_URL?: string;
    SUPABASE_DIRECT_URL?: string;
    COSMOSDB_MONGODB_URI?: string;
    SQLSERVER_URL?: string;
    
    // Logging
    DATABASE_LOG_LEVEL?: string;
    PRISMA_LOGGING?: string;
    
    // Default provider
    DEFAULT_DATABASE_PROVIDER?: string;
  }
  
  interface Process {
    env: ProcessEnv;
  }
  
  interface Require {
    (id: string): any;
    resolve: (id: string) => string;
    cache: any;
    extensions: any;
    main: any;
  }
  
  interface Module {
    exports: any;
    require: Require;
    id: string;
    filename: string;
    loaded: boolean;
    parent: any;
    children: any[];
    paths: string[];
  }
}

declare var process: NodeJS.Process;
declare var require: NodeJS.Require;
declare var module: NodeJS.Module;
