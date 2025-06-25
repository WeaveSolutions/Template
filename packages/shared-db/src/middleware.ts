import { db } from './client';

// Define middleware parameter types locally to avoid direct dependency on Prisma types
interface MiddlewareParams {
  model?: string;
  action: string;
  args: any;
  dataPath: string[];
  runInTransaction: boolean;
}

type NextFn = (params: MiddlewareParams) => Promise<any>;

/**
 * Logging middleware for Prisma
 */
export function withLogging() {
  return async function loggingMiddleware(
    params: MiddlewareParams,
    next: (params: MiddlewareParams) => Promise<any>
  ) {
    const before = Date.now();
    const result = await next(params);
    const after = Date.now();
    
    console.log(`Prisma Query ${params.model}.${params.action} took ${after - before}ms`);
    
    return result;
  };
}

/**
 * Performance monitoring middleware
 */
export function withPerformanceMonitoring() {
  return async function metricsMiddleware(
    params: MiddlewareParams,
    next: (params: MiddlewareParams) => Promise<any>
  ) {
    // Add your performance monitoring logic here
    // e.g., send metrics to Datadog, Prometheus, etc.
    return next(params);
  };
}

/**
 * Multi-tenancy middleware
 */

export function withMultiTenancy(tenantId: string) {
  return async function multiTenancyMiddleware(
    params: MiddlewareParams,
    next: (params: MiddlewareParams) => Promise<any>
  ) {
    // Skip for system models or specific actions
    if (params.model === 'Tenant' || params.action === 'createTenant') {
      return next(params);
    }

    // Add tenant ID to where clauses
    if (['findMany', 'findFirst', 'findUnique', 'update', 'delete', 'count'].includes(params.action)) {
      if (!params.args.where) {
        params.args.where = { tenantId };
      } else if (params.args.where.tenantId === undefined) {
        params.args.where.tenantId = tenantId;
      }
    }

    // Add tenant ID to create operations
    if (['create', 'createMany', 'upsert'].includes(params.action) && params.args.data) {
      if (Array.isArray(params.args.data)) {
        params.args.data = params.args.data.map((item: any) => ({
          ...item,
          tenantId,
        }));
      } else {
        params.args.data = {
          ...params.args.data,
          tenantId,
        };
      }
    }

    return next(params);
  };
}

/**
 * Field encryption middleware
 */
export function withFieldEncryption(encrypt: (value: string) => string, decrypt: (value: string) => string) {
  return async function encryptionMiddleware(
    params: MiddlewareParams,
    next: (params: MiddlewareParams) => Promise<any>
  ) {
    // Encrypt sensitive fields before writing to the database
    if (['create', 'update', 'upsert'].includes(params.action) && params.args.data) {
      // Define which fields to encrypt
      const encryptedFields = ['password', 'ssn', 'creditCard'];
      
      const processData = (data: any) => {
        Object.keys(data).forEach(key => {
          if (encryptedFields.includes(key) && typeof data[key] === 'string') {
            data[`${key}Encrypted`] = encrypt(data[key]);
            delete data[key];
          } else if (data[key] && typeof data[key] === 'object' && !Array.isArray(data[key])) {
            processData(data[key]);
          }
        });
      };

      if (Array.isArray(params.args.data)) {
        params.args.data.forEach(processData);
      } else {
        processData(params.args.data);
      }
    }

    const result = await next(params);

    // Decrypt fields after reading from the database
    if (result && ['findUnique', 'findFirst', 'findMany'].includes(params.action)) {
      const decryptData = (data: any) => {
        if (!data) return data;
        
        Object.keys(data).forEach(key => {
          if (key.endsWith('Encrypted') && typeof data[key] === 'string') {
            const fieldName = key.replace('Encrypted', '');
            data[fieldName] = decrypt(data[key]);
            delete data[key];
          } else if (data[key] && typeof data[key] === 'object' && !Array.isArray(data[key])) {
            decryptData(data[key]);
          }
        });
        
        return data;
      };

      if (Array.isArray(result)) {
        return result.map(decryptData);
      }
      return decryptData(result);
    }

    return result;
  };
}

/**
 * Initialize all middleware
 */
export function initializeMiddleware() {
  // Add middleware in the order they should run
  // @ts-ignore - db.prisma exists at runtime
  db.prisma.$use(withLogging());
  // @ts-ignore - db.prisma exists at runtime
  db.prisma.$use(withPerformanceMonitoring());
  
  // Add other middleware as needed
  // @ts-ignore - db.prisma exists at runtime
  // db.prisma.$use(withSoftDelete());
  // @ts-ignore - db.prisma exists at runtime
  // db.prisma.$use(withFieldEncryption(encryptFn, decryptFn));
}
