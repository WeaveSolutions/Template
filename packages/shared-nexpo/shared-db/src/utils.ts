import { Prisma, PrismaClient } from '@prisma/client';
import { db } from './client';

// Define the interface for database access
interface DbWithPrisma {
  prisma: PrismaClient;
  [key: string]: any;
}

// Type for middleware parameters
type MiddlewareParams = {
  model?: string;
  action: string;
  args: any;
  dataPath: string[];
  runInTransaction: boolean;
};

/**
 * Database transaction utilities
 */
export async function transaction<T>(
  callback: (tx: PrismaClient) => Promise<T>,
  options?: { maxRetries?: number; retryDelay?: number }
): Promise<T> {
  const maxRetries = options?.maxRetries ?? 3;
  const retryDelay = options?.retryDelay ?? 100;

  for (let attempt = 1; attempt <= maxRetries; attempt++) {
    try {
      return await (db as unknown as DbWithPrisma).prisma.$transaction(async (tx: PrismaClient) => {
        return await callback(tx);
      });
    } catch (error) {
      if (isRetryableError(error) && attempt < maxRetries) {
        // Exponential backoff
        const delay = retryDelay * Math.pow(2, attempt - 1);
        await new Promise((resolve) => setTimeout(resolve, delay));
        continue;
      }
      throw error;
    }
  }
  throw new Error('Max retries reached for transaction');
}

/**
 * Check if an error is retryable
 */
function isRetryableError(error: unknown): boolean {
  // Check for PrismaClientKnownRequestError by checking for code property
  if (error && typeof error === 'object' && 'code' in error) {
    // List of retryable error codes
    const retryableCodes = [
      'P1008', // Operation timed out
      'P2024', // Timed out fetching a new connection from the connection pool
      'P2034', // Transaction failed due to a write conflict or a deadlock
    ];
    return retryableCodes.includes((error as { code: string }).code);
  }
  return false;
}

/**
 * Pagination utilities
 */
export interface PaginationOptions {
  page?: number;
  pageSize?: number;
  orderBy?: Record<string, 'asc' | 'desc'>;
}

export interface PaginatedResult<T> {
  data: T[];
  total: number;
  page: number;
  pageSize: number;
  totalPages: number;
  hasNextPage: boolean;
  hasPreviousPage: boolean;
}

export async function paginate<T, WhereInput, OrderByInput>(
  model: keyof PrismaClient,
  where: WhereInput,
  options: PaginationOptions = {}
): Promise<PaginatedResult<T>> {
  const page = Math.max(1, options.page || 1);
  const pageSize = Math.min(100, Math.max(1, options.pageSize || 10));
  const skip = (page - 1) * pageSize;

  const [total, items] = await Promise.all([
    (db.prisma[model] as any).count({ where }),
    (db.prisma[model] as any).findMany({
      where,
      skip,
      take: pageSize,
      orderBy: options.orderBy,
    }),
  ]);

  const totalPages = Math.ceil(total / pageSize);

  return {
    data: items,
    total,
    page,
    pageSize,
    totalPages,
    hasNextPage: page < totalPages,
    hasPreviousPage: page > 1,
  };
}

/**
 * Soft delete middleware
 */
export function createSoftDeleteMiddleware() {
  return async (params: MiddlewareParams, next: (params: MiddlewareParams) => Promise<any>) => {
    // Handle soft delete for delete operations
    if (params.model && params.action === 'delete') {
      // Replace with update operation
      params.action = 'update';
      params.args.data = { deletedAt: new Date() };
    }

    // Filter out soft-deleted records for find operations
    if (params.model && ['findUnique', 'findFirst', 'findMany'].includes(params.action)) {
      if (!params.args.where) {
        params.args.where = { deletedAt: null };
      } else if (params.args.where.deletedAt === undefined) {
        params.args.where.deletedAt = null;
      }
    }

    return next(params);
  };
}

/**
 * Database health check
 */
export async function checkDatabaseHealth() {
  try {
    await (db as unknown as DbWithPrisma).prisma.$queryRaw`SELECT 1`;
    return { status: 'ok' };
  } catch (error) {
    return { status: 'error', error: error instanceof Error ? error.message : 'Unknown error' };
  }
}
