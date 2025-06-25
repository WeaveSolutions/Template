// Type declarations for third-party modules
declare module 'uuid';
declare module 'bcryptjs';
declare module '@prisma/client';

// Import Prisma types
import { PrismaClient as OriginalPrismaClient } from '@prisma/client';

// Augment the PrismaClient type definitions
declare global {
  // Define global variable for PrismaClient (used for development hot reloading)
  var prisma: OriginalPrismaClient | undefined;
}

// Extend Prisma namespace
declare namespace PrismaClient {
  // Define MiddlewareParams interface
  interface MiddlewareParams {
    model?: string;
    action: string;
    args: any;
    dataPath: string[];
    runInTransaction: boolean;
  }
  
  // Define the TransactionClient interface
  type TransactionClient = Omit<OriginalPrismaClient, '$connect' | '$disconnect' | '$on' | '$transaction' | '$use'>
}

// Extend NodeJS namespace
declare namespace NodeJS {
  interface Process {
    exit(code?: number): never;
    exitCode?: number;
  }
}

// Define Prisma namespace for middleware types
declare namespace Prisma {
  interface MiddlewareParams {
    model?: string;
    action: string;
    args: any;
    dataPath: string[];
    runInTransaction: boolean;
  }
}

// Define DatabaseClient interface with prisma property
interface DatabaseClient {
  prisma: import('@prisma/client').PrismaClient;
  connect(): Promise<void>;
  disconnect(): Promise<void>;
  [key: string]: any;
}
