// Jest setup file for email service tests

// Mock console methods to reduce noise in tests
global.console = {
  ...console,
  // Suppress console.log in tests unless explicitly needed
  log: jest.fn(),
  debug: jest.fn(),
  info: jest.fn(),
  warn: jest.fn(),
  error: jest.fn(),
};

// Mock process.env for consistent testing
process.env.NODE_ENV = 'test';
process.env.BREVO_API_KEY = 'test-brevo-key';
process.env.BREVO_SENDER_EMAIL = 'test@example.com';
process.env.BREVO_SENDER_NAME = 'Test Sender';
process.env.ZEROBOUNCE_API_KEY = 'test-zerobounce-key';

// Global test timeout
jest.setTimeout(30000);

// Mock FormData for Node.js environment
if (!global.FormData) {
  global.FormData = class FormData {
    private data: Map<string, any> = new Map();
    
    append(key: string, value: any, filename?: string) {
      this.data.set(key, { value, filename });
    }
    
    get(key: string) {
      return this.data.get(key)?.value;
    }
    
    has(key: string) {
      return this.data.has(key);
    }
    
    delete(key: string) {
      this.data.delete(key);
    }
    
    entries() {
      return this.data.entries();
    }
  };
}

// Mock Blob for file upload tests
if (!global.Blob) {
  global.Blob = class Blob {
    constructor(public parts: any[], public options: any = {}) {}
    
    get size() {
      return this.parts.reduce((size, part) => size + (part.length || 0), 0);
    }
    
    get type() {
      return this.options.type || '';
    }
  };
}
