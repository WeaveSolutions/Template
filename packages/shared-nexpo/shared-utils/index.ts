export function greet(name: string): string {
  return `Hello, ${name}`;
}

// Export auth0 configuration
export * from './src/auth0-config';
