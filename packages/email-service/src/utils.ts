import { EmailService } from './email-service';
import { EmailServiceConfig } from './types';

/**
 * Create and configure an EmailService instance
 */
export function createEmailService(config: EmailServiceConfig, autoVerify = true): EmailService {
  validateEmailConfig(config);
  return new EmailService(config, autoVerify);
}

/**
 * Validate email service configuration
 */
export function validateEmailConfig(config: EmailServiceConfig): void {
  if (!config.brevo?.apiKey) {
    throw new Error('Brevo API key is required');
  }
  
  if (!config.brevo?.senderEmail) {
    throw new Error('Brevo sender email is required');
  }
  
  if (!config.brevo?.senderName) {
    throw new Error('Brevo sender name is required');
  }
  
  if (!config.zeroBounce?.apiKey) {
    throw new Error('Zero Bounce API key is required');
  }
  
  // Validate email format
  const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
  if (!emailRegex.test(config.brevo.senderEmail)) {
    throw new Error('Invalid sender email format');
  }
}

/**
 * Create email service from environment variables
 */
export function createEmailServiceFromEnv(): EmailService {
  const config: EmailServiceConfig = {
    brevo: {
      apiKey: process.env.BREVO_API_KEY || '',
      senderEmail: process.env.BREVO_SENDER_EMAIL || '',
      senderName: process.env.BREVO_SENDER_NAME || '',
    },
    zeroBounce: {
      apiKey: process.env.ZEROBOUNCE_API_KEY || '',
    },
  };
  
  return createEmailService(config);
}

/**
 * Batch process items with rate limiting
 */
export async function batchProcess<T, R>(
  items: T[],
  processor: (item: T) => Promise<R>,
  batchSize = 10,
  delayMs = 1000
): Promise<R[]> {
  const results: R[] = [];
  
  for (let i = 0; i < items.length; i += batchSize) {
    const batch = items.slice(i, i + batchSize);
    const batchPromises = batch.map(processor);
    const batchResults = await Promise.all(batchPromises);
    
    results.push(...batchResults);
    
    // Add delay between batches
    if (i + batchSize < items.length) {
      await new Promise(resolve => setTimeout(resolve, delayMs));
    }
  }
  
  return results;
}

/**
 * Retry a function with exponential backoff
 */
export async function retryWithBackoff<T>(
  fn: () => Promise<T>,
  maxRetries = 3,
  baseDelayMs = 1000
): Promise<T> {
  let lastError: Error;
  
  for (let attempt = 0; attempt <= maxRetries; attempt++) {
    try {
      return await fn();
    } catch (error) {
      lastError = error as Error;
      
      if (attempt === maxRetries) {
        break;
      }
      
      const delay = baseDelayMs * Math.pow(2, attempt);
      await new Promise(resolve => setTimeout(resolve, delay));
    }
  }
  
  throw lastError!;
}

/**
 * Parse CSV content into array of objects
 */
export function parseCsv(csvContent: string): Record<string, string>[] {
  const lines = csvContent.split('\n').filter(line => line.trim());
  if (lines.length === 0) return [];
  
  const headers = lines[0].split(',').map(h => h.trim());
  const results: Record<string, string>[] = [];
  
  for (let i = 1; i < lines.length; i++) {
    const values = lines[i].split(',').map(v => v.trim());
    const row: Record<string, string> = {};
    
    headers.forEach((header, index) => {
      row[header] = values[index] || '';
    });
    
    results.push(row);
  }
  
  return results;
}

/**
 * Convert array of objects to CSV format
 */
export function toCsv(data: Record<string, any>[]): string {
  if (data.length === 0) return '';
  
  const headers = Object.keys(data[0]);
  const csvLines = [headers.join(',')];
  
  for (const row of data) {
    const values = headers.map(header => {
      const value = row[header];
      return typeof value === 'string' && value.includes(',') 
        ? `"${value}"` 
        : String(value || '');
    });
    csvLines.push(values.join(','));
  }
  
  return csvLines.join('\n');
}

/**
 * Validate email address format
 */
export function isValidEmail(email: string): boolean {
  const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
  return emailRegex.test(email);
}

/**
 * Extract email addresses from text
 */
export function extractEmails(text: string): string[] {
  const emailRegex = /[^\s@]+@[^\s@]+\.[^\s@]+/g;
  const matches = text.match(emailRegex) || [];
  return matches.filter(isValidEmail);
}

/**
 * Deduplicate email addresses
 */
export function deduplicateEmails(emails: string[]): string[] {
  const seen = new Set<string>();
  return emails.filter(email => {
    const lowercaseEmail = email.toLowerCase();
    if (seen.has(lowercaseEmail)) {
      return false;
    }
    seen.add(lowercaseEmail);
    return true;
  });
}

/**
 * Chunk array into smaller arrays
 */
export function chunkArray<T>(array: T[], chunkSize: number): T[][] {
  const chunks: T[][] = [];
  for (let i = 0; i < array.length; i += chunkSize) {
    chunks.push(array.slice(i, i + chunkSize));
  }
  return chunks;
}
