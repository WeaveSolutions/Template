import axios, { AxiosInstance } from 'axios';
import { EmailVerificationResult, BulkVerificationResult } from './types';

export class ZeroBounceClient {
  private client: AxiosInstance;
  private apiKey: string;

  constructor(apiKey: string) {
    this.apiKey = apiKey;
    this.client = axios.create({
      baseURL: 'https://api.zerobounce.net/v2',
      timeout: 30000,
    });
  }

  /**
   * Verify a single email address
   */
  async verifyEmail(email: string, ipAddress?: string): Promise<EmailVerificationResult> {
    try {
      const response = await this.client.get('/validate', {
        params: {
          api_key: this.apiKey,
          email: email,
          ip_address: ipAddress || '',
        },
      });

      return this.mapVerificationResponse(response.data);
    } catch (error) {
      throw new Error(`ZeroBounce API error: ${error}`);
    }
  }

  /**
   * Verify multiple email addresses in bulk
   */
  async verifyEmailBulk(emails: string[]): Promise<BulkVerificationResult> {
    if (emails.length === 0) {
      return {
        results: [],
        totalCount: 0,
        validCount: 0,
        invalidCount: 0,
        creditsUsed: 0,
      };
    }

    // For bulk verification, we'll use the batch validation endpoint
    // First, send the file for processing
    const fileId = await this.sendBulkFile(emails);
    
    // Poll for results
    const results = await this.getBulkResults(fileId);
    
    return this.processBulkResults(results);
  }

  /**
   * Send bulk verification file
   */
  private async sendBulkFile(emails: string[]): Promise<string> {
    const formData = new FormData();
    
    // Create CSV content
    const csvContent = 'email\n' + emails.join('\n');
    const blob = new Blob([csvContent], { type: 'text/csv' });
    
    formData.append('api_key', this.apiKey);
    formData.append('email_batch', blob, 'emails.csv');
    formData.append('email_address_column', '1');

    try {
      const response = await this.client.post('/sendfile', formData, {
        headers: {
          'Content-Type': 'multipart/form-data',
        },
      });

      if (response.data.success === false) {
        throw new Error(`Bulk verification failed: ${response.data.message}`);
      }

      return response.data.file_id;
    } catch (error) {
      throw new Error(`Failed to send bulk verification file: ${error}`);
    }
  }

  /**
   * Get bulk verification results
   */
  private async getBulkResults(fileId: string): Promise<any[]> {
    const maxAttempts = 60; // 5 minutes max wait time
    let attempts = 0;

    while (attempts < maxAttempts) {
      try {
        const response = await this.client.get('/getfile', {
          params: {
            api_key: this.apiKey,
            file_id: fileId,
          },
        });

        if (response.data.file_status === 'Complete') {
          return response.data.local_file_path ? 
            await this.downloadResults(response.data.local_file_path) : 
            [];
        }

        if (response.data.file_status === 'Processing') {
          // Wait 5 seconds before checking again
          await new Promise(resolve => setTimeout(resolve, 5000));
          attempts++;
          continue;
        }

        throw new Error(`Bulk verification failed with status: ${response.data.file_status}`);
      } catch (error) {
        throw new Error(`Failed to get bulk results: ${error}`);
      }
    }

    throw new Error('Bulk verification timed out');
  }

  /**
   * Download and parse bulk results
   */
  private async downloadResults(downloadUrl: string): Promise<any[]> {
    try {
      const response = await axios.get(downloadUrl);
      return this.parseCsvResults(response.data);
    } catch (error) {
      throw new Error(`Failed to download results: ${error}`);
    }
  }

  /**
   * Parse CSV results into structured data
   */
  private parseCsvResults(csvData: string): any[] {
    const lines = csvData.split('\n');
    const headers = lines[0].split(',');
    const results = [];

    for (let i = 1; i < lines.length; i++) {
      if (lines[i].trim()) {
        const values = lines[i].split(',');
        const result: any = {};
        
        headers.forEach((header, index) => {
          result[header.trim().toLowerCase()] = values[index]?.trim() || '';
        });
        
        results.push(result);
      }
    }

    return results;
  }

  /**
   * Process bulk results into structured format
   */
  private processBulkResults(rawResults: any[]): BulkVerificationResult {
    const results: EmailVerificationResult[] = [];
    let validCount = 0;
    let invalidCount = 0;

    for (const raw of rawResults) {
      const result = this.mapBulkVerificationResponse(raw);
      results.push(result);
      
      if (result.status === 'valid') {
        validCount++;
      } else if (result.status === 'invalid') {
        invalidCount++;
      }
    }

    return {
      results,
      totalCount: results.length,
      validCount,
      invalidCount,
      creditsUsed: results.length,
    };
  }

  /**
   * Map API response to EmailVerificationResult
   */
  private mapVerificationResponse(data: any): EmailVerificationResult {
    return {
      email: data.address,
      status: this.mapStatus(data.status),
      subStatus: data.sub_status || '',
      account: data.account || '',
      domain: data.domain || '',
      firstName: data.firstname || undefined,
      lastName: data.lastname || undefined,
      gender: data.gender || undefined,
      location: data.location || undefined,
      creationDate: data.creation_date || undefined,
      processingTime: parseFloat(data.processingtime) || 0,
    };
  }

  /**
   * Map bulk verification response to EmailVerificationResult
   */
  private mapBulkVerificationResponse(data: any): EmailVerificationResult {
    return {
      email: data.email || data.address,
      status: this.mapStatus(data.zerobounce_status || data.status),
      subStatus: data.zerobounce_sub_status || data.sub_status || '',
      account: data.account || '',
      domain: data.domain || '',
      firstName: data.firstname || undefined,
      lastName: data.lastname || undefined,
      gender: data.gender || undefined,
      location: data.location || undefined,
      creationDate: data.creation_date || undefined,
      processingTime: 0, // Not provided in bulk results
    };
  }

  /**
   * Map ZeroBounce status to our standardized status
   */
  private mapStatus(status: string): EmailVerificationResult['status'] {
    const statusMap: Record<string, EmailVerificationResult['status']> = {
      'valid': 'valid',
      'invalid': 'invalid',
      'catch-all': 'catch-all',
      'unknown': 'unknown',
      'spamtrap': 'spamtrap',
      'abuse': 'abuse',
      'do_not_mail': 'do_not_mail',
    };

    return statusMap[status?.toLowerCase()] || 'unknown';
  }

  /**
   * Get account credits
   */
  async getCredits(): Promise<number> {
    try {
      const response = await this.client.get('/getcredits', {
        params: {
          api_key: this.apiKey,
        },
      });

      return response.data.Credits || 0;
    } catch (error) {
      throw new Error(`Failed to get credits: ${error}`);
    }
  }

  /**
   * Get API usage statistics
   */
  async getApiUsage(startDate: string, endDate: string): Promise<any> {
    try {
      const response = await this.client.get('/getapiusage', {
        params: {
          api_key: this.apiKey,
          start_date: startDate,
          end_date: endDate,
        },
      });

      return response.data;
    } catch (error) {
      throw new Error(`Failed to get API usage: ${error}`);
    }
  }
}
