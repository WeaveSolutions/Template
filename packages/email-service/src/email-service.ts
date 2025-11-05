import { BrevoClient } from './brevo-client';
import { ZeroBounceClient } from './zerobounce-client';
import {
  EmailServiceConfig,
  TransactionalEmailOptions,
  EmailCampaignOptions,
  EmailVerificationResult,
  BulkVerificationResult,
  EmailAddress,
  ContactOptions,
} from './types';

export class EmailService {
  private brevoClient: BrevoClient;
  private zeroBounceClient: ZeroBounceClient;
  private autoVerify: boolean;

  constructor(config: EmailServiceConfig, autoVerify = true) {
    this.brevoClient = new BrevoClient(
      config.brevo.apiKey,
      config.brevo.senderEmail,
      config.brevo.senderName
    );
    this.zeroBounceClient = new ZeroBounceClient(config.zeroBounce.apiKey);
    this.autoVerify = autoVerify;
  }

  /**
   * Send a transactional email with optional verification
   */
  async sendTransactionalEmail(options: TransactionalEmailOptions): Promise<any> {
    if (this.autoVerify) {
      // Verify recipient emails before sending
      const recipients = Array.isArray(options.to) ? options.to : [options.to];
      const verifiedRecipients = await this.verifyAndFilterEmails(recipients);
      
      if (verifiedRecipients.length === 0) {
        throw new Error('No valid email addresses found after verification');
      }

      // Update options with verified emails
      options.to = verifiedRecipients.length === 1 ? verifiedRecipients[0] : verifiedRecipients;
    }

    return await this.brevoClient.sendTransactionalEmail(options);
  }

  /**
   * Create and optionally send an email campaign with verified recipients
   */
  async createEmailCampaign(
    options: EmailCampaignOptions,
    verifyListEmails = true
  ): Promise<{ campaign: any; verificationResults?: BulkVerificationResult }> {
    let verificationResults: BulkVerificationResult | undefined;

    if (verifyListEmails) {
      // Get emails from the specified lists
      const listEmails = await this.getEmailsFromLists(options.listIds);
      
      if (listEmails.length > 0) {
        // Verify all emails in the lists
        verificationResults = await this.zeroBounceClient.verifyEmailBulk(listEmails);
        
        // Create a new list with only verified emails
        const verifiedEmails = verificationResults.results
          .filter(result => result.status === 'valid')
          .map(result => result.email);

        if (verifiedEmails.length > 0) {
          const verifiedListId = await this.createVerifiedList(
            `${options.name} - Verified`,
            verifiedEmails
          );
          
          // Update campaign to use verified list
          options.listIds = [verifiedListId];
        } else {
          throw new Error('No valid email addresses found in the specified lists');
        }
      }
    }

    const campaign = await this.brevoClient.createEmailCampaign(options);
    
    return {
      campaign,
      verificationResults,
    };
  }

  /**
   * Verify a single email address
   */
  async verifyEmail(email: string): Promise<EmailVerificationResult> {
    return await this.zeroBounceClient.verifyEmail(email);
  }

  /**
   * Verify multiple email addresses
   */
  async verifyEmailBulk(emails: string[]): Promise<BulkVerificationResult> {
    return await this.zeroBounceClient.verifyEmailBulk(emails);
  }

  /**
   * Add contacts with email verification
   */
  async addVerifiedContacts(
    contacts: Array<{ email: string; attributes?: Record<string, any> }>,
    listIds?: number[]
  ): Promise<{ added: ContactOptions[]; invalid: string[] }> {
    const emails = contacts.map(c => c.email);
    const verificationResults = await this.zeroBounceClient.verifyEmailBulk(emails);
    
    const added: ContactOptions[] = [];
    const invalid: string[] = [];

    for (const result of verificationResults.results) {
      if (result.status === 'valid') {
        const contact = contacts.find(c => c.email === result.email);
        if (contact) {
          const contactOptions: ContactOptions = {
            email: contact.email,
            attributes: contact.attributes,
            listIds,
          };
          
          await this.brevoClient.createContact(contactOptions);
          added.push(contactOptions);
        }
      } else {
        invalid.push(result.email);
      }
    }

    return { added, invalid };
  }

  /**
   * Clean email lists by removing invalid emails
   */
  async cleanEmailLists(listIds: number[]): Promise<{
    listId: number;
    totalEmails: number;
    validEmails: number;
    removedEmails: string[];
  }[]> {
    const results = [];

    for (const listId of listIds) {
      const emails = await this.getEmailsFromLists([listId]);
      const verificationResults = await this.zeroBounceClient.verifyEmailBulk(emails);
      
      const invalidEmails = verificationResults.results
        .filter(result => result.status !== 'valid')
        .map(result => result.email);

      if (invalidEmails.length > 0) {
        await this.brevoClient.removeContactsFromList(listId, invalidEmails);
      }

      results.push({
        listId,
        totalEmails: emails.length,
        validEmails: verificationResults.validCount,
        removedEmails: invalidEmails,
      });
    }

    return results;
  }

  /**
   * Get email statistics with verification insights
   */
  async getEmailStatistics(campaignId: number): Promise<any> {
    const stats = await this.brevoClient.getEmailStatistics(campaignId);
    
    // Calculate additional metrics
    const deliverabilityRate = stats.delivered / (stats.delivered + stats.hardBounces + stats.softBounces) * 100;
    const engagementRate = (stats.uniqueOpens + stats.uniqueClicks) / stats.delivered * 100;
    
    return {
      ...stats,
      deliverabilityRate: Math.round(deliverabilityRate * 100) / 100,
      engagementRate: Math.round(engagementRate * 100) / 100,
    };
  }

  /**
   * Verify and filter email addresses, returning only valid ones
   */
  private async verifyAndFilterEmails(emails: EmailAddress[]): Promise<EmailAddress[]> {
    const emailStrings = emails.map(e => e.email);
    const verificationResults = await this.zeroBounceClient.verifyEmailBulk(emailStrings);
    
    const validEmails: EmailAddress[] = [];
    
    for (const result of verificationResults.results) {
      if (result.status === 'valid') {
        const originalEmail = emails.find(e => e.email === result.email);
        if (originalEmail) {
          validEmails.push(originalEmail);
        }
      }
    }
    
    return validEmails;
  }

  /**
   * Get all emails from specified lists
   */
  private async getEmailsFromLists(listIds: number[]): Promise<string[]> {
    // This is a simplified implementation
    // In practice, you'd need to paginate through all contacts in the lists
    const allEmails: string[] = [];
    
    // Note: Brevo API doesn't provide a direct way to get all emails from a list
    // You would need to implement pagination through contacts API
    // This is a placeholder for the actual implementation
    
    return allEmails;
  }

  /**
   * Create a new list with verified emails
   */
  private async createVerifiedList(name: string, emails: string[]): Promise<number> {
    const list = await this.brevoClient.createContactList({ name });
    
    if (emails.length > 0) {
      await this.brevoClient.addContactsToList(list.id, emails);
    }
    
    return list.id;
  }

  /**
   * Get Zero Bounce credits
   */
  async getVerificationCredits(): Promise<number> {
    return await this.zeroBounceClient.getCredits();
  }

  /**
   * Get Brevo account information
   */
  async getAccountInfo(): Promise<any> {
    return await this.brevoClient.getAccountInfo();
  }

  /**
   * Batch process email verification with rate limiting
   */
  async batchVerifyEmails(
    emails: string[],
    batchSize = 100,
    delayMs = 1000
  ): Promise<BulkVerificationResult> {
    const allResults: EmailVerificationResult[] = [];
    let totalCreditsUsed = 0;

    for (let i = 0; i < emails.length; i += batchSize) {
      const batch = emails.slice(i, i + batchSize);
      const batchResults = await this.zeroBounceClient.verifyEmailBulk(batch);
      
      allResults.push(...batchResults.results);
      totalCreditsUsed += batchResults.creditsUsed;

      // Add delay between batches to respect rate limits
      if (i + batchSize < emails.length) {
        await new Promise(resolve => setTimeout(resolve, delayMs));
      }
    }

    const validCount = allResults.filter(r => r.status === 'valid').length;
    const invalidCount = allResults.filter(r => r.status === 'invalid').length;

    return {
      results: allResults,
      totalCount: allResults.length,
      validCount,
      invalidCount,
      creditsUsed: totalCreditsUsed,
    };
  }
}
