import * as brevo from '@getbrevo/brevo';
import {
  TransactionalEmailOptions,
  EmailCampaignOptions,
  EmailAddress,
  EmailStatistics,
  ContactListOptions,
  ContactOptions,
} from './types';

export class BrevoClient {
  private transactionalEmailsApi: brevo.TransactionalEmailsApi;
  private emailCampaignsApi: brevo.EmailCampaignsApi;
  private contactsApi: brevo.ContactsApi;
  private listsApi: brevo.ListsApi;
  private senderEmail: string;
  private senderName: string;

  constructor(apiKey: string, senderEmail: string, senderName: string) {
    this.senderEmail = senderEmail;
    this.senderName = senderName;

    // Configure Brevo API client
    const apiInstance = brevo.ApiClient.instance;
    const apiKeyAuth = apiInstance.authentications['api-key'];
    apiKeyAuth.apiKey = apiKey;

    // Initialize API instances
    this.transactionalEmailsApi = new brevo.TransactionalEmailsApi();
    this.emailCampaignsApi = new brevo.EmailCampaignsApi();
    this.contactsApi = new brevo.ContactsApi();
    this.listsApi = new brevo.ListsApi();
  }

  /**
   * Send a transactional email
   */
  async sendTransactionalEmail(options: TransactionalEmailOptions): Promise<any> {
    try {
      const sendSmtpEmail = new brevo.SendSmtpEmail();
      
      // Set sender
      sendSmtpEmail.sender = {
        email: this.senderEmail,
        name: this.senderName,
      };

      // Set recipients
      sendSmtpEmail.to = this.formatEmailAddresses(
        Array.isArray(options.to) ? options.to : [options.to]
      );

      // Set optional CC and BCC
      if (options.cc) {
        sendSmtpEmail.cc = this.formatEmailAddresses(options.cc);
      }
      if (options.bcc) {
        sendSmtpEmail.bcc = this.formatEmailAddresses(options.bcc);
      }

      // Set reply-to
      if (options.replyTo) {
        sendSmtpEmail.replyTo = this.formatEmailAddress(options.replyTo);
      }

      // Set subject
      sendSmtpEmail.subject = options.subject;

      // Set content (template or direct content)
      if (options.templateId) {
        sendSmtpEmail.templateId = options.templateId;
        if (options.params) {
          sendSmtpEmail.params = options.params;
        }
      } else {
        if (options.htmlContent) {
          sendSmtpEmail.htmlContent = options.htmlContent;
        }
        if (options.textContent) {
          sendSmtpEmail.textContent = options.textContent;
        }
      }

      // Set tags
      if (options.tags) {
        sendSmtpEmail.tags = options.tags;
      }

      const result = await this.transactionalEmailsApi.sendTransacEmail(sendSmtpEmail);
      return result;
    } catch (error) {
      throw new Error(`Failed to send transactional email: ${error}`);
    }
  }

  /**
   * Create an email campaign
   */
  async createEmailCampaign(options: EmailCampaignOptions): Promise<any> {
    try {
      const emailCampaign = new brevo.CreateEmailCampaign();
      
      emailCampaign.name = options.name;
      emailCampaign.subject = options.subject;
      emailCampaign.htmlContent = options.htmlContent;
      
      if (options.textContent) {
        emailCampaign.textContent = options.textContent;
      }

      // Set sender
      emailCampaign.sender = {
        email: this.senderEmail,
        name: this.senderName,
      };

      // Set recipients (lists)
      emailCampaign.recipients = {
        listIds: options.listIds,
      };

      if (options.excludedListIds) {
        emailCampaign.recipients.exclusionListIds = options.excludedListIds;
      }

      // Set scheduling
      if (options.scheduledAt) {
        emailCampaign.scheduledAt = options.scheduledAt.toISOString();
      }

      if (options.sendAtBestTime) {
        emailCampaign.sendAtBestTime = options.sendAtBestTime;
      }

      // Set UTM tracking
      if (options.utmCampaign) {
        emailCampaign.utmCampaign = options.utmCampaign;
      }

      // Set tags
      if (options.tags) {
        emailCampaign.tags = options.tags;
      }

      const result = await this.emailCampaignsApi.createEmailCampaign(emailCampaign);
      return result;
    } catch (error) {
      throw new Error(`Failed to create email campaign: ${error}`);
    }
  }

  /**
   * Send an existing email campaign
   */
  async sendEmailCampaign(campaignId: number): Promise<any> {
    try {
      const result = await this.emailCampaignsApi.sendEmailCampaignNow(campaignId);
      return result;
    } catch (error) {
      throw new Error(`Failed to send email campaign: ${error}`);
    }
  }

  /**
   * Get email campaign statistics
   */
  async getEmailStatistics(campaignId: number): Promise<EmailStatistics> {
    try {
      const result = await this.emailCampaignsApi.getEmailCampaign(campaignId);
      
      return {
        campaignId: campaignId.toString(),
        delivered: result.statistics?.delivered || 0,
        hardBounces: result.statistics?.hardBounces || 0,
        softBounces: result.statistics?.softBounces || 0,
        clicks: result.statistics?.clicks || 0,
        uniqueClicks: result.statistics?.uniqueClicks || 0,
        opens: result.statistics?.opens || 0,
        uniqueOpens: result.statistics?.uniqueOpens || 0,
        spam: result.statistics?.spam || 0,
        blocked: result.statistics?.blocked || 0,
        invalid: result.statistics?.invalid || 0,
        unsubscribed: result.statistics?.unsubscribed || 0,
      };
    } catch (error) {
      throw new Error(`Failed to get email statistics: ${error}`);
    }
  }

  /**
   * Create a contact list
   */
  async createContactList(options: ContactListOptions): Promise<any> {
    try {
      const createList = new brevo.CreateList();
      createList.name = options.name;
      
      if (options.folderId) {
        createList.folderId = options.folderId;
      }

      const result = await this.listsApi.createList(createList);
      return result;
    } catch (error) {
      throw new Error(`Failed to create contact list: ${error}`);
    }
  }

  /**
   * Add a contact to lists
   */
  async createContact(options: ContactOptions): Promise<any> {
    try {
      const createContact = new brevo.CreateContact();
      createContact.email = options.email;
      
      if (options.attributes) {
        createContact.attributes = options.attributes;
      }

      if (options.listIds) {
        createContact.listIds = options.listIds;
      }

      if (options.updateEnabled !== undefined) {
        createContact.updateEnabled = options.updateEnabled;
      }

      if (options.emailBlacklisted !== undefined) {
        createContact.emailBlacklisted = options.emailBlacklisted;
      }

      if (options.smsBlacklisted !== undefined) {
        createContact.smsBlacklisted = options.smsBlacklisted;
      }

      const result = await this.contactsApi.createContact(createContact);
      return result;
    } catch (error) {
      throw new Error(`Failed to create contact: ${error}`);
    }
  }

  /**
   * Update a contact
   */
  async updateContact(email: string, options: Partial<ContactOptions>): Promise<any> {
    try {
      const updateContact = new brevo.UpdateContact();
      
      if (options.attributes) {
        updateContact.attributes = options.attributes;
      }

      if (options.listIds) {
        updateContact.listIds = options.listIds;
      }

      if (options.emailBlacklisted !== undefined) {
        updateContact.emailBlacklisted = options.emailBlacklisted;
      }

      if (options.smsBlacklisted !== undefined) {
        updateContact.smsBlacklisted = options.smsBlacklisted;
      }

      const result = await this.contactsApi.updateContact(email, updateContact);
      return result;
    } catch (error) {
      throw new Error(`Failed to update contact: ${error}`);
    }
  }

  /**
   * Delete a contact
   */
  async deleteContact(email: string): Promise<any> {
    try {
      const result = await this.contactsApi.deleteContact(email);
      return result;
    } catch (error) {
      throw new Error(`Failed to delete contact: ${error}`);
    }
  }

  /**
   * Get contact information
   */
  async getContact(email: string): Promise<any> {
    try {
      const result = await this.contactsApi.getContactInfo(email);
      return result;
    } catch (error) {
      throw new Error(`Failed to get contact: ${error}`);
    }
  }

  /**
   * Get all contact lists
   */
  async getContactLists(limit = 50, offset = 0): Promise<any> {
    try {
      const result = await this.listsApi.getLists({ limit, offset });
      return result;
    } catch (error) {
      throw new Error(`Failed to get contact lists: ${error}`);
    }
  }

  /**
   * Add contacts to a list
   */
  async addContactsToList(listId: number, emails: string[]): Promise<any> {
    try {
      const contactEmails = new brevo.AddContactToList();
      contactEmails.emails = emails;

      const result = await this.listsApi.addContactToList(listId, contactEmails);
      return result;
    } catch (error) {
      throw new Error(`Failed to add contacts to list: ${error}`);
    }
  }

  /**
   * Remove contacts from a list
   */
  async removeContactsFromList(listId: number, emails: string[]): Promise<any> {
    try {
      const contactEmails = new brevo.RemoveContactFromList();
      contactEmails.emails = emails;

      const result = await this.listsApi.removeContactFromList(listId, contactEmails);
      return result;
    } catch (error) {
      throw new Error(`Failed to remove contacts from list: ${error}`);
    }
  }

  /**
   * Format email addresses for Brevo API
   */
  private formatEmailAddresses(addresses: EmailAddress[]): any[] {
    return addresses.map(addr => this.formatEmailAddress(addr));
  }

  /**
   * Format single email address for Brevo API
   */
  private formatEmailAddress(address: EmailAddress): any {
    return {
      email: address.email,
      name: address.name || undefined,
    };
  }

  /**
   * Get account information
   */
  async getAccountInfo(): Promise<any> {
    try {
      const accountApi = new brevo.AccountApi();
      const result = await accountApi.getAccount();
      return result;
    } catch (error) {
      throw new Error(`Failed to get account info: ${error}`);
    }
  }

  /**
   * Get email templates
   */
  async getEmailTemplates(templateStatus?: 'true' | 'false', limit = 50, offset = 0): Promise<any> {
    try {
      const result = await this.transactionalEmailsApi.getSmtpTemplates({
        templateStatus,
        limit,
        offset,
      });
      return result;
    } catch (error) {
      throw new Error(`Failed to get email templates: ${error}`);
    }
  }
}
