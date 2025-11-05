import { EmailService } from '../email-service';
import { BrevoClient } from '../brevo-client';
import { ZeroBounceClient } from '../zerobounce-client';
import { EmailServiceConfig } from '../types';

// Mock the clients
jest.mock('../brevo-client');
jest.mock('../zerobounce-client');

const MockedBrevoClient = BrevoClient as jest.MockedClass<typeof BrevoClient>;
const MockedZeroBounceClient = ZeroBounceClient as jest.MockedClass<typeof ZeroBounceClient>;

describe('EmailService', () => {
  let emailService: EmailService;
  let mockBrevoClient: jest.Mocked<BrevoClient>;
  let mockZeroBounceClient: jest.Mocked<ZeroBounceClient>;

  const mockConfig: EmailServiceConfig = {
    brevo: {
      apiKey: 'test-brevo-key',
      senderEmail: 'test@example.com',
      senderName: 'Test Sender',
    },
    zeroBounce: {
      apiKey: 'test-zerobounce-key',
    },
  };

  beforeEach(() => {
    jest.clearAllMocks();
    
    mockBrevoClient = {
      sendTransactionalEmail: jest.fn(),
      createEmailCampaign: jest.fn(),
      getEmailStatistics: jest.fn(),
      createContactList: jest.fn(),
      addContactsToList: jest.fn(),
      removeContactsFromList: jest.fn(),
      createContact: jest.fn(),
      getAccountInfo: jest.fn(),
    } as any;

    mockZeroBounceClient = {
      verifyEmail: jest.fn(),
      verifyEmailBulk: jest.fn(),
      getCredits: jest.fn(),
    } as any;

    MockedBrevoClient.mockImplementation(() => mockBrevoClient);
    MockedZeroBounceClient.mockImplementation(() => mockZeroBounceClient);

    emailService = new EmailService(mockConfig, true);
  });

  describe('sendTransactionalEmail', () => {
    it('should verify emails before sending when autoVerify is enabled', async () => {
      const mockVerificationResult = {
        results: [
          {
            email: 'valid@example.com',
            status: 'valid' as const,
            subStatus: '',
            account: 'valid',
            domain: 'example.com',
            processingTime: 0.1,
          },
        ],
        totalCount: 1,
        validCount: 1,
        invalidCount: 0,
        creditsUsed: 1,
      };

      const mockEmailResult = { messageId: '123' };

      mockZeroBounceClient.verifyEmailBulk.mockResolvedValue(mockVerificationResult);
      mockBrevoClient.sendTransactionalEmail.mockResolvedValue(mockEmailResult);

      const result = await emailService.sendTransactionalEmail({
        to: { email: 'valid@example.com', name: 'Test User' },
        subject: 'Test Email',
        htmlContent: '<h1>Test</h1>',
      });

      expect(mockZeroBounceClient.verifyEmailBulk).toHaveBeenCalledWith(['valid@example.com']);
      expect(mockBrevoClient.sendTransactionalEmail).toHaveBeenCalledWith({
        to: { email: 'valid@example.com', name: 'Test User' },
        subject: 'Test Email',
        htmlContent: '<h1>Test</h1>',
      });
      expect(result).toEqual(mockEmailResult);
    });

    it('should throw error if no valid emails found after verification', async () => {
      const mockVerificationResult = {
        results: [
          {
            email: 'invalid@example.com',
            status: 'invalid' as const,
            subStatus: 'mailbox_not_found',
            account: 'invalid',
            domain: 'example.com',
            processingTime: 0.1,
          },
        ],
        totalCount: 1,
        validCount: 0,
        invalidCount: 1,
        creditsUsed: 1,
      };

      mockZeroBounceClient.verifyEmailBulk.mockResolvedValue(mockVerificationResult);

      await expect(
        emailService.sendTransactionalEmail({
          to: { email: 'invalid@example.com', name: 'Test User' },
          subject: 'Test Email',
          htmlContent: '<h1>Test</h1>',
        })
      ).rejects.toThrow('No valid email addresses found after verification');

      expect(mockBrevoClient.sendTransactionalEmail).not.toHaveBeenCalled();
    });

    it('should handle multiple recipients', async () => {
      const mockVerificationResult = {
        results: [
          {
            email: 'valid1@example.com',
            status: 'valid' as const,
            subStatus: '',
            account: 'valid1',
            domain: 'example.com',
            processingTime: 0.1,
          },
          {
            email: 'invalid@example.com',
            status: 'invalid' as const,
            subStatus: 'mailbox_not_found',
            account: 'invalid',
            domain: 'example.com',
            processingTime: 0.1,
          },
          {
            email: 'valid2@example.com',
            status: 'valid' as const,
            subStatus: '',
            account: 'valid2',
            domain: 'example.com',
            processingTime: 0.1,
          },
        ],
        totalCount: 3,
        validCount: 2,
        invalidCount: 1,
        creditsUsed: 3,
      };

      mockZeroBounceClient.verifyEmailBulk.mockResolvedValue(mockVerificationResult);
      mockBrevoClient.sendTransactionalEmail.mockResolvedValue({ messageId: '123' });

      await emailService.sendTransactionalEmail({
        to: [
          { email: 'valid1@example.com', name: 'User 1' },
          { email: 'invalid@example.com', name: 'User 2' },
          { email: 'valid2@example.com', name: 'User 3' },
        ],
        subject: 'Test Email',
        htmlContent: '<h1>Test</h1>',
      });

      expect(mockZeroBounceClient.verifyEmailBulk).toHaveBeenCalledWith([
        'valid1@example.com',
        'invalid@example.com',
        'valid2@example.com',
      ]);

      expect(mockBrevoClient.sendTransactionalEmail).toHaveBeenCalledWith({
        to: [
          { email: 'valid1@example.com', name: 'User 1' },
          { email: 'valid2@example.com', name: 'User 3' },
        ],
        subject: 'Test Email',
        htmlContent: '<h1>Test</h1>',
      });
    });
  });

  describe('verifyEmail', () => {
    it('should verify a single email address', async () => {
      const mockResult = {
        email: 'test@example.com',
        status: 'valid' as const,
        subStatus: '',
        account: 'test',
        domain: 'example.com',
        processingTime: 0.1,
      };

      mockZeroBounceClient.verifyEmail.mockResolvedValue(mockResult);

      const result = await emailService.verifyEmail('test@example.com');

      expect(mockZeroBounceClient.verifyEmail).toHaveBeenCalledWith('test@example.com');
      expect(result).toEqual(mockResult);
    });
  });

  describe('verifyEmailBulk', () => {
    it('should verify multiple email addresses', async () => {
      const emails = ['test1@example.com', 'test2@example.com'];
      const mockResult = {
        results: [
          {
            email: 'test1@example.com',
            status: 'valid' as const,
            subStatus: '',
            account: 'test1',
            domain: 'example.com',
            processingTime: 0.1,
          },
          {
            email: 'test2@example.com',
            status: 'invalid' as const,
            subStatus: 'mailbox_not_found',
            account: 'test2',
            domain: 'example.com',
            processingTime: 0.1,
          },
        ],
        totalCount: 2,
        validCount: 1,
        invalidCount: 1,
        creditsUsed: 2,
      };

      mockZeroBounceClient.verifyEmailBulk.mockResolvedValue(mockResult);

      const result = await emailService.verifyEmailBulk(emails);

      expect(mockZeroBounceClient.verifyEmailBulk).toHaveBeenCalledWith(emails);
      expect(result).toEqual(mockResult);
    });
  });

  describe('addVerifiedContacts', () => {
    it('should add only verified contacts', async () => {
      const contacts = [
        { email: 'valid@example.com', attributes: { name: 'Valid User' } },
        { email: 'invalid@example.com', attributes: { name: 'Invalid User' } },
      ];

      const mockVerificationResult = {
        results: [
          {
            email: 'valid@example.com',
            status: 'valid' as const,
            subStatus: '',
            account: 'valid',
            domain: 'example.com',
            processingTime: 0.1,
          },
          {
            email: 'invalid@example.com',
            status: 'invalid' as const,
            subStatus: 'mailbox_not_found',
            account: 'invalid',
            domain: 'example.com',
            processingTime: 0.1,
          },
        ],
        totalCount: 2,
        validCount: 1,
        invalidCount: 1,
        creditsUsed: 2,
      };

      mockZeroBounceClient.verifyEmailBulk.mockResolvedValue(mockVerificationResult);
      mockBrevoClient.createContact.mockResolvedValue({ id: 1 });

      const result = await emailService.addVerifiedContacts(contacts, [1, 2]);

      expect(mockZeroBounceClient.verifyEmailBulk).toHaveBeenCalledWith([
        'valid@example.com',
        'invalid@example.com',
      ]);

      expect(mockBrevoClient.createContact).toHaveBeenCalledWith({
        email: 'valid@example.com',
        attributes: { name: 'Valid User' },
        listIds: [1, 2],
      });

      expect(result.added).toHaveLength(1);
      expect(result.invalid).toEqual(['invalid@example.com']);
    });
  });

  describe('getEmailStatistics', () => {
    it('should return enhanced statistics with calculated metrics', async () => {
      const mockStats = {
        campaignId: '123',
        delivered: 1000,
        hardBounces: 50,
        softBounces: 30,
        clicks: 200,
        uniqueClicks: 150,
        opens: 400,
        uniqueOpens: 300,
        spam: 5,
        blocked: 10,
        invalid: 15,
        unsubscribed: 20,
      };

      mockBrevoClient.getEmailStatistics.mockResolvedValue(mockStats);

      const result = await emailService.getEmailStatistics(123);

      expect(mockBrevoClient.getEmailStatistics).toHaveBeenCalledWith(123);
      
      // Deliverability rate: delivered / (delivered + hardBounces + softBounces) * 100
      // 1000 / (1000 + 50 + 30) * 100 = 92.59
      expect(result.deliverabilityRate).toBe(92.59);
      
      // Engagement rate: (uniqueOpens + uniqueClicks) / delivered * 100
      // (300 + 150) / 1000 * 100 = 45
      expect(result.engagementRate).toBe(45);
    });
  });

  describe('batchVerifyEmails', () => {
    it('should process emails in batches with delays', async () => {
      const emails = ['email1@test.com', 'email2@test.com', 'email3@test.com'];
      
      // Mock multiple batch responses
      mockZeroBounceClient.verifyEmailBulk
        .mockResolvedValueOnce({
          results: [
            {
              email: 'email1@test.com',
              status: 'valid' as const,
              subStatus: '',
              account: 'email1',
              domain: 'test.com',
              processingTime: 0.1,
            },
            {
              email: 'email2@test.com',
              status: 'valid' as const,
              subStatus: '',
              account: 'email2',
              domain: 'test.com',
              processingTime: 0.1,
            },
          ],
          totalCount: 2,
          validCount: 2,
          invalidCount: 0,
          creditsUsed: 2,
        })
        .mockResolvedValueOnce({
          results: [
            {
              email: 'email3@test.com',
              status: 'invalid' as const,
              subStatus: 'mailbox_not_found',
              account: 'email3',
              domain: 'test.com',
              processingTime: 0.1,
            },
          ],
          totalCount: 1,
          validCount: 0,
          invalidCount: 1,
          creditsUsed: 1,
        });

      const result = await emailService.batchVerifyEmails(emails, 2, 100);

      expect(mockZeroBounceClient.verifyEmailBulk).toHaveBeenCalledTimes(2);
      expect(result.totalCount).toBe(3);
      expect(result.validCount).toBe(2);
      expect(result.invalidCount).toBe(1);
      expect(result.creditsUsed).toBe(3);
    });
  });

  describe('getVerificationCredits', () => {
    it('should return remaining credits', async () => {
      mockZeroBounceClient.getCredits.mockResolvedValue(5000);

      const result = await emailService.getVerificationCredits();

      expect(mockZeroBounceClient.getCredits).toHaveBeenCalled();
      expect(result).toBe(5000);
    });
  });

  describe('getAccountInfo', () => {
    it('should return Brevo account information', async () => {
      const mockAccountInfo = {
        email: 'test@example.com',
        firstName: 'Test',
        lastName: 'User',
        companyName: 'Test Company',
      };

      mockBrevoClient.getAccountInfo.mockResolvedValue(mockAccountInfo);

      const result = await emailService.getAccountInfo();

      expect(mockBrevoClient.getAccountInfo).toHaveBeenCalled();
      expect(result).toEqual(mockAccountInfo);
    });
  });
});
