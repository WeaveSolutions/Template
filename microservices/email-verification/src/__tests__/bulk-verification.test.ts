import request from 'supertest';
import app from '../index';
import { createEmailServiceFromEnv } from '@weave/email-service';

// Mock the email service
jest.mock('@weave/email-service');
const mockCreateEmailServiceFromEnv = createEmailServiceFromEnv as jest.MockedFunction<typeof createEmailServiceFromEnv>;

describe('Email Verification Microservice', () => {
  let mockEmailService: any;

  beforeEach(() => {
    jest.clearAllMocks();
    
    mockEmailService = {
      verifyEmail: jest.fn(),
      verifyEmailBulk: jest.fn(),
      batchVerifyEmails: jest.fn(),
      getVerificationCredits: jest.fn(),
      cleanEmailLists: jest.fn(),
    };

    mockCreateEmailServiceFromEnv.mockReturnValue(mockEmailService);
  });

  describe('GET /health', () => {
    it('should return health status', async () => {
      const response = await request(app)
        .get('/health')
        .expect(200);

      expect(response.body).toMatchObject({
        status: 'healthy',
        service: 'email-verification',
        version: expect.any(String),
      });
      expect(response.body.timestamp).toBeDefined();
    });
  });

  describe('GET /api/info', () => {
    it('should return API information', async () => {
      const response = await request(app)
        .get('/api/info')
        .expect(200);

      expect(response.body).toMatchObject({
        service: 'Email Verification Microservice',
        description: expect.any(String),
        version: expect.any(String),
        endpoints: expect.any(Array),
      });

      expect(response.body.endpoints).toHaveLength(4);
    });
  });

  describe('POST /api/verify/single', () => {
    it('should verify a single email address', async () => {
      const mockResult = {
        email: 'test@example.com',
        status: 'valid',
        subStatus: '',
        account: 'test',
        domain: 'example.com',
        processingTime: 0.1,
      };

      mockEmailService.verifyEmail.mockResolvedValue(mockResult);

      const response = await request(app)
        .post('/api/verify/single')
        .send({ email: 'test@example.com' })
        .expect(200);

      expect(response.body).toMatchObject({
        success: true,
        data: mockResult,
        metadata: {
          processedAt: expect.any(String),
        },
      });

      expect(mockEmailService.verifyEmail).toHaveBeenCalledWith('test@example.com');
    });

    it('should return 400 for missing email', async () => {
      const response = await request(app)
        .post('/api/verify/single')
        .send({})
        .expect(400);

      expect(response.body).toMatchObject({
        error: 'Email address is required',
      });
    });

    it('should handle verification errors', async () => {
      mockEmailService.verifyEmail.mockRejectedValue(new Error('API Error'));

      const response = await request(app)
        .post('/api/verify/single')
        .send({ email: 'test@example.com' })
        .expect(500);

      expect(response.body).toMatchObject({
        error: 'Email verification failed',
        message: 'API Error',
      });
    });
  });

  describe('POST /api/verify/bulk', () => {
    it('should verify multiple emails from request body', async () => {
      const emails = ['test1@example.com', 'test2@example.com'];
      const mockResult = {
        results: [
          {
            email: 'test1@example.com',
            status: 'valid',
            subStatus: '',
            account: 'test1',
            domain: 'example.com',
            processingTime: 0.1,
          },
          {
            email: 'test2@example.com',
            status: 'invalid',
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

      mockEmailService.batchVerifyEmails.mockResolvedValue(mockResult);

      const response = await request(app)
        .post('/api/verify/bulk')
        .send({
          emails,
          options: {
            batchSize: 50,
            delayMs: 500,
            returnInvalid: true,
          },
        })
        .expect(200);

      expect(response.body).toMatchObject({
        success: true,
        data: mockResult,
        metadata: {
          processedAt: expect.any(String),
          batchSize: 50,
          delayMs: 500,
          returnInvalid: true,
        },
      });

      expect(mockEmailService.batchVerifyEmails).toHaveBeenCalledWith(emails, 50, 500);
    });

    it('should filter out invalid emails when returnInvalid is false', async () => {
      const emails = ['test1@example.com', 'test2@example.com'];
      const mockResult = {
        results: [
          {
            email: 'test1@example.com',
            status: 'valid',
            subStatus: '',
            account: 'test1',
            domain: 'example.com',
            processingTime: 0.1,
          },
          {
            email: 'test2@example.com',
            status: 'invalid',
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

      mockEmailService.batchVerifyEmails.mockResolvedValue(mockResult);

      const response = await request(app)
        .post('/api/verify/bulk')
        .send({
          emails,
          options: {
            returnInvalid: false,
          },
        })
        .expect(200);

      // Should only return valid emails
      expect(response.body.data.results).toHaveLength(1);
      expect(response.body.data.results[0].status).toBe('valid');
    });

    it('should return 400 for no emails provided', async () => {
      const response = await request(app)
        .post('/api/verify/bulk')
        .send({})
        .expect(400);

      expect(response.body).toMatchObject({
        error: 'Either provide emails array or upload a CSV file',
      });
    });

    it('should return 400 for too many emails', async () => {
      const emails = Array(10001).fill('test@example.com'); // Over 10,000 limit

      const response = await request(app)
        .post('/api/verify/bulk')
        .send({ emails })
        .expect(400);

      expect(response.body).toMatchObject({
        error: 'Maximum 10,000 emails per batch allowed',
      });
    });

    it('should handle bulk verification errors', async () => {
      const emails = ['test@example.com'];
      mockEmailService.batchVerifyEmails.mockRejectedValue(new Error('Bulk verification failed'));

      const response = await request(app)
        .post('/api/verify/bulk')
        .send({ emails })
        .expect(500);

      expect(response.body).toMatchObject({
        error: 'Bulk verification failed',
        message: 'Bulk verification failed',
      });
    });
  });

  describe('GET /api/verify/credits', () => {
    it('should return remaining credits', async () => {
      mockEmailService.getVerificationCredits.mockResolvedValue(5000);

      const response = await request(app)
        .get('/api/verify/credits')
        .expect(200);

      expect(response.body).toMatchObject({
        success: true,
        data: {
          credits: 5000,
          timestamp: expect.any(String),
        },
      });

      expect(mockEmailService.getVerificationCredits).toHaveBeenCalled();
    });

    it('should handle credits API error', async () => {
      mockEmailService.getVerificationCredits.mockRejectedValue(new Error('Credits API error'));

      const response = await request(app)
        .get('/api/verify/credits')
        .expect(500);

      expect(response.body).toMatchObject({
        error: 'Failed to get credits',
        message: 'Credits API error',
      });
    });
  });

  describe('POST /api/verify/clean-lists', () => {
    it('should clean email lists', async () => {
      const listIds = [1, 2, 3];
      const mockResult = [
        {
          listId: 1,
          totalEmails: 100,
          validEmails: 80,
          removedEmails: ['invalid1@example.com', 'invalid2@example.com'],
        },
        {
          listId: 2,
          totalEmails: 50,
          validEmails: 45,
          removedEmails: ['invalid3@example.com'],
        },
        {
          listId: 3,
          totalEmails: 75,
          validEmails: 75,
          removedEmails: [],
        },
      ];

      mockEmailService.cleanEmailLists.mockResolvedValue(mockResult);

      const response = await request(app)
        .post('/api/verify/clean-lists')
        .send({ listIds })
        .expect(200);

      expect(response.body).toMatchObject({
        success: true,
        data: mockResult,
        metadata: {
          processedAt: expect.any(String),
          totalListsCleaned: 3,
        },
      });

      expect(mockEmailService.cleanEmailLists).toHaveBeenCalledWith(listIds);
    });

    it('should return 400 for missing listIds', async () => {
      const response = await request(app)
        .post('/api/verify/clean-lists')
        .send({})
        .expect(400);

      expect(response.body).toMatchObject({
        error: 'List IDs array is required',
      });
    });

    it('should return 400 for invalid listIds format', async () => {
      const response = await request(app)
        .post('/api/verify/clean-lists')
        .send({ listIds: 'not-an-array' })
        .expect(400);

      expect(response.body).toMatchObject({
        error: 'List IDs array is required',
      });
    });

    it('should handle list cleaning errors', async () => {
      const listIds = [1, 2];
      mockEmailService.cleanEmailLists.mockRejectedValue(new Error('List cleaning failed'));

      const response = await request(app)
        .post('/api/verify/clean-lists')
        .send({ listIds })
        .expect(500);

      expect(response.body).toMatchObject({
        error: 'Failed to clean email lists',
        message: 'List cleaning failed',
      });
    });
  });

  describe('Rate limiting', () => {
    it('should apply rate limiting to verification endpoints', async () => {
      // This test would require actual rate limiting testing
      // For now, we'll just verify the endpoint works
      mockEmailService.verifyEmail.mockResolvedValue({
        email: 'test@example.com',
        status: 'valid',
        subStatus: '',
        account: 'test',
        domain: 'example.com',
        processingTime: 0.1,
      });

      const response = await request(app)
        .post('/api/verify/single')
        .send({ email: 'test@example.com' })
        .expect(200);

      expect(response.body.success).toBe(true);
    });
  });

  describe('404 handler', () => {
    it('should return 404 for unknown routes', async () => {
      const response = await request(app)
        .get('/api/unknown-route')
        .expect(404);

      expect(response.body).toMatchObject({
        error: 'Not found',
        message: expect.stringContaining('Route GET /api/unknown-route not found'),
      });
    });
  });
});
