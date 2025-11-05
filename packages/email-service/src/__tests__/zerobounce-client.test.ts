import axios from 'axios';
import { ZeroBounceClient } from '../zerobounce-client';

// Mock axios
jest.mock('axios');
const mockedAxios = axios as jest.Mocked<typeof axios>;

describe('ZeroBounceClient', () => {
  let client: ZeroBounceClient;
  const apiKey = 'test-api-key';

  beforeEach(() => {
    jest.clearAllMocks();
    
    // Mock axios.create
    const mockAxiosInstance = {
      get: jest.fn(),
      post: jest.fn(),
    } as any;
    
    mockedAxios.create.mockReturnValue(mockAxiosInstance);
    mockedAxios.get = jest.fn();
    
    client = new ZeroBounceClient(apiKey);
  });

  describe('verifyEmail', () => {
    it('should verify a single email successfully', async () => {
      const mockResponse = {
        data: {
          address: 'test@example.com',
          status: 'valid',
          sub_status: '',
          account: 'test',
          domain: 'example.com',
          firstname: 'John',
          lastname: 'Doe',
          gender: 'male',
          location: 'US',
          creation_date: '2023-01-01',
          processingtime: '0.123',
        },
      };

      // Get the mocked instance
      const mockInstance = mockedAxios.create();
      mockInstance.get.mockResolvedValue(mockResponse);

      const result = await client.verifyEmail('test@example.com');

      expect(mockInstance.get).toHaveBeenCalledWith('/validate', {
        params: {
          api_key: apiKey,
          email: 'test@example.com',
          ip_address: '',
        },
      });

      expect(result).toEqual({
        email: 'test@example.com',
        status: 'valid',
        subStatus: '',
        account: 'test',
        domain: 'example.com',
        firstName: 'John',
        lastName: 'Doe',
        gender: 'male',
        location: 'US',
        creationDate: '2023-01-01',
        processingTime: 0.123,
      });
    });

    it('should handle IP address parameter', async () => {
      const mockResponse = {
        data: {
          address: 'test@example.com',
          status: 'valid',
          sub_status: '',
          account: 'test',
          domain: 'example.com',
          processingtime: '0.123',
        },
      };

      const mockInstance = mockedAxios.create();
      mockInstance.get.mockResolvedValue(mockResponse);

      await client.verifyEmail('test@example.com', '192.168.1.1');

      expect(mockInstance.get).toHaveBeenCalledWith('/validate', {
        params: {
          api_key: apiKey,
          email: 'test@example.com',
          ip_address: '192.168.1.1',
        },
      });
    });

    it('should handle API errors', async () => {
      const mockInstance = mockedAxios.create();
      mockInstance.get.mockRejectedValue(new Error('API Error'));

      await expect(client.verifyEmail('test@example.com')).rejects.toThrow(
        'ZeroBounce API error: Error: API Error'
      );
    });

    it('should map different status values correctly', async () => {
      const testCases = [
        { apiStatus: 'valid', expectedStatus: 'valid' },
        { apiStatus: 'invalid', expectedStatus: 'invalid' },
        { apiStatus: 'catch-all', expectedStatus: 'catch-all' },
        { apiStatus: 'unknown', expectedStatus: 'unknown' },
        { apiStatus: 'spamtrap', expectedStatus: 'spamtrap' },
        { apiStatus: 'abuse', expectedStatus: 'abuse' },
        { apiStatus: 'do_not_mail', expectedStatus: 'do_not_mail' },
        { apiStatus: 'some_unknown_status', expectedStatus: 'unknown' },
      ];

      const mockInstance = mockedAxios.create();

      for (const testCase of testCases) {
        const mockResponse = {
          data: {
            address: 'test@example.com',
            status: testCase.apiStatus,
            sub_status: '',
            account: 'test',
            domain: 'example.com',
            processingtime: '0.123',
          },
        };

        mockInstance.get.mockResolvedValue(mockResponse);

        const result = await client.verifyEmail('test@example.com');
        expect(result.status).toBe(testCase.expectedStatus);
      }
    });
  });

  describe('verifyEmailBulk', () => {
    it('should handle empty email array', async () => {
      const result = await client.verifyEmailBulk([]);

      expect(result).toEqual({
        results: [],
        totalCount: 0,
        validCount: 0,
        invalidCount: 0,
        creditsUsed: 0,
      });
    });

    it('should process bulk verification with file upload', async () => {
      const emails = ['test1@example.com', 'test2@example.com'];
      
      const mockInstance = mockedAxios.create();
      
      // Mock sendfile response
      mockInstance.post.mockResolvedValueOnce({
        data: {
          success: true,
          file_id: 'test-file-id',
        },
      });

      // Mock getfile responses (processing then complete)
      mockInstance.get
        .mockResolvedValueOnce({
          data: {
            file_status: 'Processing',
          },
        })
        .mockResolvedValueOnce({
          data: {
            file_status: 'Complete',
            local_file_path: 'http://example.com/results.csv',
          },
        });

      // Mock download results
      mockedAxios.get.mockResolvedValue({
        data: 'email,zerobounce_status,zerobounce_sub_status,account,domain\ntest1@example.com,valid,,test1,example.com\ntest2@example.com,invalid,mailbox_not_found,test2,example.com',
      });

      const result = await client.verifyEmailBulk(emails);

      expect(mockInstance.post).toHaveBeenCalledWith('/sendfile', expect.any(FormData), {
        headers: {
          'Content-Type': 'multipart/form-data',
        },
      });

      expect(result.totalCount).toBe(2);
      expect(result.validCount).toBe(1);
      expect(result.invalidCount).toBe(1);
      expect(result.results).toHaveLength(2);
    });

    it('should handle bulk verification failure', async () => {
      const emails = ['test@example.com'];
      
      const mockInstance = mockedAxios.create();
      
      mockInstance.post.mockResolvedValue({
        data: {
          success: false,
          message: 'File upload failed',
        },
      });

      await expect(client.verifyEmailBulk(emails)).rejects.toThrow(
        'Failed to send bulk verification file: Error: Bulk verification failed: File upload failed'
      );
    });

    it('should handle timeout during bulk processing', async () => {
      const emails = ['test@example.com'];
      
      const mockInstance = mockedAxios.create();
      
      // Mock sendfile response
      mockInstance.post.mockResolvedValue({
        data: {
          success: true,
          file_id: 'test-file-id',
        },
      });

      // Mock getfile to always return processing (timeout scenario)
      mockInstance.get.mockResolvedValue({
        data: {
          file_status: 'Processing',
        },
      });

      await expect(client.verifyEmailBulk(emails)).rejects.toThrow(
        'Bulk verification timed out'
      );
    });

    it('should handle failed bulk processing status', async () => {
      const emails = ['test@example.com'];
      
      const mockInstance = mockedAxios.create();
      
      // Mock sendfile response
      mockInstance.post.mockResolvedValue({
        data: {
          success: true,
          file_id: 'test-file-id',
        },
      });

      // Mock getfile to return failed status
      mockInstance.get.mockResolvedValue({
        data: {
          file_status: 'Failed',
        },
      });

      await expect(client.verifyEmailBulk(emails)).rejects.toThrow(
        'Failed to get bulk results: Error: Bulk verification failed with status: Failed'
      );
    });
  });

  describe('getCredits', () => {
    it('should return available credits', async () => {
      const mockResponse = {
        data: {
          Credits: 5000,
        },
      };

      const mockInstance = mockedAxios.create();
      mockInstance.get.mockResolvedValue(mockResponse);

      const result = await client.getCredits();

      expect(mockInstance.get).toHaveBeenCalledWith('/getcredits', {
        params: {
          api_key: apiKey,
        },
      });

      expect(result).toBe(5000);
    });

    it('should return 0 if no credits in response', async () => {
      const mockResponse = {
        data: {},
      };

      const mockInstance = mockedAxios.create();
      mockInstance.get.mockResolvedValue(mockResponse);

      const result = await client.getCredits();
      expect(result).toBe(0);
    });

    it('should handle credits API error', async () => {
      const mockInstance = mockedAxios.create();
      mockInstance.get.mockRejectedValue(new Error('API Error'));

      await expect(client.getCredits()).rejects.toThrow(
        'Failed to get credits: Error: API Error'
      );
    });
  });

  describe('getApiUsage', () => {
    it('should return API usage statistics', async () => {
      const mockResponse = {
        data: {
          total: 1000,
          status_valid: 800,
          status_invalid: 150,
          status_catch_all: 30,
          status_unknown: 20,
        },
      };

      const mockInstance = mockedAxios.create();
      mockInstance.get.mockResolvedValue(mockResponse);

      const result = await client.getApiUsage('2023-01-01', '2023-01-31');

      expect(mockInstance.get).toHaveBeenCalledWith('/getapiusage', {
        params: {
          api_key: apiKey,
          start_date: '2023-01-01',
          end_date: '2023-01-31',
        },
      });

      expect(result).toEqual(mockResponse.data);
    });

    it('should handle API usage error', async () => {
      const mockInstance = mockedAxios.create();
      mockInstance.get.mockRejectedValue(new Error('API Error'));

      await expect(client.getApiUsage('2023-01-01', '2023-01-31')).rejects.toThrow(
        'Failed to get API usage: Error: API Error'
      );
    });
  });
});
