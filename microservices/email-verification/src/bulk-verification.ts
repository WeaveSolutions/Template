import express from 'express';
import multer from 'multer';
import csvParser from 'csv-parser';
import { createEmailServiceFromEnv } from '@weave/email-service';
import { BulkVerificationResult } from '@weave/email-service';
import logger from './logger';

const router = express.Router();
const upload = multer({ 
  dest: 'uploads/',
  limits: { fileSize: 10 * 1024 * 1024 } // 10MB limit
});

interface BulkVerificationRequest {
  emails?: string[];
  file?: Express.Multer.File;
  options?: {
    batchSize?: number;
    delayMs?: number;
    returnInvalid?: boolean;
  };
}

/**
 * POST /verify/bulk
 * Verify multiple emails in bulk
 */
router.post('/verify/bulk', upload.single('file'), async (req, res) => {
  try {
    const emailService = createEmailServiceFromEnv();
    let emails: string[] = [];

    // Handle different input formats
    if (req.file) {
      // Parse CSV file
      emails = await parseEmailsFromCsv(req.file.path);
    } else if (req.body.emails) {
      // Use emails from request body
      emails = Array.isArray(req.body.emails) ? req.body.emails : [req.body.emails];
    } else {
      return res.status(400).json({
        error: 'Either provide emails array or upload a CSV file',
      });
    }

    if (emails.length === 0) {
      return res.status(400).json({
        error: 'No emails provided for verification',
      });
    }

    if (emails.length > 10000) {
      return res.status(400).json({
        error: 'Maximum 10,000 emails per batch allowed',
      });
    }

    logger.info(`Starting bulk verification for ${emails.length} emails`);

    // Get options from request
    const options = req.body.options || {};
    const batchSize = options.batchSize || 100;
    const delayMs = options.delayMs || 1000;
    const returnInvalid = options.returnInvalid !== false; // Default to true

    // Perform bulk verification
    const result: BulkVerificationResult = await emailService.batchVerifyEmails(
      emails,
      batchSize,
      delayMs
    );

    // Filter results based on options
    let filteredResults = result.results;
    if (!returnInvalid) {
      filteredResults = result.results.filter(r => r.status === 'valid');
    }

    logger.info(`Bulk verification completed: ${result.validCount}/${result.totalCount} valid emails`);

    res.json({
      success: true,
      data: {
        ...result,
        results: filteredResults,
      },
      metadata: {
        processedAt: new Date().toISOString(),
        batchSize,
        delayMs,
        returnInvalid,
      },
    });

  } catch (error) {
    logger.error('Bulk verification failed:', error);
    res.status(500).json({
      error: 'Bulk verification failed',
      message: error instanceof Error ? error.message : 'Unknown error',
    });
  }
});

/**
 * POST /verify/single
 * Verify a single email address
 */
router.post('/verify/single', async (req, res) => {
  try {
    const { email } = req.body;

    if (!email) {
      return res.status(400).json({
        error: 'Email address is required',
      });
    }

    const emailService = createEmailServiceFromEnv();
    const result = await emailService.verifyEmail(email);

    logger.info(`Single verification completed for: ${email} - Status: ${result.status}`);

    res.json({
      success: true,
      data: result,
      metadata: {
        processedAt: new Date().toISOString(),
      },
    });

  } catch (error) {
    logger.error('Single verification failed:', error);
    res.status(500).json({
      error: 'Email verification failed',
      message: error instanceof Error ? error.message : 'Unknown error',
    });
  }
});

/**
 * GET /verify/credits
 * Get remaining verification credits
 */
router.get('/verify/credits', async (req, res) => {
  try {
    const emailService = createEmailServiceFromEnv();
    const credits = await emailService.getVerificationCredits();

    res.json({
      success: true,
      data: {
        credits,
        timestamp: new Date().toISOString(),
      },
    });

  } catch (error) {
    logger.error('Failed to get credits:', error);
    res.status(500).json({
      error: 'Failed to get credits',
      message: error instanceof Error ? error.message : 'Unknown error',
    });
  }
});

/**
 * POST /verify/clean-lists
 * Clean Brevo email lists by removing invalid emails
 */
router.post('/verify/clean-lists', async (req, res) => {
  try {
    const { listIds } = req.body;

    if (!listIds || !Array.isArray(listIds)) {
      return res.status(400).json({
        error: 'List IDs array is required',
      });
    }

    const emailService = createEmailServiceFromEnv();
    const results = await emailService.cleanEmailLists(listIds);

    logger.info(`Cleaned ${listIds.length} email lists`);

    res.json({
      success: true,
      data: results,
      metadata: {
        processedAt: new Date().toISOString(),
        totalListsCleaned: results.length,
      },
    });

  } catch (error) {
    logger.error('Failed to clean lists:', error);
    res.status(500).json({
      error: 'Failed to clean email lists',
      message: error instanceof Error ? error.message : 'Unknown error',
    });
  }
});

/**
 * Parse emails from uploaded CSV file
 */
async function parseEmailsFromCsv(filePath: string): Promise<string[]> {
  return new Promise((resolve, reject) => {
    const emails: string[] = [];
    const fs = require('fs');

    fs.createReadStream(filePath)
      .pipe(csvParser())
      .on('data', (row) => {
        // Try to find email in various column names
        const email = row.email || row.Email || row.EMAIL || 
                     row.address || row.Address || row.ADDRESS ||
                     Object.values(row)[0]; // Fallback to first column

        if (email && typeof email === 'string') {
          const trimmedEmail = email.trim();
          if (trimmedEmail && trimmedEmail.includes('@')) {
            emails.push(trimmedEmail);
          }
        }
      })
      .on('end', () => {
        // Clean up uploaded file
        fs.unlink(filePath, (err: any) => {
          if (err) logger.warn('Failed to clean up uploaded file:', err);
        });
        resolve([...new Set(emails)]); // Remove duplicates
      })
      .on('error', (error) => {
        // Clean up uploaded file on error
        fs.unlink(filePath, (err: any) => {
          if (err) logger.warn('Failed to clean up uploaded file:', err);
        });
        reject(error);
      });
  });
}

export default router;
