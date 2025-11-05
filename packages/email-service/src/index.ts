// Email Service - Main Export
export { EmailService } from './email-service';
export { BrevoClient } from './brevo-client';
export { ZeroBounceClient } from './zerobounce-client';

// Types
export type {
  EmailServiceConfig,
  EmailAddress,
  TransactionalEmailOptions,
  EmailCampaignOptions,
  EmailVerificationResult,
  BulkVerificationResult,
  EmailStatistics,
  ContactListOptions,
  ContactOptions,
} from './types';

// Utility functions
export { createEmailService, validateEmailConfig } from './utils';

// Constants
export const EMAIL_STATUSES = {
  VALID: 'valid',
  INVALID: 'invalid',
  CATCH_ALL: 'catch-all',
  UNKNOWN: 'unknown',
  SPAMTRAP: 'spamtrap',
  ABUSE: 'abuse',
  DO_NOT_MAIL: 'do_not_mail',
} as const;
