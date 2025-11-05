// Email Service Types

export interface EmailAddress {
  email: string;
  name?: string;
}

export interface TransactionalEmailOptions {
  to: EmailAddress | EmailAddress[];
  subject: string;
  templateId?: number;
  htmlContent?: string;
  textContent?: string;
  params?: Record<string, any>;
  replyTo?: EmailAddress;
  cc?: EmailAddress[];
  bcc?: EmailAddress[];
  tags?: string[];
}

export interface EmailCampaignOptions {
  name: string;
  subject: string;
  htmlContent: string;
  textContent?: string;
  listIds: number[];
  excludedListIds?: number[];
  scheduledAt?: Date;
  sendAtBestTime?: boolean;
  abTestId?: string;
  utmCampaign?: string;
  tags?: string[];
}

export interface EmailVerificationResult {
  email: string;
  status: 'valid' | 'invalid' | 'catch-all' | 'unknown' | 'spamtrap' | 'abuse' | 'do_not_mail';
  subStatus: string;
  account: string;
  domain: string;
  firstName?: string;
  lastName?: string;
  gender?: string;
  location?: string;
  creationDate?: string;
  processingTime: number;
}

export interface BulkVerificationResult {
  results: EmailVerificationResult[];
  totalCount: number;
  validCount: number;
  invalidCount: number;
  creditsUsed: number;
}

export interface EmailStatistics {
  campaignId?: string;
  delivered: number;
  hardBounces: number;
  softBounces: number;
  clicks: number;
  uniqueClicks: number;
  opens: number;
  uniqueOpens: number;
  spam: number;
  blocked: number;
  invalid: number;
  unsubscribed: number;
}

export interface ContactListOptions {
  name: string;
  folderId?: number;
}

export interface ContactOptions {
  email: string;
  attributes?: Record<string, any>;
  listIds?: number[];
  updateEnabled?: boolean;
  emailBlacklisted?: boolean;
  smsBlacklisted?: boolean;
}

export interface EmailServiceConfig {
  brevo: {
    apiKey: string;
    senderEmail: string;
    senderName: string;
  };
  zeroBounce: {
    apiKey: string;
  };
}
