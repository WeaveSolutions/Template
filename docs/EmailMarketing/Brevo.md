# Brevo Email Marketing Integration

## Overview

Brevo (formerly Sendinblue) is a comprehensive email marketing platform that provides transactional and marketing email services. This integration combines Brevo's email capabilities with Zero Bounce's email verification to ensure high deliverability rates.

## Features

- **Email Verification**: Bulk email verification using Zero Bounce API before sending campaigns
- **Transactional Emails**: Send automated emails triggered by user actions
- **Marketing Campaigns**: Create and send targeted email campaigns
- **Contact Management**: Manage subscriber lists and segments
- **Analytics**: Track email performance metrics

## Setup

### Prerequisites

1. **Brevo Account**: Sign up at [brevo.com](https://www.brevo.com)
2. **Zero Bounce Account**: Sign up at [zerobounce.net](https://www.zerobounce.net)
3. **API Keys**: Obtain API keys from both services

### Environment Variables

Add the following to your `.env` file:

```env
# Brevo Configuration
BREVO_API_KEY=your_brevo_api_key_here
BREVO_SENDER_EMAIL=noreply@yourdomain.com
BREVO_SENDER_NAME=Your Company Name

# Zero Bounce Configuration
ZEROBOUNCE_API_KEY=your_zerobounce_api_key_here
```

### Installation

Install the required dependencies:

```bash
pnpm add @getbrevo/brevo zerobounce-node
```

## Usage

### Email Verification with Zero Bounce

Before sending emails, verify email addresses to improve deliverability:

```typescript
import { verifyEmailBulk } from '../utils/email-verification';

const emails = ['user1@example.com', 'user2@example.com'];
const verifiedEmails = await verifyEmailBulk(emails);
```

### Sending Transactional Emails

```typescript
import { sendTransactionalEmail } from '../utils/brevo-client';

await sendTransactionalEmail({
  to: 'user@example.com',
  subject: 'Welcome to Our Platform',
  templateId: 1,
  params: {
    firstName: 'John',
    lastName: 'Doe'
  }
});
```

### Creating Email Campaigns

```typescript
import { createEmailCampaign } from '../utils/brevo-client';

const campaign = await createEmailCampaign({
  name: 'Monthly Newsletter',
  subject: 'Our Latest Updates',
  htmlContent: '<h1>Newsletter Content</h1>',
  listIds: [1, 2],
  scheduledAt: new Date('2024-01-15T10:00:00Z')
});
```

## API Reference

### Brevo Integration

#### `sendTransactionalEmail(options)`

Send a transactional email.

**Parameters:**
- `to` (string): Recipient email address
- `subject` (string): Email subject
- `templateId` (number): Brevo template ID
- `params` (object): Template parameters

#### `createEmailCampaign(options)`

Create an email campaign.

**Parameters:**
- `name` (string): Campaign name
- `subject` (string): Email subject
- `htmlContent` (string): Email HTML content
- `listIds` (number[]): Target list IDs
- `scheduledAt` (Date): Schedule date/time

### Zero Bounce Integration

#### `verifyEmailBulk(emails)`

Verify multiple email addresses.

**Parameters:**
- `emails` (string[]): Array of email addresses to verify

**Returns:**
- Promise<VerifiedEmail[]>: Array of verified email results

#### `verifyEmailSingle(email)`

Verify a single email address.

**Parameters:**
- `email` (string): Email address to verify

**Returns:**
- Promise<VerifiedEmail>: Verification result

## Best Practices

### Email Verification
- Always verify emails before adding to lists
- Remove invalid emails to maintain sender reputation
- Re-verify old email lists periodically

### Deliverability
- Use double opt-in for new subscribers
- Maintain clean email lists
- Monitor bounce rates and unsubscribes
- Authenticate your domain (SPF, DKIM, DMARC)

### Compliance
- Follow GDPR/CAN-SPAM regulations
- Include unsubscribe links in all emails
- Honor unsubscribe requests promptly
- Maintain consent records

## Error Handling

Common error scenarios and solutions:

### Brevo API Errors
```typescript
try {
  await sendTransactionalEmail(options);
} catch (error) {
  if (error.status === 402) {
    console.error('Insufficient credits');
  } else if (error.status === 400) {
    console.error('Invalid request data');
  }
}
```

### Zero Bounce API Errors
```typescript
try {
  const result = await verifyEmailBulk(emails);
} catch (error) {
  if (error.message.includes('insufficient credits')) {
    console.error('Zero Bounce credits exhausted');
  }
}
```

## Monitoring and Analytics

### Key Metrics to Track
- Email deliverability rate
- Open rates
- Click-through rates
- Bounce rates
- Unsubscribe rates
- Spam complaints

### Implementation
```typescript
import { getEmailStatistics } from '../utils/brevo-client';

const stats = await getEmailStatistics({
  campaignId: 123,
  startDate: '2024-01-01',
  endDate: '2024-01-31'
});
```

## Troubleshooting

### Common Issues

1. **High Bounce Rates**
   - Solution: Increase email verification frequency
   - Check domain authentication settings

2. **Low Deliverability**
   - Solution: Clean email lists regularly
   - Monitor sender reputation

3. **API Rate Limits**
   - Solution: Implement rate limiting in your code
   - Use batch operations where possible

### Support Resources

- [Brevo API Documentation](https://developers.brevo.com/)
- [Zero Bounce API Documentation](https://www.zerobounce.net/docs/)
- [Email Deliverability Best Practices](https://www.brevo.com/blog/email-deliverability/)

## Integration Examples

See the implementation files:
- `packages/email-service/src/brevo-client.ts`
- `packages/email-service/src/zerobounce-client.ts`
- `microservices/email-verification/src/bulk-verification.ts`
