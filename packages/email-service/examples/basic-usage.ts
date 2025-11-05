import { EmailService, createEmailServiceFromEnv } from '../src';

async function basicUsageExample() {
  // Create email service from environment variables
  const emailService = createEmailServiceFromEnv();

  try {
    // 1. Verify a single email
    console.log('Verifying single email...');
    const singleVerification = await emailService.verifyEmail('user@example.com');
    console.log('Verification result:', singleVerification);

    // 2. Verify multiple emails in bulk
    console.log('\nVerifying bulk emails...');
    const emails = [
      'valid@example.com',
      'invalid@invalid-domain.xyz',
      'test@gmail.com'
    ];
    const bulkVerification = await emailService.verifyEmailBulk(emails);
    console.log('Bulk verification results:', bulkVerification);

    // 3. Send a transactional email (with auto-verification)
    console.log('\nSending transactional email...');
    const transactionalResult = await emailService.sendTransactionalEmail({
      to: { email: 'recipient@example.com', name: 'John Doe' },
      subject: 'Welcome to our platform!',
      htmlContent: '<h1>Welcome!</h1><p>Thank you for joining us.</p>',
      tags: ['welcome', 'onboarding']
    });
    console.log('Email sent:', transactionalResult);

    // 4. Create an email campaign with verification
    console.log('\nCreating email campaign...');
    const campaignResult = await emailService.createEmailCampaign({
      name: 'Monthly Newsletter',
      subject: 'Our Latest Updates',
      htmlContent: '<h1>Newsletter</h1><p>Check out our latest features!</p>',
      listIds: [1], // Replace with actual list IDs
      tags: ['newsletter', 'monthly']
    });
    console.log('Campaign created:', campaignResult);

    // 5. Get account information
    console.log('\nFetching account info...');
    const accountInfo = await emailService.getAccountInfo();
    console.log('Account info:', accountInfo);

    // 6. Check verification credits
    console.log('\nChecking verification credits...');
    const credits = await emailService.getVerificationCredits();
    console.log('Remaining credits:', credits);

  } catch (error) {
    console.error('Error occurred:', error);
  }
}

// Run the example
if (require.main === module) {
  basicUsageExample()
    .then(() => console.log('\nExample completed successfully!'))
    .catch(error => console.error('Example failed:', error));
}
