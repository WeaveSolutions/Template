# CI/CD Setup for Terraform Infrastructure

This guide explains how to set up GitHub Actions for automated Terraform deployments.

## Overview

The CI/CD pipeline automatically manages your infrastructure deployments with:
- Automated validation and formatting checks
- Environment-specific deployments (dev/prod)
- Pull request plan previews
- Cost estimation
- Manual deployment options

## GitHub Secrets Configuration

### Required Secrets

#### 1. AWS Credentials
```bash
# Development Environment
AWS_ACCESS_KEY_ID_DEV
AWS_SECRET_ACCESS_KEY_DEV

# Production Environment
AWS_ACCESS_KEY_ID_PROD
AWS_SECRET_ACCESS_KEY_PROD
```

#### 2. Terraform State Backend
```bash
TF_STATE_BUCKET          # S3 bucket for Terraform state
TF_STATE_LOCK_TABLE      # DynamoDB table for state locking
```

#### 3. Application Configuration
```bash
# Development
SUPABASE_URL             # Supabase project URL
SUPABASE_ANON_KEY        # Supabase anonymous key

# Production
SUPABASE_URL_PROD        # Production Supabase URL
SUPABASE_ANON_KEY_PROD   # Production Supabase key
```

#### 4. Domain Configuration (Production Only)
```bash
CUSTOM_DOMAIN            # Your domain (e.g., app.example.com)
ACM_CERTIFICATE_ARN      # AWS Certificate Manager ARN
ROUTE53_ZONE_ID          # Route53 hosted zone ID
```

#### 5. Monitoring & Alerts
```bash
# Development
ALARM_EMAILS             # JSON array: ["email1@example.com"]

# Production
ALARM_EMAILS_PROD        # JSON array: ["email1@example.com", "email2@example.com"]
ALARM_SMS_PROD           # JSON array: ["+1234567890"]
BUDGET_EMAILS_PROD       # JSON array: ["finance@example.com"]
```

#### 6. Cost Estimation (Optional)
```bash
INFRACOST_API_KEY        # API key from infracost.io
```

### Setting Secrets in GitHub

1. Navigate to Settings → Secrets and variables → Actions
2. Click "New repository secret"
3. Add each secret with the appropriate value

## Initial Setup

### 1. Create S3 Backend

Before running the CI/CD pipeline, create the Terraform state backend:

```bash
# Create S3 bucket
aws s3api create-bucket \
  --bucket your-terraform-state-bucket \
  --region us-east-1

# Enable versioning
aws s3api put-bucket-versioning \
  --bucket your-terraform-state-bucket \
  --versioning-configuration Status=Enabled

# Enable encryption
aws s3api put-bucket-encryption \
  --bucket your-terraform-state-bucket \
  --server-side-encryption-configuration '{
    "Rules": [{
      "ApplyServerSideEncryptionByDefault": {
        "SSEAlgorithm": "AES256"
      }
    }]
  }'

# Create DynamoDB table for state locking
aws dynamodb create-table \
  --table-name terraform-state-lock \
  --attribute-definitions AttributeName=LockID,AttributeType=S \
  --key-schema AttributeName=LockID,KeyType=HASH \
  --billing-mode PAY_PER_REQUEST \
  --region us-east-1
```

### 2. IAM User Setup

Create IAM users with appropriate permissions:

```bash
# Development IAM Policy
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Action": [
        "ec2:*",
        "s3:*",
        "iam:*",
        "rds:*",
        "cloudfront:*",
        "cloudwatch:*",
        "logs:*",
        "sns:*",
        "dynamodb:*",
        "lambda:*",
        "ecs:*",
        "elasticloadbalancing:*",
        "autoscaling:*",
        "cloudformation:*",
        "waf:*",
        "guardduty:*",
        "securityhub:*"
      ],
      "Resource": "*",
      "Condition": {
        "StringEquals": {
          "aws:RequestedRegion": "us-east-1"
        }
      }
    },
    {
      "Effect": "Allow",
      "Action": [
        "s3:GetObject",
        "s3:PutObject",
        "s3:DeleteObject"
      ],
      "Resource": "arn:aws:s3:::your-terraform-state-bucket/*"
    },
    {
      "Effect": "Allow",
      "Action": [
        "dynamodb:GetItem",
        "dynamodb:PutItem",
        "dynamodb:DeleteItem"
      ],
      "Resource": "arn:aws:dynamodb:us-east-1:*:table/terraform-state-lock"
    }
  ]
}
```

## Workflow Triggers

### Automatic Deployments

1. **Development Environment**
   - Triggers on pull requests to `main`
   - Runs `terraform plan` and posts results to PR

2. **Production Environment**
   - Triggers on push to `main` branch
   - Automatically applies changes after successful plan

### Manual Deployments

Use the workflow dispatch feature:

1. Go to Actions → "Terraform Infrastructure"
2. Click "Run workflow"
3. Select:
   - **Environment**: `dev` or `prod`
   - **Action**: `plan`, `apply`, or `destroy`

## Workflow Jobs

### 1. terraform-check
- Validates Terraform formatting
- Initializes and validates all modules
- Runs on all PRs and pushes

### 2. terraform-dev
- Plans changes for development environment
- Posts plan results to PR comments
- Uploads plan artifacts

### 3. terraform-prod
- Plans and applies changes for production
- Only runs on main branch pushes
- Requires all secrets to be configured

### 4. terraform-manual
- Allows manual plan/apply/destroy
- Environment-specific configuration
- Useful for emergency changes

### 5. cost-estimation
- Uses Infracost to estimate costs
- Posts cost breakdown to PRs
- Helps prevent budget surprises

## Best Practices

### 1. Branch Protection

Enable branch protection for `main`:
- Require PR reviews
- Require status checks to pass
- Dismiss stale PR approvals

### 2. Environment Protection

Configure environment protection rules:
1. Go to Settings → Environments
2. Configure for `production`:
   - Required reviewers
   - Deployment branches: `main` only
   - Environment secrets

### 3. Monitoring

Set up notifications:
- Watch repository for workflow failures
- Configure Slack/email notifications
- Monitor AWS Cost Explorer

### 4. Security

- Rotate AWS access keys regularly
- Use least-privilege IAM policies
- Enable MFA for AWS accounts
- Audit CloudTrail logs

## Troubleshooting

### Common Issues

1. **State Lock Error**
   ```bash
   # Force unlock if needed
   terraform force-unlock <LOCK_ID>
   ```

2. **Permission Denied**
   - Check IAM policies
   - Verify secret values
   - Check AWS credentials

3. **Plan Differences**
   - Ensure local and CI environments match
   - Check for drift in manual changes
   - Verify variable values

### Debugging

Enable debug logging:
```yaml
env:
  TF_LOG: DEBUG
```

Check workflow logs:
1. Go to Actions tab
2. Click on failed workflow
3. Expand job steps

## Cost Management

### Monitoring Costs

1. **AWS Budgets**
   - Set up through Terraform
   - Alerts sent to configured emails

2. **Infracost**
   - Review PR cost estimates
   - Compare dev vs prod costs

### Cost Optimization

- Use spot instances for dev
- Enable auto-scaling
- Set up lifecycle policies
- Review unused resources

## Next Steps

1. **Set up GitHub secrets**
2. **Create backend resources**
3. **Test with a PR**
4. **Configure environments**
5. **Enable branch protection**

For questions or issues, check:
- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [Terraform CI/CD Best Practices](https://www.terraform.io/docs/cloud/guides/recommended-practices/index.html)
- [AWS IAM Best Practices](https://docs.aws.amazon.com/IAM/latest/UserGuide/best-practices.html)
