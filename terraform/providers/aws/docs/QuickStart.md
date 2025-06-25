# Terraform Quick Start Guide

This guide will help you quickly deploy the Nexpo infrastructure.

## Prerequisites Checklist

- [ ] AWS Account with administrative access
- [ ] AWS CLI installed and configured
- [ ] Terraform 1.0+ installed
- [ ] Docker installed (for building container images)
- [ ] Supabase project created (get URL and anon key)

## Step 1: Set Up AWS Backend

Create S3 bucket and DynamoDB table for Terraform state:

```bash
# Create S3 bucket for state
aws s3api create-bucket \
  --bucket your-terraform-state-bucket \
  --region us-east-1

# Enable versioning
aws s3api put-bucket-versioning \
  --bucket your-terraform-state-bucket \
  --versioning-configuration Status=Enabled

# Create DynamoDB table for state locking
aws dynamodb create-table \
  --table-name terraform-state-lock \
  --attribute-definitions AttributeName=LockID,AttributeType=S \
  --key-schema AttributeName=LockID,KeyType=HASH \
  --provisioned-throughput ReadCapacityUnits=5,WriteCapacityUnits=5 \
  --region us-east-1
```

## Step 2: Configure Development Environment

```bash
cd terraform/environments/dev

# Copy and edit variables
cp terraform.tfvars.example terraform.tfvars
```

Edit `terraform.tfvars`:
```hcl
# Minimum required configuration
project_name = "my-app"
environment  = "dev"
aws_region   = "us-east-1"

# From your Supabase project
supabase_url      = "https://xxxxxxxxxxxx.supabase.co"
supabase_anon_key = "your-anon-key"

# Your email for alerts
alarm_email_endpoints = ["your-email@example.com"]
```

## Step 3: Update Backend Configuration

Edit `terraform/environments/dev/main.tf`:

```hcl
terraform {
  backend "s3" {
    bucket = "your-terraform-state-bucket"  # Replace with your bucket
    key    = "my-app/dev/terraform.tfstate"
    region = "us-east-1"
    encrypt = true
    dynamodb_table = "terraform-state-lock"
  }
}
```

## Step 4: Deploy Infrastructure

```bash
# Initialize Terraform
terraform init

# Review the plan
terraform plan

# Deploy (this will take 10-15 minutes)
terraform apply
```

## Step 5: Build and Deploy Application

After infrastructure is created:

```bash
# Get ECR repository URL from outputs
ECR_URL=$(terraform output -raw ecr_repository_url)

# Build and push Docker image
cd ../../../  # Back to project root
docker build -t $ECR_URL:latest -f apps/next/Dockerfile .

# Login to ECR
aws ecr get-login-password --region us-east-1 | docker login --username AWS --password-stdin $ECR_URL

# Push image
docker push $ECR_URL:latest

# Update App Runner service (happens automatically on push)
```

## Step 6: Access Your Application

```bash
# Get application URL
terraform output app_url

# Get CloudFront URL
terraform output cloudfront_distribution_domain
```

## Common Commands

### View all outputs
```bash
terraform output
```

### Update infrastructure
```bash
terraform plan
terraform apply
```

### Destroy infrastructure
```bash
terraform destroy
```

### View costs
```bash
# Estimate monthly costs
terraform plan -out=tfplan
terraform show -json tfplan | jq '.planned_values.root_module' > costs.json
```

## Troubleshooting

### Issue: Terraform init fails
**Solution**: Check AWS credentials and backend configuration

### Issue: Apply fails with permission error
**Solution**: Ensure your AWS user has required permissions (see README)

### Issue: App Runner deployment fails
**Solution**: Check ECR image exists and App Runner has pull permissions

### Issue: Database connection fails
**Solution**: Check security groups allow traffic from App Runner

## Next Steps

1. **Set up custom domain**:
   - Create ACM certificate
   - Update terraform.tfvars with domain info
   - Run terraform apply

2. **Enable production features**:
   - Deploy to production environment
   - Enable WAF and security features
   - Set up monitoring dashboards

3. **Configure CI/CD**:
   - Set up GitHub Actions
   - Automate deployments
   - Add environment protection rules

## Cost Estimates

Typical monthly costs:
- **Development**: $50-100
  - App Runner: ~$20
  - RDS (db.t3.micro): ~$15
  - S3 & CloudFront: ~$10
  - Other services: ~$5-10

- **Production**: $200-500+
  - App Runner: ~$50-100
  - RDS (db.t3.small): ~$30
  - CloudFront: ~$20-50
  - Monitoring: ~$10-20
  - WAF: ~$20

## Quick Teardown

To completely remove all resources:

```bash
# Remove infrastructure
cd terraform/environments/dev
terraform destroy -auto-approve

# Delete state bucket (after terraform destroy)
aws s3 rb s3://your-terraform-state-bucket --force

# Delete DynamoDB table
aws dynamodb delete-table --table-name terraform-state-lock
```

## Support

Need help? Check:
1. Full documentation in [README.md](./README.md)
2. AWS service quotas and limits
3. CloudWatch logs for application errors
4. Terraform state for resource details
