# Terraform Deployment Checklist

Use this checklist to ensure your infrastructure is properly configured before deployment.

## 🔧 Pre-Deployment Setup

### 1. AWS Account Setup
- [ ] AWS account created
- [ ] IAM user created with appropriate permissions
- [ ] AWS CLI installed and configured (`aws configure`)
- [ ] AWS credentials tested (`aws sts get-caller-identity`)

### 2. Terraform Backend Setup
- [ ] Run backend setup script:
  - **Windows**: `.\terraform\scripts\setup-backend.ps1`
  - **Linux/Mac**: `./terraform/scripts/setup-backend.sh`
- [ ] Verify S3 bucket created
- [ ] Verify DynamoDB table created
- [ ] Backend configuration files generated

### 3. Prerequisites Installation
- [ ] Terraform installed (v1.5.0+)
- [ ] Node.js installed (v18+)
- [ ] Docker installed (for local testing)
- [ ] Make installed (optional, for Makefile commands)

### 4. Environment Variables
- [ ] Copy `.env.example` to `.env` in root directory
- [ ] Set Supabase credentials:
  - [ ] `NEXT_PUBLIC_SUPABASE_URL`
  - [ ] `NEXT_PUBLIC_SUPABASE_ANON_KEY`
- [ ] Set Auth0 credentials (if using):
  - [ ] `AUTH0_DOMAIN`
  - [ ] `AUTH0_CLIENT_ID`
  - [ ] `AUTH0_CLIENT_SECRET`

### 5. Terraform Variables
- [ ] Copy `terraform.tfvars.example` to `terraform.tfvars` in environment directories
- [ ] Update variables in `terraform/environments/dev/terraform.tfvars`:
  ```hcl
  project_name          = "Nexpo"
  environment           = "dev"
  aws_region           = "us-east-1"
  supabase_url         = "your-supabase-url"
  supabase_anon_key    = "your-supabase-anon-key"
  alarm_email_endpoints = ["your-email@example.com"]
  ```

## 🚀 Development Deployment

### 1. Initialize Terraform
```bash
cd terraform/environments/dev
terraform init
```

### 2. Review Infrastructure Plan
```bash
terraform plan
```
- [ ] Review resources to be created
- [ ] Check estimated costs
- [ ] Verify no unexpected changes

### 3. Deploy Infrastructure
```bash
terraform apply
```
- [ ] Type 'yes' to confirm
- [ ] Wait for deployment to complete
- [ ] Note down important outputs

### 4. Verify Deployment
- [ ] Check AWS Console for resources
- [ ] Test application URL
- [ ] Verify CloudWatch logs
- [ ] Check monitoring dashboard

## 🏭 Production Deployment

### 1. Additional Production Setup
- [ ] Custom domain registered
- [ ] SSL certificate requested in ACM
- [ ] Route53 hosted zone created
- [ ] Production database credentials secured

### 2. Update Production Variables
- [ ] Update `terraform/environments/prod/terraform.tfvars`:
  ```hcl
  custom_domain        = "app.yourdomain.com"
  acm_certificate_arn  = "arn:aws:acm:..."
  route53_zone_id      = "Z..."
  alarm_sms_endpoints  = ["+1234567890"]
  budget_alert_emails  = ["finance@yourdomain.com"]
  ```

### 3. Production Deployment
```bash
cd terraform/environments/prod
terraform init
terraform plan
terraform apply
```

### 4. DNS Configuration
- [ ] Update DNS records to point to CloudFront
- [ ] Wait for DNS propagation
- [ ] Test custom domain access

## 🔐 Security Checklist

### 1. Access Control
- [ ] IAM roles follow least privilege principle
- [ ] MFA enabled on AWS account
- [ ] Secrets stored in AWS Secrets Manager
- [ ] Database credentials rotated

### 2. Network Security
- [ ] Security groups properly configured
- [ ] VPC endpoints enabled
- [ ] WAF rules configured (production)
- [ ] GuardDuty enabled (production)

### 3. Data Protection
- [ ] S3 buckets encrypted
- [ ] RDS encryption enabled
- [ ] Backup retention configured
- [ ] CloudTrail logging enabled

## 📊 Monitoring Setup

### 1. CloudWatch Configuration
- [ ] Log groups created
- [ ] Metrics configured
- [ ] Alarms set up
- [ ] Dashboard accessible

### 2. Notification Setup
- [ ] SNS topics configured
- [ ] Email subscriptions confirmed
- [ ] SMS endpoints verified (production)
- [ ] Slack integration (optional)

## 🔄 CI/CD Setup

### 1. GitHub Repository
- [ ] Code pushed to GitHub
- [ ] Branch protection enabled
- [ ] Required reviewers configured

### 2. GitHub Secrets
- [ ] AWS credentials added:
  - [ ] `AWS_ACCESS_KEY_ID_DEV`
  - [ ] `AWS_SECRET_ACCESS_KEY_DEV`
  - [ ] `AWS_ACCESS_KEY_ID_PROD`
  - [ ] `AWS_SECRET_ACCESS_KEY_PROD`
- [ ] Backend configuration:
  - [ ] `TF_STATE_BUCKET`
  - [ ] `TF_STATE_LOCK_TABLE`
- [ ] Application secrets:
  - [ ] `SUPABASE_URL`
  - [ ] `SUPABASE_ANON_KEY`
  - [ ] Additional production secrets

### 3. Workflow Testing
- [ ] Create test PR to verify plan
- [ ] Review workflow logs
- [ ] Merge to main for production deploy

## 📱 Application Deployment

### 1. Build Application
```bash
# From root directory
pnpm install --legacy-peer-deps
pnpm run build
```

### 2. Deploy to Infrastructure
- [ ] Docker image built
- [ ] Image pushed to ECR
- [ ] ECS service updated
- [ ] Health checks passing

### 3. Mobile App Setup
- [ ] Expo build configured
- [ ] App Store credentials ready
- [ ] Google Play credentials ready
- [ ] OTA updates configured

## ✅ Post-Deployment Verification

### 1. Functionality Testing
- [ ] Web application accessible
- [ ] Authentication working
- [ ] Database connections verified
- [ ] API endpoints responding

### 2. Performance Testing
- [ ] Load testing completed
- [ ] CDN caching verified
- [ ] Auto-scaling tested
- [ ] Response times acceptable

### 3. Backup and Recovery
- [ ] Backup schedule verified
- [ ] Recovery procedure documented
- [ ] Disaster recovery plan tested

## 📋 Documentation

- [ ] Infrastructure documented
- [ ] Runbooks created
- [ ] Team trained on procedures
- [ ] Support contacts updated

## 🚨 Emergency Procedures

### Rollback Plan
1. Revert to previous Terraform state:
   ```bash
   terraform apply -target=<resource> -replace=<resource>
   ```
2. Or destroy and recreate:
   ```bash
   terraform destroy
   terraform apply
   ```

### Support Contacts
- AWS Support: [AWS Console](https://console.aws.amazon.com/support)
- Terraform Issues: Check error logs and state file
- Application Issues: Check CloudWatch logs

## 📝 Notes

- Always test in development first
- Keep terraform.tfstate files secure
- Regular backups are essential
- Monitor costs weekly
- Review security monthly

---

**Last Updated**: Use this checklist for each deployment to ensure consistency and reliability.
