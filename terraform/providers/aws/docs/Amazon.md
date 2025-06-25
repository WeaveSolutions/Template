# AWS Infrastructure for Nexpo

This directory contains the AWS-specific Terraform configurations for deploying the Nexpo application on Amazon Web Services.

## 🏗️ Architecture Overview

The AWS infrastructure includes:

- **Networking**: VPC with public/private subnets across multiple AZs
- **Compute**: AWS App Runner for containerized Next.js application
- **Storage**: S3 buckets for assets, uploads, and backups
- **Database**: RDS PostgreSQL with optional read replicas, DynamoDB tables
- **CDN**: CloudFront distribution for global content delivery
- **Security**: WAF, GuardDuty, Security Hub, and Secrets Manager
- **Monitoring**: CloudWatch dashboards, alarms, and X-Ray tracing

## 📁 Directory Structure

```
aws/
├── modules/                    # Reusable Terraform modules
│   ├── networking/            # VPC, subnets, security groups
│   ├── compute/              # App Runner, ECR
│   ├── storage/              # S3 buckets and policies
│   ├── database/             # RDS PostgreSQL, DynamoDB
│   ├── cdn/                  # CloudFront distribution
│   ├── monitoring/           # CloudWatch, alarms, dashboards
│   └── security/             # WAF, Secrets Manager, IAM
├── environments/              # Environment-specific configurations
│   ├── development/           # Development environment
│   │   ├── main.tf          # Module compositions
│   │   ├── variables.tf     # Environment variables
│   │   ├── outputs.tf       # Environment outputs
│   │   └── terraform.tfvars.example
│   ├── staging/              # Staging environment
│   │   ├── main.tf
│   │   ├── variables.tf
│   │   ├── outputs.tf
│   │   └── terraform.tfvars.example
│   └── production/           # Production environment
│       ├── main.tf
│       ├── variables.tf
│       ├── outputs.tf
│       └── terraform.tfvars.example
├── scripts/                   # Helper scripts
│   ├── deploy.sh             # Deployment automation
│   └── cleanup.sh            # Resource cleanup
├── docs/                      # Additional documentation
│   ├── CI_CD_SETUP.md        # CI/CD pipeline setup
│   ├── DEPLOYMENT_CHECKLIST.md # Pre-deployment checklist
│   └── QUICK_START.md        # Quick start guide
├── terraform.tfvars.example   # Example configuration
├── versions.tf               # Provider version constraints
├── Makefile                  # Common commands
└── README.md                 # This file
```

## 🚀 Quick Start

### Prerequisites

1. **AWS CLI**: Install and configure
   ```bash
   aws configure
   ```

2. **Terraform**: Version 1.5.0 or later
   ```bash
   terraform version
   ```

3. **Docker**: For building container images
   ```bash
   docker --version
   ```

### Configuration

1. **Copy the example configuration**:
   ```bash
   cp terraform.tfvars.example ../../aws.tfvars
   ```

2. **Edit the configuration**:
   ```bash
   # Edit ../../aws.tfvars with your AWS-specific settings
   vim ../../aws.tfvars
   ```

3. **Key configurations to update**:
   - `aws_region`: Your preferred AWS region
   - `availability_zones`: AZs in your region
   - `alarm_email_endpoints`: Email for alerts
   - `db_instance_class`: RDS instance size
   - `app_runner_cpu` and `app_runner_memory`: Container resources

### Deployment

From the root terraform directory:

```bash
# Initialize Terraform
terraform init

# Plan the deployment
terraform plan -var="enable_aws=true" \
               -var="environment=dev" \
               -var-file="aws.tfvars"

# Apply the configuration
terraform apply -var="enable_aws=true" \
                -var="environment=dev" \
                -var-file="aws.tfvars"
```

## 🔧 Module Details

### Networking Module
- Creates VPC with public, private, and database subnets
- NAT Gateways for private subnet internet access
- Security groups with least-privilege rules
- VPC Flow Logs for security monitoring

### Compute Module
- ECR repository for Docker images
- App Runner service with auto-scaling
- IAM roles and policies
- Integration with Secrets Manager

### Storage Module
- S3 buckets with encryption and versioning
- Lifecycle policies for cost optimization
- CloudFront origin access identity
- Backup bucket with cross-region replication (production)

### Database Module
- RDS PostgreSQL with Multi-AZ (production)
- Automated backups and snapshots
- DynamoDB tables for sessions and cache
- Secrets rotation for database credentials

### CDN Module
- CloudFront distribution with multiple origins
- Custom error pages
- Security headers via Lambda@Edge
- WAF integration for DDoS protection

### Monitoring Module
- CloudWatch dashboards for key metrics
- SNS topics for alert notifications
- X-Ray tracing for performance analysis
- Cost anomaly detection

### Security Module
- WAF with managed rule sets
- GuardDuty for threat detection
- Security Hub for compliance monitoring
- Config rules for governance

## 📊 Environment Configurations

### Development
- Minimal resources for cost optimization
- Single AZ deployment
- No read replicas
- Basic monitoring

### Staging
- Production-like configuration
- Multi-AZ deployment
- Reduced instance sizes
- Full monitoring

### Production
- High availability across AZs
- Read replicas for database
- Enhanced monitoring
- Full security suite

## 💰 Cost Optimization

### Development Environment
- App Runner: ~$5/month (minimum)
- RDS: ~$15/month (db.t3.micro)
- S3: ~$1/month
- **Total**: ~$25-30/month

### Production Environment
- App Runner: ~$50-200/month (based on traffic)
- RDS: ~$100-300/month (with Multi-AZ)
- CloudFront: ~$10-50/month
- S3: ~$5-20/month
- **Total**: ~$200-600/month

### Cost Saving Tips
1. Use Savings Plans for consistent workloads
2. Enable S3 lifecycle policies
3. Use Aurora Serverless for variable workloads
4. Set up budget alerts
5. Review Cost Explorer regularly

## 🔒 Security Best Practices

1. **Network Security**
   - Private subnets for compute and database
   - Security groups as virtual firewalls
   - NACLs for additional protection

2. **Data Security**
   - Encryption at rest for all storage
   - SSL/TLS for data in transit
   - Secrets Manager for credentials

3. **Access Control**
   - IAM roles instead of keys
   - Least privilege policies
   - MFA for console access

4. **Monitoring**
   - CloudTrail for API logging
   - GuardDuty for threat detection
   - Security Hub for compliance

## 🆘 Troubleshooting

### Common Issues

1. **App Runner deployment fails**
   - Check ECR repository permissions
   - Verify Docker image architecture (x86_64)
   - Review CloudWatch logs

2. **Database connection issues**
   - Verify security group rules
   - Check Secrets Manager permissions
   - Ensure RDS is in correct subnets

3. **High costs**
   - Review Cost Explorer
   - Check for unused resources
   - Enable cost allocation tags

### Useful Commands

```bash
# Check App Runner service status
aws apprunner list-services --region us-east-1

# View RDS instances
aws rds describe-db-instances --region us-east-1

# List S3 buckets
aws s3 ls

# View CloudWatch alarms
aws cloudwatch describe-alarms --region us-east-1
```

## 📚 Additional Resources

- [AWS App Runner Documentation](https://docs.aws.amazon.com/apprunner/)
- [RDS Best Practices](https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_BestPractices.html)
- [CloudFront Best Practices](https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/BestPractices.html)
- [AWS Well-Architected Framework](https://aws.amazon.com/architecture/well-architected/)

## 🚀 CI/CD Integration

See [CI_CD_SETUP.md](./CI_CD_SETUP.md) for GitHub Actions workflows that:
- Build and push Docker images to ECR
- Deploy to App Runner
- Run database migrations
- Invalidate CloudFront cache
