# Google Cloud Platform (GCP) Terraform Infrastructure

This directory contains the Terraform configuration for deploying the Nexpo application on Google Cloud Platform.

## Architecture Overview

### Components

1. **Networking**: VPC with public/private subnets, Cloud NAT, and firewall rules
2. **Storage**: Cloud Storage buckets for assets, uploads, and backups
3. **Database**: Cloud SQL (PostgreSQL) and Firestore for session management
4. **Compute**: Cloud Run for containerized application deployment
5. **CDN**: Cloud CDN with SSL certificates and Cloud Armor protection
6. **Monitoring**: Cloud Monitoring with dashboards, alerts, and budgets
7. **Security**: KMS encryption, Secret Manager, Binary Authorization, and VPC Service Controls

## Prerequisites

1. **GCP Account**: Create a GCP account and project
2. **APIs**: Enable required APIs (will be done automatically by Terraform)
3. **Service Account**: Create a service account with necessary permissions
4. **Terraform**: Install Terraform >= 1.5.0
5. **gcloud CLI**: Install and configure Google Cloud SDK

## Initial Setup

### 1. Authentication

```bash
# Login to GCP
gcloud auth login

# Set up application default credentials
gcloud auth application-default login

# Set your project
gcloud config set project YOUR_PROJECT_ID
```

### 2. Create Backend Bucket

```bash
# Create bucket for Terraform state
gsutil mb -p YOUR_PROJECT_ID gs://Nexpo-terraform-state

# Enable versioning
gsutil versioning set on gs://Nexpo-terraform-state
```

### 3. Enable Required APIs

```bash
# Enable essential APIs
gcloud services enable compute.googleapis.com
gcloud services enable run.googleapis.com
gcloud services enable cloudsql.googleapis.com
gcloud services enable storage.googleapis.com
```

## Environment Configuration

### Development Environment

1. Copy the example variables file:
```bash
cd environments/dev
cp terraform.tfvars.example terraform.tfvars
```

2. Edit `terraform.tfvars` with your values:
```hcl
project_id         = "your-gcp-project-id"
project_number     = "123456789012"
billing_account_id = "XXXXX-XXXXX-XXXXX"
region            = "us-central1"

# Supabase configuration
supabase_url         = "https://your-project.supabase.co"
supabase_anon_key    = "your-anon-key"
supabase_service_key = "your-service-key"

# Alerts
alert_email_endpoints = ["admin@example.com"]
```

### Production Environment

1. Copy and configure production variables:
```bash
cd environments/prod
cp terraform.tfvars.example terraform.tfvars
```

2. Additional production settings:
```hcl
custom_domain         = "app.yourdomain.com"
organization_id       = "123456789012"  # Optional
alert_sms_endpoints   = ["+1234567890"]
access_policy_id      = "your-access-policy-id"  # For VPC Service Controls
```

## Deployment

### Development Deployment

```bash
cd environments/dev

# Initialize Terraform
terraform init

# Review the plan
terraform plan

# Apply changes
terraform apply
```

### Production Deployment

```bash
cd environments/prod

# Initialize with backend
terraform init -backend-config="bucket=Nexpo-terraform-state"

# Review the plan
terraform plan

# Apply changes (consider using approval workflow)
terraform apply
```

## Module Configuration

### Networking Module
- Creates VPC with public and private subnets
- Sets up Cloud NAT for outbound connectivity
- Configures firewall rules
- Creates serverless VPC connector for Cloud Run

### Storage Module
- Creates three buckets: assets, uploads, backups
- Configures lifecycle policies
- Sets up CORS for web access
- Manages IAM bindings

### Database Module
- Provisions Cloud SQL PostgreSQL instance
- Configures high availability (production)
- Sets up read replicas (production)
- Creates Firestore database for sessions
- Manages automated backups

### Compute Module
- Deploys Cloud Run service
- Configures auto-scaling
- Sets up Cloud Build trigger
- Manages environment variables
- Handles VPC connectivity

### CDN Module
- Creates global load balancer
- Manages SSL certificates
- Configures backend buckets and services
- Sets up Cloud Armor security policies
- Handles URL mapping

### Monitoring Module
- Creates custom dashboards
- Sets up uptime checks
- Configures alert policies
- Manages notification channels
- Sets billing budgets

### Security Module
- Creates KMS keys for encryption
- Manages secrets in Secret Manager
- Configures Binary Authorization
- Sets up VPC Service Controls (production)
- Manages security scanning

## Cost Optimization

### Development Environment
- Uses minimal instance sizes
- No high availability
- Shorter backup retention
- Lower budget alerts

### Production Environment
- Right-sized instances with auto-scaling
- High availability where needed
- Efficient use of committed use discounts
- Regular cost reviews

## Security Best Practices

1. **Least Privilege**: Service accounts have minimal required permissions
2. **Encryption**: All data encrypted at rest and in transit
3. **Network Security**: Private subnets for databases, Cloud Armor for DDoS protection
4. **Secrets Management**: Using Secret Manager for sensitive data
5. **Compliance**: VPC Service Controls and Binary Authorization in production

## Monitoring and Alerts

- **Uptime Monitoring**: Health checks every minute
- **Error Rate Alerts**: Triggered on high 5xx errors
- **Database Alerts**: Connection limits and performance
- **Budget Alerts**: 50%, 90%, and 100% thresholds

## Disaster Recovery

1. **Backups**: Automated daily backups with point-in-time recovery
2. **Multi-Region**: Consider deploying to multiple regions
3. **Data Export**: Regular exports to Cloud Storage
4. **Infrastructure as Code**: Everything in Terraform for quick recovery

## Troubleshooting

### Common Issues

1. **API Not Enabled**:
   ```bash
   gcloud services enable SERVICE_NAME.googleapis.com
   ```

2. **Permission Denied**:
   - Check service account permissions
   - Verify project IAM bindings

3. **Quota Exceeded**:
   - Request quota increase in GCP Console
   - Check regional quotas

4. **Cloud Run Deployment Failed**:
   - Check container logs
   - Verify environment variables
   - Check VPC connectivity

## Clean Up

To destroy resources:

```bash
# Development
cd environments/dev
terraform destroy

# Production (be very careful!)
cd environments/prod
terraform destroy
```

## Support

For issues or questions:
1. Check GCP documentation
2. Review Terraform logs
3. Contact your DevOps team
