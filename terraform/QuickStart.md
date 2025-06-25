# Quick Start Guide - Multi-Cloud Terraform

This guide will help you quickly deploy the Nexpo infrastructure to your chosen cloud provider(s).

## 🚀 5-Minute Setup

### Step 1: Prerequisites

Ensure you have the following installed:
- Terraform >= 1.5.0
- Cloud CLI tools (for your chosen provider)
- Docker (for container deployments)

### Step 2: Choose Your Cloud Provider

```bash
# Clone the repository (if not already done)
git clone https://github.com/yourusername/Nexpo
cd Nexpo/terraform
```

### Step 3: Configure Your Deployment

1. **Copy the main configuration**:
   ```bash
   cp terraform.tfvars.example terraform.tfvars
   ```

2. **Edit terraform.tfvars** to enable your provider(s):
   ```hcl
   # Enable one or more providers in the .env file and do not share your .env file with anyone.
   enable_aws   = true
   enable_gcp   = false
   enable_azure = false
   
   # Set your project details
   project_name = "my-app"
   environment  = "dev"
   ```

3. **Copy provider-specific configuration**:
   
   **For AWS:**
   ```bash
   cp providers/aws/terraform.tfvars.example aws.tfvars
   # Edit aws.tfvars with your AWS settings
   ```
   
   **For GCP:**
   ```bash
   cp providers/gcp/terraform.tfvars.example gcp.tfvars
   # Edit gcp.tfvars with your GCP settings
   ```
   
   **For Azure:**
   ```bash
   cp providers/azure/terraform.tfvars.example azure.tfvars
   # Edit azure.tfvars with your Azure settings
   ```

### Step 4: Authenticate with Your Cloud Provider

**AWS:**
```bash
aws configure
# Enter your AWS Access Key ID and Secret Access Key
```

**GCP:**
```bash
gcloud auth application-default login
gcloud config set project YOUR_PROJECT_ID
```

**Azure:**
```bash
az login
az account set --subscription "Your Subscription Name"
```

### Step 5: Deploy!

```bash
# Initialize Terraform
terraform init

# Preview what will be created
terraform plan

# Deploy the infrastructure
terraform apply
```

## 📋 Essential Configuration

### Minimum Required Settings

**AWS (aws.tfvars):**
```hcl
aws_region = "us-east-1"
alarm_email_endpoints = ["your-email@example.com"]
```

**GCP (gcp.tfvars):**
```hcl
gcp_project_id = "your-project-id"
gcp_region = "us-central1"
notification_emails = ["your-email@example.com"]
```

**Azure (azure.tfvars):**
```hcl
location = "East US"
action_group_emails = ["your-email@example.com"]
```

## 🎯 Common Deployment Scenarios

### Single Cloud Deployment (Recommended for Starting)

```bash
# Deploy to AWS only
terraform apply -var="enable_aws=true" \
                -var="enable_gcp=false" \
                -var="enable_azure=false" \
                -var-file="aws.tfvars"
```

### Multi-Cloud Deployment

```bash
# Deploy to AWS and Azure
terraform apply -var="enable_aws=true" \
                -var="enable_gcp=false" \
                -var="enable_azure=true" \
                -var-file="aws.tfvars" \
                -var-file="azure.tfvars"
```

### Environment-Specific Deployment

```bash
# Deploy production environment
terraform apply -var="environment=prod" \
                -var="enable_aws=true" \
                -var-file="aws-prod.tfvars"
```

## 🔍 Verify Your Deployment

After deployment, Terraform will output important information:

```bash
# View outputs
terraform output

# Example output:
deployment_summary = {
  aws = {
    app_url = "https://your-app.us-east-1.awsapprunner.com"
    cdn_url = "https://d1234567890.cloudfront.net"
    database_endpoint = "your-db.cluster-xyz.us-east-1.rds.amazonaws.com"
  }
}
```

### Access Your Application

1. **Web Application**: Visit the `app_url` from the output
2. **CDN Assets**: Access static content via `cdn_url`
3. **Database**: Connect using the endpoint and credentials in secrets manager

## 🧹 Clean Up

To avoid ongoing charges, destroy resources when not needed:

```bash
# Destroy everything
terraform destroy

# Or destroy specific provider only
terraform destroy -target=module.aws
```

## 🚨 Common Issues & Solutions

### "Provider credentials not found"
- Ensure you've authenticated with your cloud provider CLI
- Check environment variables are set correctly

### "Resource already exists"
- Choose unique project names
- Check for existing resources in your cloud console

### "Quota exceeded"
- Request quota increases from your cloud provider
- Start with smaller instance sizes

### "Permission denied"
- Ensure your cloud account has necessary permissions
- See provider-specific README for required roles

## 📊 Estimated Costs

| Environment | AWS | GCP | Azure |
|-------------|-----|-----|-------|
| Development | $25-30/mo | $20-25/mo | $40-50/mo |
| Staging | $100-150/mo | $80-120/mo | $150-200/mo |
| Production | $200-600/mo | $150-500/mo | $350-1000/mo |

*Costs vary based on usage and region*

## 🆘 Getting Help

1. **Check provider-specific docs**:
   - [AWS Documentation](./providers/aws/README.md)
   - [GCP Documentation](./providers/gcp/README.md)
   - [Azure Documentation](./providers/azure/README.md)

2. **Review terraform logs**:
   ```bash
   export TF_LOG=DEBUG
   terraform apply
   ```

3. **Common commands**:
   ```bash
   terraform validate  # Check configuration syntax
   terraform fmt      # Format terraform files
   terraform refresh  # Update state file
   ```

## 🚀 Next Steps

1. **Set up CI/CD**: Automate deployments with GitHub Actions
2. **Configure monitoring**: Set up dashboards and alerts
3. **Enable backups**: Configure automated backups
4. **Add custom domain**: Point your domain to the application
5. **Scale up**: Adjust instance sizes and replica counts

Happy deploying! 🎉
