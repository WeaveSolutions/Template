# Azure Quick Start Guide

Get your Nexpo app running on Azure in 15 minutes!

## Prerequisites

- Azure CLI installed (`az --version`)
- Terraform installed (`terraform --version`)
- GitHub account with repository access
- Auth0 account (free tier works)

## 1. Azure Setup (5 minutes)

### Login to Azure
```bash
# Login to Azure
az login

# Set subscription (if you have multiple)
az account set --subscription "YOUR_SUBSCRIPTION_NAME"

# Get subscription ID
az account show --query id -o tsv
```

### Create Service Principal
```bash
# Create service principal for Terraform
az ad sp create-for-rbac \
  --name "terraform-sp" \
  --role "Contributor" \
  --scopes /subscriptions/$(az account show --query id -o tsv) \
  --json-auth > azure-creds.json

# Extract credentials (save these securely!)
cat azure-creds.json
```

### Create State Storage
```bash
# Variables
RESOURCE_GROUP="rg-terraform-state"
STORAGE_ACCOUNT="tfstate$RANDOM"
CONTAINER_NAME="tfstate"

# Create resource group
az group create --name $RESOURCE_GROUP --location eastus

# Create storage account
az storage account create \
  --resource-group $RESOURCE_GROUP \
  --name $STORAGE_ACCOUNT \
  --sku Standard_LRS \
  --encryption-services blob

# Create blob container
az storage container create \
  --name $CONTAINER_NAME \
  --account-name $STORAGE_ACCOUNT \
  --auth-mode login

echo "Storage Account: $STORAGE_ACCOUNT"
```

## 2. Auth0 Setup (5 minutes)

1. **Create Auth0 Application**
   - Go to [Auth0 Dashboard](https://manage.auth0.com)
   - Create new Application (Regular Web App)
   - Note down:
     - Domain: `your-tenant.auth0.com`
     - Client ID: `your-client-id`
     - Client Secret: `your-client-secret`

2. **Configure Application**
   - Allowed Callback URLs: 
     ```
     http://localhost:3000/api/auth/callback
     https://your-app.azurecontainerapps.io/api/auth/callback
     ```
   - Allowed Logout URLs:
     ```
     http://localhost:3000
     https://your-app.azurecontainerapps.io
     ```

3. **Create API**
   - Go to APIs → Create API
   - Name: `Next Solito Expo API`
   - Identifier: `https://api.your-app.com`
   - Signing Algorithm: RS256

## 3. Configure Environment (2 minutes)

### Create terraform.tfvars
```bash
cd terraform/providers/azure/environments/dev

# Copy example file
cp terraform.tfvars.example terraform.tfvars

# Edit with your values
cat > terraform.tfvars << EOF
project_name = "Nexpo"
environment = "dev"
location = "eastus"

# Auth0 Configuration
auth0_domain = "your-tenant.auth0.com"
auth0_client_id = "your-client-id"
auth0_client_secret = "your-client-secret"
auth0_audience = "https://api.your-app.com"

# Container Registry (optional, leave empty to create new)
container_registry_name = ""

# Networking
vnet_address_space = ["10.0.0.0/16"]
container_app_subnet_prefix = "10.0.1.0/24"
database_subnet_prefix = "10.0.2.0/24"

# Tags
tags = {
  Project = "NextSolitoExpo"
  ManagedBy = "Terraform"
}
EOF
```

### Set Environment Variables
```bash
# From azure-creds.json
export ARM_CLIENT_ID="your-client-id"
export ARM_CLIENT_SECRET="your-client-secret"
export ARM_SUBSCRIPTION_ID="your-subscription-id"
export ARM_TENANT_ID="your-tenant-id"

# State storage (from earlier)
export TF_STATE_STORAGE_ACCOUNT="your-storage-account"
export TF_STATE_CONTAINER="tfstate"
export TF_STATE_RESOURCE_GROUP="rg-terraform-state"
```

## 4. Deploy Infrastructure (3 minutes)

### Initialize Terraform
```bash
cd ../../../../  # Back to terraform root
terraform init \
  -backend-config="storage_account_name=$TF_STATE_STORAGE_ACCOUNT" \
  -backend-config="container_name=$TF_STATE_CONTAINER" \
  -backend-config="key=azure/dev/terraform.tfstate" \
  -backend-config="resource_group_name=$TF_STATE_RESOURCE_GROUP"
```

### Deploy
```bash
# Plan first
terraform plan \
  -var="enable_azure=true" \
  -var="enable_aws=false" \
  -var="enable_gcp=false" \
  -var-file="providers/azure/environments/dev/terraform.tfvars"

# Apply
terraform apply \
  -var="enable_azure=true" \
  -var="enable_aws=false" \
  -var="enable_gcp=false" \
  -var-file="providers/azure/environments/dev/terraform.tfvars" \
  -auto-approve
```

### Get Outputs
```bash
# Show all outputs
terraform output

# Get specific values
export RESOURCE_GROUP=$(terraform output -raw resource_group_name)
export ACR_NAME=$(terraform output -raw container_registry_name)
export CAE_NAME=$(terraform output -raw container_app_environment_name)
```

## 5. Deploy Application

### Build and Push Container
```bash
# Login to ACR
az acr login --name $ACR_NAME

# Build Auth Service
cd ../microservices/auth
docker build -t $ACR_NAME.azurecr.io/auth-service:latest .
docker push $ACR_NAME.azurecr.io/auth-service:latest

# Deploy to Container Apps
az containerapp create \
  --name auth-service \
  --resource-group $RESOURCE_GROUP \
  --environment $CAE_NAME \
  --image $ACR_NAME.azurecr.io/auth-service:latest \
  --target-port 8080 \
  --ingress external \
  --min-replicas 1 \
  --max-replicas 10 \
  --cpu 0.5 \
  --memory 1Gi \
  --env-vars \
    AUTH0_DOMAIN=$AUTH0_DOMAIN \
    AUTH0_CLIENT_ID=$AUTH0_CLIENT_ID \
    AUTH0_AUDIENCE=$AUTH0_AUDIENCE \
  --secrets auth0-secret=$AUTH0_CLIENT_SECRET
```

### Deploy Next.js App
```bash
# Build Next.js app
cd ../../../apps/next
docker build -t $ACR_NAME.azurecr.io/next-app:latest .
docker push $ACR_NAME.azurecr.io/next-app:latest

# Deploy
az containerapp create \
  --name next-app \
  --resource-group $RESOURCE_GROUP \
  --environment $CAE_NAME \
  --image $ACR_NAME.azurecr.io/next-app:latest \
  --target-port 3000 \
  --ingress external \
  --min-replicas 1 \
  --max-replicas 10 \
  --cpu 1 \
  --memory 2Gi
```

## 6. Verify Deployment

### Check Application Status
```bash
# List Container Apps
az containerapp list \
  --resource-group $RESOURCE_GROUP \
  --output table

# Get App URL
APP_URL=$(az containerapp show \
  --name next-app \
  --resource-group $RESOURCE_GROUP \
  --query properties.configuration.ingress.fqdn -o tsv)

echo "Your app is available at: https://$APP_URL"
```

### Test Auth Flow
1. Open `https://$APP_URL` in browser
2. Click "Sign In"
3. Use Auth0 to authenticate
4. Verify redirect back to app

## Quick Commands Reference

### View Logs
```bash
# Container App logs
az containerapp logs show \
  --name auth-service \
  --resource-group $RESOURCE_GROUP \
  --type console --follow

# Application Insights
az monitor app-insights query \
  --app APP_INSIGHTS_NAME \
  --resource-group $RESOURCE_GROUP \
  --query "traces | take 50"
```

### Scale Application
```bash
# Update scaling rules
az containerapp update \
  --name next-app \
  --resource-group $RESOURCE_GROUP \
  --min-replicas 2 \
  --max-replicas 20
```

### Update Application
```bash
# Build new version
docker build -t $ACR_NAME.azurecr.io/next-app:v2 .
docker push $ACR_NAME.azurecr.io/next-app:v2

# Deploy update
az containerapp update \
  --name next-app \
  --resource-group $RESOURCE_GROUP \
  --image $ACR_NAME.azurecr.io/next-app:v2
```

## Cleanup

### Destroy Everything
```bash
# Remove all resources
terraform destroy \
  -var="enable_azure=true" \
  -var="enable_aws=false" \
  -var="enable_gcp=false" \
  -var-file="providers/azure/environments/dev/terraform.tfvars" \
  -auto-approve

# Remove state storage (optional)
az group delete --name $TF_STATE_RESOURCE_GROUP --yes
```

## Next Steps

1. **Setup CI/CD**: Follow [CI/CD Setup Guide](./CI_CD_SETUP.md)
2. **Production Deploy**: Use [Deployment Checklist](./DEPLOYMENT_CHECKLIST.md)
3. **Add Custom Domain**: Configure custom domain in Container Apps
4. **Enable CDN**: Add Azure Front Door for global distribution
5. **Setup Monitoring**: Configure alerts and dashboards

## Troubleshooting

### Common Issues

**Container App not accessible**
```bash
# Check ingress configuration
az containerapp ingress show \
  --name next-app \
  --resource-group $RESOURCE_GROUP
```

**Auth0 callback error**
- Verify callback URLs in Auth0 dashboard
- Check environment variables in Container App
- Review auth service logs

**High costs**
```bash
# Set auto-scale to zero for dev
az containerapp update \
  --name next-app \
  --resource-group $RESOURCE_GROUP \
  --min-replicas 0
```

## Support

- [Azure Container Apps Docs](https://docs.microsoft.com/azure/container-apps/)
- [Auth0 Documentation](https://auth0.com/docs)
- [Terraform Azure Provider](https://registry.terraform.io/providers/hashicorp/azurerm/latest/docs)
