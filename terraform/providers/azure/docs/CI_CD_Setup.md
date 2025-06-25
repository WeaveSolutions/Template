# CI/CD Setup for Azure Terraform Infrastructure

This guide explains how to set up GitHub Actions for automated Terraform deployments to Azure.

## Overview

The CI/CD pipeline automatically manages your Azure infrastructure deployments with:
- Automated validation and formatting checks
- Environment-specific deployments (dev/staging/prod)
- Pull request plan previews
- Cost estimation using Infracost
- Automated security scanning
- Manual deployment options

## GitHub Secrets Configuration

### Required Secrets

#### 1. Azure Service Principal
```bash
# Service Principal for Terraform
ARM_CLIENT_ID           # Application (client) ID
ARM_CLIENT_SECRET       # Client secret value
ARM_SUBSCRIPTION_ID     # Azure subscription ID
ARM_TENANT_ID          # Directory (tenant) ID

# Per-environment (optional)
ARM_CLIENT_ID_DEV
ARM_CLIENT_SECRET_DEV
ARM_CLIENT_ID_PROD
ARM_CLIENT_SECRET_PROD
```

#### 2. Terraform State Backend
```bash
TF_STATE_STORAGE_ACCOUNT    # Storage account name for state
TF_STATE_CONTAINER          # Container name (default: tfstate)
TF_STATE_RESOURCE_GROUP     # Resource group of storage account
```

#### 3. Auth0 Configuration
```bash
AUTH0_TENANT               # Your Auth0 tenant name
AUTH0_TENANT_REGION        # Tenant region (e.g., us, eu)
AUTH0_CLIENT_ID            # Auth0 application client ID
AUTH0_CLIENT_SECRET        # Auth0 application client secret
AUTH0_DOMAIN               # Full Auth0 domain
```

## GitHub Actions Workflows

### 1. Pull Request Validation

Create `.github/workflows/azure-terraform-pr.yml`:

```yaml
name: Azure Terraform PR

on:
  pull_request:
    paths:
      - 'terraform/providers/azure/**'
      - 'terraform/main.tf'
      - 'terraform/variables.tf'

env:
  TF_VERSION: '1.5.0'
  WORKING_DIR: 'terraform'

jobs:
  validate:
    name: Validate Terraform
    runs-on: ubuntu-latest
    
    steps:
      - uses: actions/checkout@v4
      
      - name: Setup Terraform
        uses: hashicorp/setup-terraform@v3
        with:
          terraform_version: ${{ env.TF_VERSION }}
      
      - name: Azure Login
        uses: azure/login@v2
        with:
          creds: |
            {
              "clientId": "${{ secrets.ARM_CLIENT_ID }}",
              "clientSecret": "${{ secrets.ARM_CLIENT_SECRET }}",
              "subscriptionId": "${{ secrets.ARM_SUBSCRIPTION_ID }}",
              "tenantId": "${{ secrets.ARM_TENANT_ID }}"
            }
      
      - name: Terraform Init
        working-directory: ${{ env.WORKING_DIR }}
        run: |
          terraform init \
            -backend-config="storage_account_name=${{ secrets.TF_STATE_STORAGE_ACCOUNT }}" \
            -backend-config="container_name=${{ secrets.TF_STATE_CONTAINER }}" \
            -backend-config="key=azure/terraform.tfstate" \
            -backend-config="resource_group_name=${{ secrets.TF_STATE_RESOURCE_GROUP }}"
      
      - name: Terraform Format Check
        working-directory: ${{ env.WORKING_DIR }}
        run: terraform fmt -check -recursive
      
      - name: Terraform Validate
        working-directory: ${{ env.WORKING_DIR }}
        run: terraform validate
      
      - name: Terraform Plan
        working-directory: ${{ env.WORKING_DIR }}
        run: |
          terraform plan \
            -var="enable_azure=true" \
            -var="enable_aws=false" \
            -var="enable_gcp=false" \
            -var-file="providers/azure/terraform.tfvars" \
            -out=tfplan
      
      - name: Post Plan to PR
        uses: actions/github-script@v7
        if: github.event_name == 'pull_request'
        with:
          script: |
            const output = `#### Terraform Plan 📖
            
            <details><summary>Show Plan</summary>
            
            \`\`\`
            ${{ steps.plan.outputs.stdout }}
            \`\`\`
            
            </details>
            
            *Pushed by: @${{ github.actor }}, Action: \`${{ github.event_name }}\`*`;
            
            github.rest.issues.createComment({
              issue_number: context.issue.number,
              owner: context.repo.owner,
              repo: context.repo.repo,
              body: output
            })
```

### 2. Deploy Workflow

Create `.github/workflows/azure-terraform-deploy.yml`:

```yaml
name: Azure Terraform Deploy

on:
  push:
    branches:
      - main
      - develop
    paths:
      - 'terraform/providers/azure/**'
  workflow_dispatch:
    inputs:
      environment:
        description: 'Environment to deploy'
        required: true
        type: choice
        options:
          - dev
          - staging
          - prod

env:
  TF_VERSION: '1.5.0'
  WORKING_DIR: 'terraform'

jobs:
  deploy:
    name: Deploy to Azure
    runs-on: ubuntu-latest
    environment: ${{ inputs.environment || 'dev' }}
    
    steps:
      - uses: actions/checkout@v4
      
      - name: Setup Terraform
        uses: hashicorp/setup-terraform@v3
        with:
          terraform_version: ${{ env.TF_VERSION }}
      
      - name: Azure Login
        uses: azure/login@v2
        with:
          creds: |
            {
              "clientId": "${{ secrets.ARM_CLIENT_ID }}",
              "clientSecret": "${{ secrets.ARM_CLIENT_SECRET }}",
              "subscriptionId": "${{ secrets.ARM_SUBSCRIPTION_ID }}",
              "tenantId": "${{ secrets.ARM_TENANT_ID }}"
            }
      
      - name: Set Environment Variables
        run: |
          if [ "${{ inputs.environment || 'dev' }}" == "prod" ]; then
            echo "TF_WORKSPACE=prod" >> $GITHUB_ENV
            echo "TF_VAR_FILE=providers/azure/environments/prod/terraform.tfvars" >> $GITHUB_ENV
          elif [ "${{ inputs.environment || 'dev' }}" == "staging" ]; then
            echo "TF_WORKSPACE=staging" >> $GITHUB_ENV
            echo "TF_VAR_FILE=providers/azure/environments/staging/terraform.tfvars" >> $GITHUB_ENV
          else
            echo "TF_WORKSPACE=dev" >> $GITHUB_ENV
            echo "TF_VAR_FILE=providers/azure/environments/dev/terraform.tfvars" >> $GITHUB_ENV
          fi
      
      - name: Terraform Init
        working-directory: ${{ env.WORKING_DIR }}
        run: |
          terraform init \
            -backend-config="storage_account_name=${{ secrets.TF_STATE_STORAGE_ACCOUNT }}" \
            -backend-config="container_name=${{ secrets.TF_STATE_CONTAINER }}" \
            -backend-config="key=azure/${{ env.TF_WORKSPACE }}/terraform.tfstate" \
            -backend-config="resource_group_name=${{ secrets.TF_STATE_RESOURCE_GROUP }}"
      
      - name: Terraform Apply
        working-directory: ${{ env.WORKING_DIR }}
        run: |
          terraform apply \
            -var="enable_azure=true" \
            -var="enable_aws=false" \
            -var="enable_gcp=false" \
            -var="environment=${{ env.TF_WORKSPACE }}" \
            -var-file="${{ env.TF_VAR_FILE }}" \
            -auto-approve
      
      - name: Terraform Output
        working-directory: ${{ env.WORKING_DIR }}
        run: terraform output -json > outputs.json
      
      - name: Upload Outputs
        uses: actions/upload-artifact@v4
        with:
          name: terraform-outputs-${{ env.TF_WORKSPACE }}
          path: terraform/outputs.json

  deploy-auth-service:
    name: Deploy Auth0 Microservice
    needs: deploy
    runs-on: ubuntu-latest
    environment: ${{ inputs.environment || 'dev' }}
    
    steps:
      - uses: actions/checkout@v4
      
      - name: Azure Login
        uses: azure/login@v2
        with:
          creds: |
            {
              "clientId": "${{ secrets.ARM_CLIENT_ID }}",
              "clientSecret": "${{ secrets.ARM_CLIENT_SECRET }}",
              "subscriptionId": "${{ secrets.ARM_SUBSCRIPTION_ID }}",
              "tenantId": "${{ secrets.ARM_TENANT_ID }}"
            }
      
      - name: Download Terraform Outputs
        uses: actions/download-artifact@v4
        with:
          name: terraform-outputs-${{ inputs.environment || 'dev' }}
      
      - name: Deploy Auth Service
        run: |
          # Extract container app details from outputs
          CONTAINER_APP_ENV=$(jq -r '.container_app_environment_id.value' outputs.json)
          RESOURCE_GROUP=$(jq -r '.resource_group_name.value' outputs.json)
          
          # Deploy Auth0 microservice
          az containerapp create \
            --name auth-service \
            --resource-group $RESOURCE_GROUP \
            --environment $CONTAINER_APP_ENV \
            --image ghcr.io/${{ github.repository }}/auth-service:latest \
            --cpu 0.5 \
            --memory 1Gi \
            --min-replicas 1 \
            --max-replicas 10 \
            --env-vars \
              AUTH0_DOMAIN=${{ secrets.AUTH0_DOMAIN }} \
              AUTH0_CLIENT_ID=${{ secrets.AUTH0_CLIENT_ID }} \
              AUTH0_CLIENT_SECRET=secretref:auth0-secret \
            --secrets auth0-secret=${{ secrets.AUTH0_CLIENT_SECRET }}
```

### 3. Destroy Workflow

Create `.github/workflows/azure-terraform-destroy.yml`:

```yaml
name: Azure Terraform Destroy

on:
  workflow_dispatch:
    inputs:
      environment:
        description: 'Environment to destroy'
        required: true
        type: choice
        options:
          - dev
          - staging
      confirm:
        description: 'Type "destroy" to confirm'
        required: true

jobs:
  destroy:
    name: Destroy Infrastructure
    runs-on: ubuntu-latest
    if: github.event.inputs.confirm == 'destroy'
    
    steps:
      - uses: actions/checkout@v4
      
      - name: Setup Terraform
        uses: hashicorp/setup-terraform@v3
        with:
          terraform_version: '1.5.0'
      
      - name: Azure Login
        uses: azure/login@v2
        with:
          creds: |
            {
              "clientId": "${{ secrets.ARM_CLIENT_ID }}",
              "clientSecret": "${{ secrets.ARM_CLIENT_SECRET }}",
              "subscriptionId": "${{ secrets.ARM_SUBSCRIPTION_ID }}",
              "tenantId": "${{ secrets.ARM_TENANT_ID }}"
            }
      
      - name: Terraform Destroy
        working-directory: terraform
        run: |
          terraform init \
            -backend-config="storage_account_name=${{ secrets.TF_STATE_STORAGE_ACCOUNT }}" \
            -backend-config="container_name=${{ secrets.TF_STATE_CONTAINER }}" \
            -backend-config="key=azure/${{ inputs.environment }}/terraform.tfstate"
          
          terraform destroy \
            -var="enable_azure=true" \
            -var="environment=${{ inputs.environment }}" \
            -var-file="providers/azure/environments/${{ inputs.environment }}/terraform.tfvars" \
            -auto-approve
```

## Setting Up Azure Service Principal

### 1. Create Service Principal

```bash
# Create service principal with Contributor role
az ad sp create-for-rbac \
  --name "github-actions-terraform" \
  --role "Contributor" \
  --scopes /subscriptions/YOUR_SUBSCRIPTION_ID \
  --json-auth

# Save the output - you'll need:
# - clientId -> ARM_CLIENT_ID
# - clientSecret -> ARM_CLIENT_SECRET
# - subscriptionId -> ARM_SUBSCRIPTION_ID  
# - tenantId -> ARM_TENANT_ID
```

### 2. Create State Storage

```bash
# Create resource group
az group create \
  --name rg-terraform-state \
  --location eastus

# Create storage account
az storage account create \
  --name tfstateYOURNAME \
  --resource-group rg-terraform-state \
  --location eastus \
  --sku Standard_LRS \
  --encryption-services blob

# Create container
az storage container create \
  --name tfstate \
  --account-name tfstateYOURNAME \
  --auth-mode login
```

### 3. Grant Storage Access

```bash
# Get service principal object ID
SP_OBJECT_ID=$(az ad sp show --id YOUR_CLIENT_ID --query id -o tsv)

# Grant Storage Blob Data Contributor role
az role assignment create \
  --role "Storage Blob Data Contributor" \
  --assignee-object-id $SP_OBJECT_ID \
  --scope /subscriptions/YOUR_SUBSCRIPTION_ID/resourceGroups/rg-terraform-state
```

## Cost Optimization in CI/CD

### 1. Add Infracost

```yaml
- name: Setup Infracost
  uses: infracost/actions/setup@v2
  with:
    api-key: ${{ secrets.INFRACOST_API_KEY }}

- name: Generate Infracost Report
  run: |
    infracost breakdown \
      --path terraform \
      --terraform-var="enable_azure=true" \
      --terraform-var-file="providers/azure/terraform.tfvars" \
      --format json \
      --out-file /tmp/infracost.json

- name: Post Infracost comment
  uses: infracost/actions/comment@v1
  with:
    path: /tmp/infracost.json
    behavior: update
```

### 2. Auto-shutdown for Non-Prod

Add to your deployment workflow:

```yaml
- name: Configure Auto-shutdown
  if: inputs.environment != 'prod'
  run: |
    # Set auto-shutdown for Container Apps
    az containerapp revision set-mode \
      --name $APP_NAME \
      --resource-group $RG \
      --mode single
    
    # Configure scale to zero
    az containerapp update \
      --name $APP_NAME \
      --resource-group $RG \
      --min-replicas 0
```

## Security Scanning

### 1. Terraform Security Scan

```yaml
- name: Run Checkov
  id: checkov
  uses: bridgecrewio/checkov-action@master
  with:
    directory: terraform/providers/azure
    framework: terraform
    soft_fail: true
```

### 2. Container Scanning

```yaml
- name: Run Trivy scanner
  uses: aquasecurity/trivy-action@master
  with:
    image-ref: ghcr.io/${{ github.repository }}/auth-service:latest
    format: 'sarif'
    output: 'trivy-results.sarif'
```

## Best Practices

1. **Branch Protection**
   - Require PR reviews before merging
   - Run validation on all PRs
   - Enforce linear history

2. **Environment Separation**
   - Use different service principals per environment
   - Separate state files per environment
   - Use GitHub environments for approvals

3. **Monitoring**
   - Enable GitHub Actions monitoring
   - Set up alerts for failed deployments
   - Track deployment frequency

4. **Rollback Strategy**
   - Keep previous Terraform state versions
   - Use Container Apps revision management
   - Document rollback procedures

## Troubleshooting

### Common Issues

1. **Authentication Failures**
   ```bash
   # Verify service principal
   az login --service-principal \
     --username YOUR_CLIENT_ID \
     --password YOUR_CLIENT_SECRET \
     --tenant YOUR_TENANT_ID
   ```

2. **State Lock Issues**
   ```bash
   # Force unlock (use carefully)
   terraform force-unlock LOCK_ID
   ```

3. **Container App Deployment Failures**
   ```bash
   # Check logs
   az containerapp logs show \
     --name auth-service \
     --resource-group rg-Nexpo-dev
   ```

## Next Steps

1. Set up monitoring dashboards
2. Configure deployment notifications
3. Implement blue-green deployments
4. Add performance testing in CI/CD
