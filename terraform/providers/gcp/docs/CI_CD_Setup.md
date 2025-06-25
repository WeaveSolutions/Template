# CI/CD Setup for GCP Terraform Infrastructure

This guide explains how to set up GitHub Actions for automated Terraform deployments to Google Cloud Platform.

## Overview

The CI/CD pipeline automatically manages your GCP infrastructure deployments with:
- Automated validation and formatting checks
- Environment-specific deployments (dev/staging/prod)
- Pull request plan previews
- Cost estimation using Infracost
- Automated security scanning
- Manual deployment options

## GitHub Secrets Configuration

### Required Secrets

#### 1. GCP Service Account
```bash
# Service Account for Terraform
GCP_PROJECT_ID          # Your GCP project ID
GCP_SA_KEY              # Service account JSON key (base64 encoded)

# Per-environment (optional)
GCP_PROJECT_ID_DEV
GCP_PROJECT_ID_STAGING
GCP_PROJECT_ID_PROD
GCP_SA_KEY_DEV
GCP_SA_KEY_STAGING
GCP_SA_KEY_PROD
```

#### 2. Terraform State Backend
```bash
TF_STATE_BUCKET         # GCS bucket for Terraform state
TF_STATE_PREFIX         # Prefix for state files (default: terraform/state)
```

#### 3. Auth0 Configuration
```bash
AUTH0_TENANT            # Your Auth0 tenant name
AUTH0_TENANT_REGION     # Tenant region (e.g., us, eu)
AUTH0_CLIENT_ID         # Auth0 application client ID
AUTH0_CLIENT_SECRET     # Auth0 application client secret
AUTH0_DOMAIN            # Full Auth0 domain
```

## GitHub Actions Workflows

### 1. Pull Request Validation

Create `.github/workflows/gcp-terraform-pr.yml`:

```yaml
name: GCP Terraform PR

on:
  pull_request:
    paths:
      - 'terraform/providers/gcp/**'
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
      
      - name: Authenticate to Google Cloud
        uses: google-github-actions/auth@v2
        with:
          credentials_json: ${{ secrets.GCP_SA_KEY }}
      
      - name: Set up Cloud SDK
        uses: google-github-actions/setup-gcloud@v2
        with:
          project_id: ${{ secrets.GCP_PROJECT_ID }}
      
      - name: Terraform Init
        working-directory: ${{ env.WORKING_DIR }}
        run: |
          terraform init \
            -backend-config="bucket=${{ secrets.TF_STATE_BUCKET }}" \
            -backend-config="prefix=gcp/terraform.tfstate"
      
      - name: Terraform Format Check
        working-directory: ${{ env.WORKING_DIR }}
        run: terraform fmt -check -recursive
      
      - name: Terraform Validate
        working-directory: ${{ env.WORKING_DIR }}
        run: terraform validate
      
      - name: Terraform Plan
        working-directory: ${{ env.WORKING_DIR }}
        id: plan
        run: |
          terraform plan \
            -var="enable_gcp=true" \
            -var="enable_aws=false" \
            -var="enable_azure=false" \
            -var="gcp_project_id=${{ secrets.GCP_PROJECT_ID }}" \
            -var-file="providers/gcp/terraform.tfvars" \
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

Create `.github/workflows/gcp-terraform-deploy.yml`:

```yaml
name: GCP Terraform Deploy

on:
  push:
    branches:
      - main
      - develop
    paths:
      - 'terraform/providers/gcp/**'
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
    name: Deploy to GCP
    runs-on: ubuntu-latest
    environment: ${{ inputs.environment || 'dev' }}
    
    steps:
      - uses: actions/checkout@v4
      
      - name: Setup Terraform
        uses: hashicorp/setup-terraform@v3
        with:
          terraform_version: ${{ env.TF_VERSION }}
      
      - name: Authenticate to Google Cloud
        uses: google-github-actions/auth@v2
        with:
          credentials_json: ${{ secrets.GCP_SA_KEY }}
      
      - name: Set up Cloud SDK
        uses: google-github-actions/setup-gcloud@v2
        with:
          project_id: ${{ secrets.GCP_PROJECT_ID }}
      
      - name: Set Environment Variables
        run: |
          if [ "${{ inputs.environment || 'dev' }}" == "prod" ]; then
            echo "TF_WORKSPACE=prod" >> $GITHUB_ENV
            echo "TF_VAR_FILE=providers/gcp/environments/prod/terraform.tfvars" >> $GITHUB_ENV
            echo "GCP_REGION=us-central1" >> $GITHUB_ENV
          elif [ "${{ inputs.environment || 'dev' }}" == "staging" ]; then
            echo "TF_WORKSPACE=staging" >> $GITHUB_ENV
            echo "TF_VAR_FILE=providers/gcp/environments/staging/terraform.tfvars" >> $GITHUB_ENV
            echo "GCP_REGION=us-east1" >> $GITHUB_ENV
          else
            echo "TF_WORKSPACE=dev" >> $GITHUB_ENV
            echo "TF_VAR_FILE=providers/gcp/environments/dev/terraform.tfvars" >> $GITHUB_ENV
            echo "GCP_REGION=us-central1" >> $GITHUB_ENV
          fi
      
      - name: Terraform Init
        working-directory: ${{ env.WORKING_DIR }}
        run: |
          terraform init \
            -backend-config="bucket=${{ secrets.TF_STATE_BUCKET }}" \
            -backend-config="prefix=gcp/${{ env.TF_WORKSPACE }}/terraform.tfstate"
      
      - name: Terraform Apply
        working-directory: ${{ env.WORKING_DIR }}
        run: |
          terraform apply \
            -var="enable_gcp=true" \
            -var="enable_aws=false" \
            -var="enable_azure=false" \
            -var="gcp_project_id=${{ secrets.GCP_PROJECT_ID }}" \
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
      
      - name: Authenticate to Google Cloud
        uses: google-github-actions/auth@v2
        with:
          credentials_json: ${{ secrets.GCP_SA_KEY }}
      
      - name: Set up Cloud SDK
        uses: google-github-actions/setup-gcloud@v2
        with:
          project_id: ${{ secrets.GCP_PROJECT_ID }}
      
      - name: Download Terraform Outputs
        uses: actions/download-artifact@v4
        with:
          name: terraform-outputs-${{ inputs.environment || 'dev' }}
      
      - name: Configure Docker for Artifact Registry
        run: |
          REGION=$(jq -r '.artifact_registry_location.value' outputs.json)
          gcloud auth configure-docker ${REGION}-docker.pkg.dev
      
      - name: Build and Push Auth Service
        run: |
          REGISTRY=$(jq -r '.artifact_registry_url.value' outputs.json)
          
          cd microservices/auth
          docker build -t ${REGISTRY}/auth-service:latest .
          docker push ${REGISTRY}/auth-service:latest
      
      - name: Deploy to Cloud Run
        run: |
          SERVICE_ACCOUNT=$(jq -r '.cloud_run_service_account.value' outputs.json)
          VPC_CONNECTOR=$(jq -r '.vpc_connector_name.value' outputs.json)
          
          gcloud run deploy auth-service \
            --image ${REGISTRY}/auth-service:latest \
            --platform managed \
            --region ${GCP_REGION} \
            --service-account ${SERVICE_ACCOUNT} \
            --vpc-connector ${VPC_CONNECTOR} \
            --set-env-vars \
              AUTH0_DOMAIN=${{ secrets.AUTH0_DOMAIN }},\
              AUTH0_CLIENT_ID=${{ secrets.AUTH0_CLIENT_ID }},\
              AUTH0_AUDIENCE=${{ secrets.AUTH0_AUDIENCE }} \
            --set-secrets AUTH0_CLIENT_SECRET=auth0-secret:latest \
            --min-instances 1 \
            --max-instances 100
```

### 3. Destroy Workflow

Create `.github/workflows/gcp-terraform-destroy.yml`:

```yaml
name: GCP Terraform Destroy

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
      
      - name: Authenticate to Google Cloud
        uses: google-github-actions/auth@v2
        with:
          credentials_json: ${{ secrets.GCP_SA_KEY }}
      
      - name: Terraform Destroy
        working-directory: terraform
        run: |
          terraform init \
            -backend-config="bucket=${{ secrets.TF_STATE_BUCKET }}" \
            -backend-config="prefix=gcp/${{ inputs.environment }}/terraform.tfstate"
          
          terraform destroy \
            -var="enable_gcp=true" \
            -var="gcp_project_id=${{ secrets.GCP_PROJECT_ID }}" \
            -var="environment=${{ inputs.environment }}" \
            -var-file="providers/gcp/environments/${{ inputs.environment }}/terraform.tfvars" \
            -auto-approve
```

## Setting Up GCP Service Account

### 1. Create Service Account

```bash
# Set project
export PROJECT_ID="your-project-id"
gcloud config set project $PROJECT_ID

# Create service account
gcloud iam service-accounts create terraform-sa \
  --display-name="Terraform Service Account" \
  --description="Service account for Terraform deployments"

# Get service account email
export SA_EMAIL="terraform-sa@${PROJECT_ID}.iam.gserviceaccount.com"
```

### 2. Grant Permissions

```bash
# Grant necessary roles
gcloud projects add-iam-policy-binding $PROJECT_ID \
  --member="serviceAccount:${SA_EMAIL}" \
  --role="roles/editor"

gcloud projects add-iam-policy-binding $PROJECT_ID \
  --member="serviceAccount:${SA_EMAIL}" \
  --role="roles/iam.serviceAccountAdmin"

gcloud projects add-iam-policy-binding $PROJECT_ID \
  --member="serviceAccount:${SA_EMAIL}" \
  --role="roles/resourcemanager.projectIamAdmin"

gcloud projects add-iam-policy-binding $PROJECT_ID \
  --member="serviceAccount:${SA_EMAIL}" \
  --role="roles/storage.admin"
```

### 3. Create Key and Store in GitHub

```bash
# Create key
gcloud iam service-accounts keys create key.json \
  --iam-account=$SA_EMAIL

# Base64 encode for GitHub secret
base64 key.json > key-base64.txt

# Copy contents of key-base64.txt to GitHub secret GCP_SA_KEY
cat key-base64.txt

# Clean up
rm key.json key-base64.txt
```

### 4. Create State Bucket

```bash
# Create bucket for Terraform state
gsutil mb -p $PROJECT_ID -c STANDARD -l us-central1 gs://terraform-state-$PROJECT_ID/

# Enable versioning
gsutil versioning set on gs://terraform-state-$PROJECT_ID/

# Set lifecycle rule to delete old versions after 30 days
cat > lifecycle.json << EOF
{
  "lifecycle": {
    "rule": [
      {
        "action": {"type": "Delete"},
        "condition": {
          "age": 30,
          "isLive": false
        }
      }
    ]
  }
}
EOF

gsutil lifecycle set lifecycle.json gs://terraform-state-$PROJECT_ID/
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
      --terraform-var="enable_gcp=true" \
      --terraform-var="gcp_project_id=${{ secrets.GCP_PROJECT_ID }}" \
      --terraform-var-file="providers/gcp/terraform.tfvars" \
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
    # Set Cloud Run to scale to zero
    gcloud run services update auth-service \
      --region $GCP_REGION \
      --min-instances 0
    
    # Schedule stop for GKE clusters
    gcloud compute instances list \
      --filter="name~'gke-.*-${{ inputs.environment }}-.*'" \
      --format="value(name,zone)" | while read INSTANCE ZONE; do
      gcloud compute instances add-metadata $INSTANCE \
        --zone=$ZONE \
        --metadata=startup-script='#!/bin/bash
        if [ $(date +%H) -ge 19 ] || [ $(date +%H) -lt 7 ]; then
          shutdown -h now
        fi'
    done
```

## Security Scanning

### 1. Terraform Security Scan

```yaml
- name: Run Checkov
  id: checkov
  uses: bridgecrewio/checkov-action@master
  with:
    directory: terraform/providers/gcp
    framework: terraform
    soft_fail: true

- name: Run tfsec
  uses: aquasecurity/tfsec-action@v1.0.0
  with:
    working_directory: terraform/providers/gcp
```

### 2. Container Scanning

```yaml
- name: Run Trivy scanner
  uses: aquasecurity/trivy-action@master
  with:
    image-ref: ${{ env.REGISTRY }}/auth-service:latest
    format: 'sarif'
    output: 'trivy-results.sarif'

- name: Upload Trivy results
  uses: github/codeql-action/upload-sarif@v2
  with:
    sarif_file: 'trivy-results.sarif'
```

## Best Practices

1. **Branch Protection**
   - Require PR reviews before merging
   - Run validation on all PRs
   - Enforce linear history

2. **Environment Separation**
   - Use different projects for prod/non-prod
   - Separate service accounts per environment
   - Use workload identity for GKE

3. **Monitoring**
   - Enable Cloud Monitoring
   - Set up alerts for failed deployments
   - Track deployment metrics

4. **Rollback Strategy**
   - Use Cloud Run traffic splitting
   - Keep previous container images
   - Document rollback procedures

## Troubleshooting

### Common Issues

1. **Authentication Failures**
   ```bash
   # Verify service account
   gcloud auth activate-service-account --key-file=key.json
   gcloud auth list
   ```

2. **State Lock Issues**
   ```bash
   # List locks
   gsutil ls -L gs://terraform-state-$PROJECT_ID/
   
   # Force unlock (use carefully)
   terraform force-unlock LOCK_ID
   ```

3. **Cloud Run Deployment Failures**
   ```bash
   # Check logs
   gcloud run services logs read auth-service \
     --region=$GCP_REGION \
     --limit=50
   ```

## Next Steps

1. Set up monitoring dashboards
2. Configure deployment notifications
3. Implement blue-green deployments
4. Add load testing in CI/CD
