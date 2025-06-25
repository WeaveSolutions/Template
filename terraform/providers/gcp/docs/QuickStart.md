# GCP Quick Start Guide

Get your Nexpo app running on Google Cloud Platform in 15 minutes!

## Prerequisites

- Google Cloud CLI installed (`gcloud --version`)
- Terraform installed (`terraform --version`)
- Docker installed (`docker --version`)
- GitHub account with repository access
- Auth0 account (free tier works)

## 1. GCP Setup (5 minutes)

### Create Project and Enable APIs
```bash
# Create new project (or use existing)
PROJECT_ID="Nexpo-dev"
gcloud projects create $PROJECT_ID --name="Next Solito Expo"

# Set as active project
gcloud config set project $PROJECT_ID

# Link billing account (required)
BILLING_ACCOUNT=$(gcloud billing accounts list --format="value(name)" --limit=1)
gcloud billing projects link $PROJECT_ID --billing-account=$BILLING_ACCOUNT

# Enable required APIs
gcloud services enable \
  compute.googleapis.com \
  run.googleapis.com \
  artifactregistry.googleapis.com \
  secretmanager.googleapis.com \
  vpcaccess.googleapis.com
```

### Create Service Account
```bash
# Create service account
gcloud iam service-accounts create terraform-sa \
  --display-name="Terraform Service Account"

# Get email
SA_EMAIL="terraform-sa@${PROJECT_ID}.iam.gserviceaccount.com"

# Grant roles
for role in editor iam.serviceAccountAdmin resourcemanager.projectIamAdmin storage.admin; do
  gcloud projects add-iam-policy-binding $PROJECT_ID \
    --member="serviceAccount:${SA_EMAIL}" \
    --role="roles/${role}"
done

# Create and download key
gcloud iam service-accounts keys create key.json --iam-account=$SA_EMAIL
```

### Create State Bucket
```bash
# Create bucket
BUCKET_NAME="tf-state-${PROJECT_ID}"
gsutil mb -p $PROJECT_ID gs://${BUCKET_NAME}/

# Enable versioning
gsutil versioning set on gs://${BUCKET_NAME}/

echo "State bucket: ${BUCKET_NAME}"
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
     https://auth-service-*.run.app/api/auth/callback
     ```
   - Allowed Logout URLs:
     ```
     http://localhost:3000
     https://auth-service-*.run.app
     ```

3. **Create API**
   - Go to APIs → Create API
   - Name: `Next Solito Expo API`
   - Identifier: `https://api.Nexpo.com`
   - Signing Algorithm: RS256

## 3. Configure Environment (2 minutes)

### Create terraform.tfvars
```bash
cd terraform/providers/gcp/environments/dev

# Create tfvars file
cat > terraform.tfvars << EOF
project_name = "Nexpo"
environment = "dev"
region = "us-central1"
zone = "us-central1-a"

# Auth0 Configuration
auth0_domain = "your-tenant.auth0.com"
auth0_client_id = "your-client-id"
auth0_client_secret = "your-client-secret"
auth0_audience = "https://api.Nexpo.com"

# Networking
vpc_cidr = "10.0.0.0/16"
public_subnet_cidr = "10.0.1.0/24"
private_subnet_cidr = "10.0.2.0/24"

# Cloud Run
cloud_run_cpu = "1"
cloud_run_memory = "512Mi"
cloud_run_min_instances = 0
cloud_run_max_instances = 10

# Tags
labels = {
  project = "Nexpo"
  environment = "dev"
  managed_by = "terraform"
}
EOF
```

### Set Environment Variables
```bash
# GCP credentials
export GOOGLE_APPLICATION_CREDENTIALS="$(pwd)/key.json"
export GCP_PROJECT_ID=$PROJECT_ID
export TF_STATE_BUCKET=$BUCKET_NAME

# Auth0 (for secrets)
export TF_VAR_auth0_client_secret="your-auth0-client-secret"
```

## 4. Deploy Infrastructure (3 minutes)

### Initialize Terraform
```bash
cd ../../../../  # Back to terraform root
terraform init \
  -backend-config="bucket=${TF_STATE_BUCKET}" \
  -backend-config="prefix=gcp/dev/terraform.tfstate"
```

### Deploy
```bash
# Plan first
terraform plan \
  -var="enable_gcp=true" \
  -var="enable_aws=false" \
  -var="enable_azure=false" \
  -var="gcp_project_id=${GCP_PROJECT_ID}" \
  -var-file="providers/gcp/environments/dev/terraform.tfvars"

# Apply
terraform apply \
  -var="enable_gcp=true" \
  -var="enable_aws=false" \
  -var="enable_azure=false" \
  -var="gcp_project_id=${GCP_PROJECT_ID}" \
  -var-file="providers/gcp/environments/dev/terraform.tfvars" \
  -auto-approve
```

### Get Outputs
```bash
# Show all outputs
terraform output

# Get specific values
export REGION=$(terraform output -raw region)
export REGISTRY_URL=$(terraform output -raw artifact_registry_url)
export VPC_CONNECTOR=$(terraform output -raw vpc_connector_name)
```

## 5. Deploy Application

### Configure Docker Auth
```bash
# Authenticate Docker to Artifact Registry
gcloud auth configure-docker ${REGION}-docker.pkg.dev
```

### Deploy Auth Service
```bash
# Build and push
cd ../microservices/auth
docker build -t ${REGISTRY_URL}/auth-service:latest .
docker push ${REGISTRY_URL}/auth-service:latest

# Create secret for Auth0
echo -n "your-auth0-client-secret" | \
  gcloud secrets create auth0-secret --data-file=-

# Deploy to Cloud Run
gcloud run deploy auth-service \
  --image ${REGISTRY_URL}/auth-service:latest \
  --platform managed \
  --region ${REGION} \
  --allow-unauthenticated \
  --vpc-connector ${VPC_CONNECTOR} \
  --set-env-vars \
    AUTH0_DOMAIN=your-tenant.auth0.com,\
    AUTH0_CLIENT_ID=your-client-id,\
    AUTH0_AUDIENCE=https://api.Nexpo.com \
  --set-secrets AUTH0_CLIENT_SECRET=auth0-secret:latest \
  --min-instances 0 \
  --max-instances 10
```

### Deploy Next.js App
```bash
# Build and push
cd ../../../apps/next
docker build -t ${REGISTRY_URL}/next-app:latest .
docker push ${REGISTRY_URL}/next-app:latest

# Deploy
gcloud run deploy next-app \
  --image ${REGISTRY_URL}/next-app:latest \
  --platform managed \
  --region ${REGION} \
  --allow-unauthenticated \
  --vpc-connector ${VPC_CONNECTOR} \
  --min-instances 0 \
  --max-instances 10
```

## 6. Verify Deployment

### Check Services
```bash
# List Cloud Run services
gcloud run services list --platform managed --region ${REGION}

# Get service URLs
AUTH_URL=$(gcloud run services describe auth-service \
  --platform managed --region ${REGION} \
  --format 'value(status.url)')

APP_URL=$(gcloud run services describe next-app \
  --platform managed --region ${REGION} \
  --format 'value(status.url)')

echo "Auth Service: ${AUTH_URL}"
echo "Next.js App: ${APP_URL}"
```

### Test Auth Flow
1. Open `${APP_URL}` in browser
2. Click "Sign In"
3. Authenticate with Auth0
4. Verify redirect back to app

## Quick Commands Reference

### View Logs
```bash
# Cloud Run logs
gcloud run services logs read auth-service \
  --platform managed \
  --region ${REGION} \
  --limit 50

# Streaming logs
gcloud alpha run services logs tail auth-service \
  --platform managed \
  --region ${REGION}
```

### Update Service
```bash
# Update environment variable
gcloud run services update auth-service \
  --platform managed \
  --region ${REGION} \
  --update-env-vars KEY=VALUE

# Update image
docker build -t ${REGISTRY_URL}/auth-service:v2 .
docker push ${REGISTRY_URL}/auth-service:v2

gcloud run services update auth-service \
  --platform managed \
  --region ${REGION} \
  --image ${REGISTRY_URL}/auth-service:v2
```

### Traffic Management
```bash
# Split traffic between revisions
gcloud run services update-traffic auth-service \
  --platform managed \
  --region ${REGION} \
  --to-latest=50

# List revisions
gcloud run revisions list \
  --platform managed \
  --region ${REGION} \
  --service auth-service
```

## Cost Optimization Tips

### Development Environment
```bash
# Scale to zero when not in use
gcloud run services update auth-service \
  --platform managed \
  --region ${REGION} \
  --min-instances 0

# Use Cloud Scheduler to stop services at night
gcloud scheduler jobs create http stop-dev-services \
  --schedule="0 19 * * *" \
  --uri="https://run.googleapis.com/v1/projects/${PROJECT_ID}/locations/${REGION}/services/auth-service" \
  --http-method=PATCH \
  --message-body='{"spec":{"template":{"metadata":{"annotations":{"run.googleapis.com/launch-stage":"GA"}},"spec":{"containerConcurrency":0}}}}' \
  --oauth-service-account-email=${SA_EMAIL}
```

## Cleanup

### Destroy Everything
```bash
# Remove all resources
terraform destroy \
  -var="enable_gcp=true" \
  -var="enable_aws=false" \
  -var="enable_azure=false" \
  -var="gcp_project_id=${GCP_PROJECT_ID}" \
  -var-file="providers/gcp/environments/dev/terraform.tfvars" \
  -auto-approve

# Delete project (optional - removes everything)
gcloud projects delete ${PROJECT_ID}
```

## Troubleshooting

### Common Issues

**Service not accessible**
```bash
# Check service status
gcloud run services describe auth-service \
  --platform managed \
  --region ${REGION}

# Check IAM permissions
gcloud run services get-iam-policy auth-service \
  --platform managed \
  --region ${REGION}
```

**Container fails to start**
```bash
# View detailed logs
gcloud logging read "resource.type=cloud_run_revision \
  AND resource.labels.service_name=auth-service" \
  --limit 50 \
  --format json
```

**Auth0 errors**
- Verify callback URLs include Cloud Run service URL
- Check Auth0 client credentials in Secret Manager
- Review Auth0 application logs

**High costs**
```bash
# View current usage
gcloud billing accounts get-iam-policy $BILLING_ACCOUNT

# Set budget alert
gcloud billing budgets create \
  --billing-account=$BILLING_ACCOUNT \
  --display-name="Dev Environment Budget" \
  --budget-amount=50
```

## Next Steps

1. **Setup CI/CD**: Follow [CI/CD Setup Guide](./CI_CD_SETUP.md)
2. **Production Deploy**: Use [Deployment Checklist](./DEPLOYMENT_CHECKLIST.md)
3. **Custom Domain**: Map custom domain to Cloud Run services
4. **Enable CDN**: Add Cloud CDN for static assets
5. **Setup Monitoring**: Configure alerts and dashboards

## Resources

- [Cloud Run Documentation](https://cloud.google.com/run/docs)
- [Terraform GCP Provider](https://registry.terraform.io/providers/hashicorp/google/latest/docs)
- [Auth0 Documentation](https://auth0.com/docs)
- [GCP Best Practices](https://cloud.google.com/docs/enterprise/best-practices-for-enterprise-organizations)
