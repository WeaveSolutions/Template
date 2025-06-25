# GCP Deployment Checklist

Complete this checklist before deploying to Google Cloud Platform environments.

## Pre-Deployment Checklist

### 1. GCP Account Setup ✓
- [ ] GCP project created and active
- [ ] Billing account linked
- [ ] APIs enabled (Compute, Cloud Run, GKE, etc.)
- [ ] Budget alerts configured
- [ ] Quotas verified

### 2. Service Account & Permissions ✓
- [ ] Service account created for Terraform
- [ ] Required roles assigned:
  - [ ] Editor
  - [ ] Service Account Admin
  - [ ] Project IAM Admin
  - [ ] Storage Admin
- [ ] Service account key generated and secured
- [ ] Workload Identity configured (for GKE)

### 3. State Management ✓
- [ ] GCS bucket created for Terraform state
- [ ] Versioning enabled on bucket
- [ ] Lifecycle policies configured
- [ ] Bucket permissions properly set
- [ ] State locking configured

### 4. Auth0 Configuration ✓
- [ ] Auth0 tenant created
- [ ] Application registered in Auth0
- [ ] Callback URLs configured for Cloud Run URLs
- [ ] API created and configured
- [ ] Social connections set up (if needed)
- [ ] Custom domain configured (production)

### 5. Environment Variables ✓
```bash
# Verify all required variables are set
- [ ] GCP_PROJECT_ID
- [ ] GCP_SA_KEY (base64 encoded)
- [ ] TF_STATE_BUCKET
- [ ] AUTH0_TENANT
- [ ] AUTH0_TENANT_REGION
- [ ] AUTH0_CLIENT_ID
- [ ] AUTH0_CLIENT_SECRET
- [ ] AUTH0_DOMAIN
```

### 6. Networking ✓
- [ ] VPC network design documented
- [ ] Subnet CIDR ranges allocated
- [ ] Firewall rules defined
- [ ] Cloud NAT configured (if needed)
- [ ] Private Google Access enabled
- [ ] VPC connector for Cloud Run created

### 7. Security ✓
- [ ] Secret Manager enabled
- [ ] KMS keys created (if needed)
- [ ] IAM roles and bindings documented
- [ ] VPC Service Controls configured (if needed)
- [ ] Binary Authorization policies set
- [ ] Cloud Armor policies defined

### 8. Container Infrastructure ✓
- [ ] Artifact Registry repository created
- [ ] Cloud Run service configuration ready
- [ ] GKE cluster design (if using K8s)
- [ ] Cloud Build configuration
- [ ] Container scanning enabled

## Deployment Steps

### 1. Enable Required APIs
```bash
# Enable all required APIs
gcloud services enable \
  compute.googleapis.com \
  container.googleapis.com \
  containerregistry.googleapis.com \
  artifactregistry.googleapis.com \
  run.googleapis.com \
  cloudbuild.googleapis.com \
  secretmanager.googleapis.com \
  cloudkms.googleapis.com \
  monitoring.googleapis.com \
  logging.googleapis.com
```

### 2. Initialize Backend
```bash
# Dev environment
terraform init \
  -backend-config="bucket=${TF_STATE_BUCKET}" \
  -backend-config="prefix=gcp/dev/terraform.tfstate"
```

### 3. Review Plan
```bash
# Generate and review plan
terraform plan \
  -var="enable_gcp=true" \
  -var="enable_aws=false" \
  -var="enable_azure=false" \
  -var="gcp_project_id=${GCP_PROJECT_ID}" \
  -var-file="providers/gcp/environments/dev/terraform.tfvars" \
  -out=tfplan

# Review resources to be created
terraform show tfplan
```

### 4. Deploy Infrastructure
```bash
# Apply configuration
terraform apply tfplan

# Or direct apply with approval
terraform apply \
  -var="enable_gcp=true" \
  -var="gcp_project_id=${GCP_PROJECT_ID}" \
  -var-file="providers/gcp/environments/dev/terraform.tfvars"
```

### 5. Configure Artifact Registry
```bash
# Configure Docker authentication
gcloud auth configure-docker ${REGION}-docker.pkg.dev

# Verify registry
gcloud artifacts repositories list --location=${REGION}
```

### 6. Deploy Microservices
```bash
# Build and push Auth service
cd microservices/auth
docker build -t ${REGION}-docker.pkg.dev/${PROJECT_ID}/services/auth-service:latest .
docker push ${REGION}-docker.pkg.dev/${PROJECT_ID}/services/auth-service:latest

# Deploy to Cloud Run
gcloud run deploy auth-service \
  --image ${REGION}-docker.pkg.dev/${PROJECT_ID}/services/auth-service:latest \
  --platform managed \
  --region ${REGION} \
  --allow-unauthenticated \
  --set-env-vars AUTH0_DOMAIN=${AUTH0_DOMAIN} \
  --set-secrets AUTH0_CLIENT_SECRET=auth0-secret:latest
```

### 7. Configure DNS
```bash
# Get Cloud Run service URL
SERVICE_URL=$(gcloud run services describe auth-service \
  --platform managed \
  --region ${REGION} \
  --format 'value(status.url)')

# Update Auth0 callback URLs with the service URL
echo "Add this to Auth0 callbacks: ${SERVICE_URL}/callback"
```

## Post-Deployment Checklist

### 1. Verification ✓
- [ ] All resources created successfully
- [ ] Cloud Run services accessible
- [ ] Networking connectivity verified
- [ ] DNS resolution working
- [ ] SSL certificates valid

### 2. Security Validation ✓
- [ ] Secret Manager secrets accessible
- [ ] Service accounts have minimal permissions
- [ ] Firewall rules properly restrictive
- [ ] No public IPs exposed unnecessarily
- [ ] Cloud Armor rules active

### 3. Monitoring Setup ✓
- [ ] Cloud Monitoring workspace created
- [ ] Uptime checks configured
- [ ] Log-based metrics created
- [ ] Alerts configured
- [ ] Dashboards created

### 4. Auth0 Integration ✓
- [ ] Auth0 connection successful
- [ ] Login flow working
- [ ] Token validation functioning
- [ ] User profile sync working
- [ ] Logout flow tested

### 5. Performance Testing ✓
- [ ] Load testing completed
- [ ] Auto-scaling verified
- [ ] Response times acceptable
- [ ] Error rates within limits
- [ ] Database performance optimized

## Environment-Specific Configurations

### Development
- [ ] Scale-to-zero enabled
- [ ] Debug logging enabled
- [ ] Cost optimization rules
- [ ] Dev-only firewall rules

### Staging
- [ ] Production-like configuration
- [ ] Integration testing enabled
- [ ] Performance monitoring
- [ ] Backup procedures tested

### Production
- [ ] Multi-region deployment
- [ ] High availability configured
- [ ] Disaster recovery plan
- [ ] Backup automation
- [ ] Compliance requirements met

## Rollback Plan

### 1. Infrastructure Rollback
```bash
# Revert to previous state
terraform plan -destroy
terraform destroy -auto-approve

# Or restore from state backup
gsutil cp gs://${TF_STATE_BUCKET}/backup/terraform.tfstate .
terraform state push terraform.tfstate
```

### 2. Application Rollback
```bash
# List Cloud Run revisions
gcloud run revisions list \
  --service=auth-service \
  --platform=managed \
  --region=${REGION}

# Route traffic to previous revision
gcloud run services update-traffic auth-service \
  --to-revisions=REVISION_NAME=100 \
  --platform=managed \
  --region=${REGION}
```

## Troubleshooting Guide

### Common Issues

1. **Authentication Failures**
   - Verify service account permissions
   - Check API enablement
   - Validate project ID

2. **State Lock Issues**
   - Check for concurrent operations
   - Verify bucket permissions
   - Clear stale locks if needed

3. **Cloud Run Deployment Failures**
   - Check container logs
   - Verify image exists in registry
   - Validate environment variables

4. **Networking Issues**
   - Check VPC connector configuration
   - Verify firewall rules
   - Test with Cloud Shell

### Debug Commands
```bash
# View Cloud Run logs
gcloud run services logs read auth-service \
  --platform=managed \
  --region=${REGION} \
  --limit=100

# Check resource status
gcloud compute instances list
gcloud run services list --platform=managed
gcloud container clusters list

# View deployment history
gcloud deployment-manager deployments list
```

## Cost Optimization

### 1. Set Resource Limits
```bash
# Update Cloud Run service
gcloud run services update auth-service \
  --cpu=1 \
  --memory=512Mi \
  --max-instances=10 \
  --min-instances=0
```

### 2. Configure Autoscaling
```bash
# Set concurrency limits
gcloud run services update auth-service \
  --concurrency=100 \
  --cpu-throttling
```

### 3. Enable Cost Controls
```bash
# Set budget alerts
gcloud billing budgets create \
  --billing-account=${BILLING_ACCOUNT} \
  --display-name="Nexpo Budget" \
  --budget-amount=1000 \
  --threshold-rule=percent=50 \
  --threshold-rule=percent=90
```

## Sign-off

### Deployment Team
- [ ] Infrastructure deployed by: _____________
- [ ] Applications deployed by: _____________
- [ ] Security review by: _____________
- [ ] Final approval by: _____________

### Deployment Details
- Date: _____________
- Environment: _____________
- Version: _____________
- Ticket/Issue: _____________

## Next Steps

1. Schedule post-deployment review
2. Update runbooks
3. Train support team
4. Plan optimization phase
5. Schedule security audit
