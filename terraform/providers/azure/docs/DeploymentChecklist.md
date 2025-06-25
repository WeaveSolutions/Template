# Azure Deployment Checklist

Complete this checklist before deploying to Azure environments.

## Pre-Deployment Checklist

### 1. Azure Account Setup ✓
- [ ] Azure subscription created and active
- [ ] Billing alerts configured
- [ ] Resource quotas verified
- [ ] Cost management policies in place

### 2. Service Principal & Permissions ✓
- [ ] Service principal created for Terraform
- [ ] Contributor role assigned at subscription level
- [ ] Storage Blob Data Contributor for state storage
- [ ] Key Vault Secrets Officer for secrets management
- [ ] Additional roles for specific services

### 3. State Management ✓
- [ ] Storage account created for Terraform state
- [ ] Container created with proper access
- [ ] State file encryption enabled
- [ ] Versioning enabled on storage account
- [ ] Soft delete configured

### 4. Auth0 Configuration ✓
- [ ] Auth0 tenant created
- [ ] Application registered in Auth0
- [ ] Callback URLs configured
- [ ] API created and configured
- [ ] Social connections set up (if needed)
- [ ] Custom domain configured (production)

### 5. Environment Variables ✓
```bash
# Verify all required variables are set
- [ ] ARM_CLIENT_ID
- [ ] ARM_CLIENT_SECRET
- [ ] ARM_SUBSCRIPTION_ID
- [ ] ARM_TENANT_ID
- [ ] TF_STATE_STORAGE_ACCOUNT
- [ ] TF_STATE_CONTAINER
- [ ] TF_STATE_RESOURCE_GROUP
- [ ] AUTH0_TENANT
- [ ] AUTH0_TENANT_REGION
- [ ] AUTH0_CLIENT_ID
- [ ] AUTH0_CLIENT_SECRET
- [ ] AUTH0_DOMAIN
```

### 6. Networking ✓
- [ ] Virtual Network CIDR ranges planned
- [ ] Subnet allocation documented
- [ ] NSG rules defined
- [ ] Private endpoints identified
- [ ] DNS zones configured
- [ ] Application Gateway/Load Balancer planned

### 7. Security ✓
- [ ] Key Vault provisioned
- [ ] Managed identities planned
- [ ] RBAC assignments documented
- [ ] Network security rules defined
- [ ] WAF policies configured
- [ ] DDoS protection enabled (if needed)

### 8. Container Apps Environment ✓
- [ ] Container registry created/selected
- [ ] Log Analytics workspace created
- [ ] Application Insights configured
- [ ] Scaling rules defined
- [ ] Health probes configured
- [ ] Ingress rules set

## Deployment Steps

### 1. Initialize Backend
```bash
# Dev environment
terraform init \
  -backend-config="storage_account_name=${TF_STATE_STORAGE_ACCOUNT}" \
  -backend-config="container_name=${TF_STATE_CONTAINER}" \
  -backend-config="key=azure/dev/terraform.tfstate" \
  -backend-config="resource_group_name=${TF_STATE_RESOURCE_GROUP}"
```

### 2. Review Plan
```bash
# Generate and review plan
terraform plan \
  -var="enable_azure=true" \
  -var="enable_aws=false" \
  -var="enable_gcp=false" \
  -var-file="providers/azure/environments/dev/terraform.tfvars" \
  -out=tfplan

# Review resources to be created
terraform show tfplan
```

### 3. Deploy Infrastructure
```bash
# Apply configuration
terraform apply tfplan

# Or direct apply with approval
terraform apply \
  -var="enable_azure=true" \
  -var-file="providers/azure/environments/dev/terraform.tfvars"
```

### 4. Deploy Microservices
```bash
# Deploy Auth service
az containerapp create \
  --name auth-service \
  --resource-group rg-Nexpo-dev \
  --environment cae-Nexpo-dev \
  --image your-registry.azurecr.io/auth-service:latest \
  --cpu 0.5 --memory 1Gi \
  --min-replicas 1 --max-replicas 10 \
  --ingress external --target-port 8080
```

### 5. Configure DNS
```bash
# Get Container App FQDN
FQDN=$(az containerapp show \
  --name auth-service \
  --resource-group rg-Nexpo-dev \
  --query properties.configuration.ingress.fqdn -o tsv)

# Update Auth0 callback URLs
# Add https://$FQDN/callback to Auth0 app settings
```

## Post-Deployment Checklist

### 1. Verification ✓
- [ ] All resources created successfully
- [ ] Container Apps running and healthy
- [ ] Networking connectivity verified
- [ ] DNS resolution working
- [ ] SSL certificates valid

### 2. Security Validation ✓
- [ ] Key Vault secrets accessible
- [ ] Managed identities working
- [ ] Network restrictions applied
- [ ] WAF rules active
- [ ] No public endpoints exposed unnecessarily

### 3. Monitoring Setup ✓
- [ ] Application Insights connected
- [ ] Log Analytics collecting logs
- [ ] Alerts configured
- [ ] Dashboards created
- [ ] Cost alerts set

### 4. Auth0 Integration ✓
- [ ] Auth0 connection successful
- [ ] Login flow working
- [ ] Token validation functioning
- [ ] User profile sync working
- [ ] Logout flow tested

### 5. Performance Testing ✓
- [ ] Load testing completed
- [ ] Scaling verified
- [ ] Response times acceptable
- [ ] Error rates within limits
- [ ] Database performance optimized

## Environment-Specific Configurations

### Development
- [ ] Auto-shutdown configured
- [ ] Minimal replicas
- [ ] Debug logging enabled
- [ ] Cost optimization rules

### Staging
- [ ] Production-like configuration
- [ ] Integration testing enabled
- [ ] Performance monitoring
- [ ] Backup procedures tested

### Production
- [ ] High availability configured
- [ ] Disaster recovery plan
- [ ] Backup automation
- [ ] Security scanning enabled
- [ ] Compliance requirements met

## Rollback Plan

### 1. Infrastructure Rollback
```bash
# Revert to previous state
terraform plan -destroy
terraform destroy -auto-approve

# Or revert to specific version
terraform state pull > backup.tfstate
```

### 2. Application Rollback
```bash
# List revisions
az containerapp revision list \
  --name auth-service \
  --resource-group rg-Nexpo-dev

# Activate previous revision
az containerapp revision activate \
  --name auth-service \
  --resource-group rg-Nexpo-dev \
  --revision PREVIOUS_REVISION_NAME
```

## Troubleshooting Guide

### Common Issues

1. **Authentication Failures**
   - Verify service principal credentials
   - Check role assignments
   - Validate tenant ID

2. **State Lock Issues**
   - Check for concurrent operations
   - Force unlock if necessary
   - Verify storage account access

3. **Container App Failures**
   - Check container logs
   - Verify environment variables
   - Validate health probes

4. **Networking Issues**
   - Verify NSG rules
   - Check subnet associations
   - Validate DNS configuration

### Debug Commands
```bash
# View Container App logs
az containerapp logs show \
  --name auth-service \
  --resource-group rg-Nexpo-dev \
  --type console

# Check resource status
az resource list \
  --resource-group rg-Nexpo-dev \
  --output table

# View deployment history
az deployment group list \
  --resource-group rg-Nexpo-dev \
  --output table
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
2. Update documentation
3. Train support team
4. Plan optimization phase
