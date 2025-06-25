# OCI Deployment Checklist

Use this checklist to ensure a smooth deployment to Oracle Cloud Infrastructure.

## Pre-Deployment

### ✅ Account Setup
- [ ] OCI tenancy created and accessible
- [ ] Compartment structure defined
- [ ] Budget alerts configured
- [ ] Cost tracking tags defined

### ✅ Authentication & Access
- [ ] OCI CLI installed and configured
- [ ] API key generated for automation
- [ ] Service principal created for CI/CD
- [ ] Dynamic groups configured for instances
- [ ] IAM policies reviewed and applied

### ✅ Networking Preparation
- [ ] VCN CIDR ranges planned (no conflicts)
- [ ] Subnet strategy defined (public/private)
- [ ] Security lists requirements documented
- [ ] Network security groups planned
- [ ] DNS zones configured (if using OCI DNS)

### ✅ Auth0 Configuration
- [ ] Auth0 application created
- [ ] Callback URLs configured for OCI endpoints
- [ ] Client ID and secret stored securely
- [ ] Custom claims configured
- [ ] Rules/Actions tested

## Infrastructure Deployment

### ✅ Terraform Setup
- [ ] Terraform installed (v1.5.0+)
- [ ] Backend configured (Object Storage)
- [ ] Variables file created from template
- [ ] Sensitive variables stored in OCI Vault
- [ ] State file encryption enabled

### ✅ Core Infrastructure
```bash
# Initialize Terraform
cd terraform/providers/oci/environments/dev
terraform init

# Review planned changes
terraform plan -var-file="terraform.tfvars"
```

- [ ] VCN and subnets created
- [ ] Internet gateway configured
- [ ] NAT gateway configured
- [ ] Route tables configured
- [ ] Security lists applied

### ✅ Compute Resources
- [ ] OKE cluster provisioned
- [ ] Node pools configured with appropriate shapes
- [ ] Auto-scaling policies configured
- [ ] Container instances deployed
- [ ] Health checks verified

### ✅ Storage Setup
- [ ] Object Storage buckets created
- [ ] Lifecycle policies configured
- [ ] Block volumes provisioned
- [ ] File storage mounted
- [ ] Backup policies enabled

### ✅ Database Configuration
- [ ] Autonomous Database provisioned
- [ ] Wallet downloaded and secured
- [ ] Application schema created
- [ ] Connection strings tested
- [ ] Automated backups configured

### ✅ Container Registry
- [ ] Registry namespace created
- [ ] Repositories created for each service
- [ ] IAM policies for pull/push access
- [ ] Image scanning enabled
- [ ] Retention policies configured

## Application Deployment

### ✅ Container Images
```bash
# Build and push images
docker build -t ${REGION}.ocir.io/${NAMESPACE}/next-app:latest .
docker push ${REGION}.ocir.io/${NAMESPACE}/next-app:latest
```

- [ ] Next.js app image built and pushed
- [ ] Microservice images built and pushed
- [ ] Image tags follow naming convention
- [ ] Vulnerability scanning passed
- [ ] Images tested locally

### ✅ Kubernetes Deployment
```bash
# Deploy to OKE
kubectl apply -f k8s/namespace.yaml
kubectl apply -f k8s/secrets.yaml
kubectl apply -f k8s/deployments/
kubectl apply -f k8s/services/
```

- [ ] Namespaces created
- [ ] Secrets configured (Auth0, database, etc.)
- [ ] Deployments applied
- [ ] Services exposed
- [ ] Ingress configured with SSL

### ✅ Load Balancer Configuration
- [ ] Load balancer provisioned
- [ ] SSL certificates uploaded
- [ ] Backend sets configured
- [ ] Health checks passing
- [ ] WAF rules applied (if enabled)

## Post-Deployment

### ✅ Monitoring & Logging
- [ ] OCI Monitoring dashboards created
- [ ] Custom metrics configured
- [ ] Log groups created
- [ ] Log analytics enabled
- [ ] Alerts configured for:
  - [ ] High CPU/memory usage
  - [ ] Failed health checks
  - [ ] Error rate thresholds
  - [ ] Database connection issues

### ✅ Security Verification
- [ ] Network security groups reviewed
- [ ] Encryption at rest verified
- [ ] Encryption in transit configured
- [ ] Secrets rotated post-deployment
- [ ] Vulnerability scanning enabled
- [ ] Audit logging enabled

### ✅ Performance Testing
```bash
# Run performance tests
npm run test:performance
```

- [ ] Load testing completed
- [ ] Response times within SLA
- [ ] Auto-scaling tested
- [ ] Database performance optimized
- [ ] CDN caching verified

### ✅ Backup & Recovery
- [ ] Database backups tested
- [ ] Volume backups verified
- [ ] Disaster recovery plan documented
- [ ] RTO/RPO requirements met
- [ ] Restore procedures tested

### ✅ Documentation
- [ ] Architecture diagram updated
- [ ] Runbook created
- [ ] Troubleshooting guide written
- [ ] Team access documented
- [ ] Cost estimates validated

## Production Readiness

### ✅ Final Checks
- [ ] All health checks passing
- [ ] SSL certificates valid
- [ ] DNS records pointing correctly
- [ ] Auth0 integration working
- [ ] Database connections stable
- [ ] Monitoring alerts tested

### ✅ Rollback Plan
- [ ] Previous version tagged
- [ ] Database backup taken
- [ ] Rollback procedure documented
- [ ] Team briefed on rollback steps
- [ ] Rollback tested in staging

### ✅ Go-Live
- [ ] Stakeholders notified
- [ ] Maintenance window scheduled (if needed)
- [ ] Support team on standby
- [ ] Deployment executed
- [ ] Smoke tests passed
- [ ] User acceptance verified

## Ongoing Maintenance

### ✅ Regular Tasks
- [ ] Weekly security patches
- [ ] Monthly cost review
- [ ] Quarterly disaster recovery test
- [ ] Annual security audit
- [ ] Continuous monitoring review

### ✅ Optimization
- [ ] Resource utilization reviewed
- [ ] Cost optimization implemented
- [ ] Performance bottlenecks addressed
- [ ] Automation opportunities identified
- [ ] Technical debt tracked

## Troubleshooting Resources

### Common Issues
1. **OKE Node Issues**: Check node pool status and security lists
2. **Database Connection**: Verify wallet configuration and network routes
3. **Image Pull Errors**: Check OCIR credentials and IAM policies
4. **Load Balancer 502**: Verify backend health and security rules

### Support Contacts
- OCI Support: [Create SR](https://support.oracle.com)
- Internal Team: [Your contact info]
- Escalation: [Escalation path]

### Useful Commands
```bash
# Check cluster status
kubectl get nodes
kubectl get pods --all-namespaces

# View logs
kubectl logs -f deployment/next-app

# Check OCI resources
oci compute instance list --compartment-id ${COMPARTMENT_ID}
oci lb load-balancer list --compartment-id ${COMPARTMENT_ID}

# Database connection test
sqlplus admin@${DB_NAME}_high
```

## Sign-off

| Role | Name | Date | Signature |
|------|------|------|-----------|
| DevOps Lead | | | |
| Security | | | |
| Database Admin | | | |
| Project Manager | | | |

---

**Note**: This checklist should be customized based on your specific requirements and compliance needs.
