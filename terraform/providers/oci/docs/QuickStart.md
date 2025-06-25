# OCI Quick Start Guide

This guide will help you quickly deploy the Nexpo application on Oracle Cloud Infrastructure (OCI) with Auth0 integration.

## Prerequisites

1. **OCI Account**
   - Sign up at [cloud.oracle.com](https://cloud.oracle.com)
   - Create a compartment for your project

2. **Auth0 Account**
   - Sign up at [auth0.com](https://auth0.com)
   - Create a new application

3. **Tools Installation**
   ```bash
   # Install OCI CLI
   # For Windows (PowerShell as Administrator)
   Set-ExecutionPolicy RemoteSigned
   Invoke-WebRequest https://raw.githubusercontent.com/oracle/oci-cli/master/scripts/install/install.ps1 -OutFile install.ps1
   ./install.ps1
   
   # For macOS/Linux
   bash -c "$(curl -L https://raw.githubusercontent.com/oracle/oci-cli/master/scripts/install/install.sh)"
   
   # Install Terraform
   # Download from https://www.terraform.io/downloads
   
   # Install kubectl
   # Download from https://kubernetes.io/docs/tasks/tools/
   ```

## Step 1: Configure OCI CLI

```bash
oci setup config
```

When prompted, provide:
- User OCID: Found in OCI Console > Identity > Users
- Tenancy OCID: Found in OCI Console > Administration > Tenancy Details
- Region: e.g., us-phoenix-1
- Generate new API key pair: Yes
- Key location: Accept default or specify custom path

## Step 2: Create Auth0 Application

1. Log in to Auth0 Dashboard
2. Create a new Single Page Application
3. Configure allowed URLs:
   ```
   Allowed Callback URLs:
   http://localhost:3000/api/auth/callback
   https://your-domain.com/api/auth/callback
   
   Allowed Logout URLs:
   http://localhost:3000
   https://your-domain.com
   
   Allowed Web Origins:
   http://localhost:3000
   https://your-domain.com
   ```
4. Note down:
   - Domain: `your-tenant.auth0.com`
   - Client ID
   - Client Secret

## Step 3: Set Up OCI Resources

### Create Compartment

```bash
# Create compartment
oci iam compartment create \
  --compartment-id <root-compartment-ocid> \
  --name "Nexpo" \
  --description "Nexpo Application"
```

### Generate API Key (if not done in setup)

```bash
# Generate API key pair
openssl genrsa -out ~/.oci/oci_api_key.pem 2048
openssl rsa -pubout -in ~/.oci/oci_api_key.pem -out ~/.oci/oci_api_key_public.pem
```

Upload the public key to OCI:
1. Go to OCI Console > Identity > Users > Your User
2. Click "API Keys" > "Add API Key"
3. Upload the public key file

## Step 4: Configure Terraform Variables

```bash
cd terraform/providers/oci/environments/dev

# Copy example file
cp ../../terraform.tfvars.example terraform.tfvars

# Edit terraform.tfvars with your values
```

Example `terraform.tfvars`:
```hcl
# OCI Authentication
tenancy_ocid     = "ocid1.tenancy.oc1..xxxxxxxxxx"
user_ocid        = "ocid1.user.oc1..xxxxxxxxxx"
fingerprint      = "xx:xx:xx:xx:xx:xx:xx:xx"
private_key_path = "~/.oci/oci_api_key.pem"
region           = "us-phoenix-1"

# Compartment
compartment_ocid = "ocid1.compartment.oc1..xxxxxxxxxx"

# Project
project_name = "Nexpo"
environment  = "dev"

# Networking
vcn_cidr_block       = "10.0.0.0/16"
public_subnet_cidr   = "10.0.1.0/24"
private_subnet_cidr  = "10.0.2.0/24"
database_subnet_cidr = "10.0.3.0/24"

# Auth0
auth0_domain         = "your-tenant.auth0.com"
auth0_client_id      = "your-client-id"
auth0_client_secret  = "your-client-secret"
auth0_api_identifier = "https://your-app.com/api"
```

## Step 5: Deploy Infrastructure

```bash
# Initialize Terraform
terraform init

# Plan deployment
terraform plan

# Apply configuration
terraform apply -auto-approve
```

## Step 6: Build and Push Container Images

```bash
# Get Container Registry endpoint from Terraform output
export REGISTRY_ENDPOINT=$(terraform output -raw registry_endpoint)

# Login to OCI Registry
docker login $REGISTRY_ENDPOINT

# Build and push images
cd ../../../../  # Go to project root

# Build Next.js app
docker build -f apps/next/Dockerfile -t $REGISTRY_ENDPOINT/Nexpo/next:latest .
docker push $REGISTRY_ENDPOINT/Nexpo/next:latest

# Build microservices
docker build -f microservices/api-gateway/Dockerfile -t $REGISTRY_ENDPOINT/Nexpo/api-gateway:latest .
docker push $REGISTRY_ENDPOINT/Nexpo/api-gateway:latest

docker build -f microservices/auth/Dockerfile -t $REGISTRY_ENDPOINT/Nexpo/auth:latest .
docker push $REGISTRY_ENDPOINT/Nexpo/auth:latest
```

## Step 7: Deploy Applications to Kubernetes

```bash
# Get kubeconfig
export KUBECONFIG=$(terraform output -raw kubeconfig_path)

# Deploy applications
kubectl apply -f k8s/namespace.yaml
kubectl apply -f k8s/deployments/
kubectl apply -f k8s/services/
kubectl apply -f k8s/ingress.yaml
```

## Step 8: Configure DNS

1. Get Load Balancer IP:
   ```bash
   kubectl get ingress -n Nexpo-dev
   ```

2. Update your domain's DNS records:
   - Type: A
   - Name: @ (or subdomain)
   - Value: Load Balancer IP

## Step 9: Verify Deployment

```bash
# Check pod status
kubectl get pods -n Nexpo-dev

# Check services
kubectl get services -n Nexpo-dev

# View logs
kubectl logs -n Nexpo-dev deployment/next-app

# Access application
curl https://your-domain.com
```

## Environment Variables

Set these in your CI/CD pipeline or local environment:

```bash
# OCI Variables
export TF_VAR_tenancy_ocid="ocid1.tenancy.oc1..xxxxx"
export TF_VAR_user_ocid="ocid1.user.oc1..xxxxx"
export TF_VAR_fingerprint="xx:xx:xx:xx:xx:xx:xx:xx"
export TF_VAR_private_key_path="~/.oci/oci_api_key.pem"
export TF_VAR_region="us-phoenix-1"
export TF_VAR_compartment_ocid="ocid1.compartment.oc1..xxxxx"

# Auth0 Variables
export TF_VAR_auth0_domain="your-tenant.auth0.com"
export TF_VAR_auth0_client_id="your-client-id"
export TF_VAR_auth0_client_secret="your-client-secret"
export TF_VAR_auth0_api_identifier="https://your-app.com/api"
```

## Troubleshooting

### OCI CLI Issues
```bash
# Test OCI CLI configuration
oci iam user get --user-id <your-user-ocid>

# List compartments
oci iam compartment list --all
```

### Kubernetes Connection Issues
```bash
# Update kubeconfig
oci ce cluster create-kubeconfig --cluster-id <cluster-ocid> --file $HOME/.kube/config

# Test connection
kubectl cluster-info
```

### Container Registry Issues
```bash
# Generate auth token
oci iam auth-token create --description "Registry Access" --user-id <your-user-ocid>

# Login with username: <tenancy-namespace>/<username>
docker login <region>.ocir.io
```

### Pod CrashLoopBackOff
```bash
# Check pod logs
kubectl logs -n Nexpo-dev <pod-name> --previous

# Describe pod
kubectl describe pod -n Nexpo-dev <pod-name>
```

## Cost Optimization Tips

1. **Use Always Free Resources**:
   - 2 Oracle Autonomous Databases
   - 2 Compute VMs
   - 4 VCPUs and 24 GB memory for VMs
   - 10 GB Object Storage
   - 10 GB Archive Storage

2. **Enable Auto-scaling**:
   ```hcl
   # In terraform.tfvars
   node_pool_min_size = 1
   node_pool_max_size = 5
   ```

3. **Use Spot Instances** for non-critical workloads

4. **Set Resource Limits** in Kubernetes deployments

## Next Steps

1. Set up CI/CD pipeline (see [CI_CD_SETUP.md](CI_CD_SETUP.md))
2. Configure monitoring and alerts
3. Set up backup and disaster recovery
4. Implement security best practices
5. Configure custom domain with SSL

## Additional Resources

- [OCI Documentation](https://docs.oracle.com/iaas/)
- [OKE Best Practices](https://docs.oracle.com/en-us/iaas/Content/ContEng/Concepts/contengbestpractices.htm)
- [Auth0 OCI Integration](https://auth0.com/docs/deploy/deploy-cli-tool)
- [Terraform OCI Provider](https://registry.terraform.io/providers/oracle/oci/latest/docs)
