# IBM Cloud Quick Start Guide

This guide provides a fast path to deploying your infrastructure on IBM Cloud using our Terraform modules. Follow these steps to quickly get your environment up and running.

## Prerequisites

Before you begin, ensure you have:

1. **IBM Cloud Account**: Sign up at [cloud.ibm.com](https://cloud.ibm.com/registration)
2. **IBM Cloud CLI**: Install from [IBM Cloud CLI](https://cloud.ibm.com/docs/cli?topic=cli-install-ibmcloud-cli)
3. **Terraform**: Install version 1.0.0+ from [terraform.io](https://www.terraform.io/downloads.html)
4. **Git**: For cloning the repository

## Step 1: Set Up Your Environment

### Clone the Repository

```bash
git clone https://github.com/your-organization/Nexpo.git
cd Nexpo
```

### Create IBM Cloud API Key

1. Log in to IBM Cloud Console
2. Navigate to Manage → Access (IAM) → API Keys
3. Click "Create an IBM Cloud API key"
4. Name your key (e.g., "terraform-deployment")
5. Copy the API key (you won't be able to see it again)

### Configure Environment Variables

Create a `.env` file in the root directory based on the provided `.env.example`:

```bash
# Copy the example file
cp .env.example .env

# Edit the file with your IBM Cloud credentials
# Set ENABLE_IBM=true
# Add your IBM Cloud API key, region, etc.
```

## Step 2: Configure Your Infrastructure

### Update terraform.tfvars

Create a file named `ibm.tfvars` in the `terraform` directory:

```hcl
# IBM Cloud Configuration
enable_ibm = true
ibmcloud_api_key = "your-ibm-cloud-api-key"
ibm_region = "us-south"
ibm_zone = "us-south-1"

# VPC Network Configuration
vpc_name = "my-vpc"
vpc_subnets = [
  {
    name = "subnet-1"
    cidr = "10.10.10.0/24"
    zone = "us-south-1"
    public_gateway = true
  }
]

# Optional configurations based on your needs:
# Uncomment and modify as required

# Kubernetes cluster configuration
# enable_kubernetes = true
# kubernetes_cluster_name = "my-cluster"
# kubernetes_worker_count = 2
# kubernetes_machine_type = "bx2-4x16"

# Database configuration
# enable_databases = true
# db_instances = [
#   {
#     name = "postgres-db"
#     type = "databases-for-postgresql"
#     plan = "standard"
#   }
# ]

# Power VS configuration (for AIX/IBM i workloads)
# enable_power_vs = true
# power_vs_zone = "dal12"
# power_vs_instances = [...]
```

## Step 3: Initialize and Deploy

### Initialize Terraform

```bash
cd terraform
terraform init
```

### Plan Your Deployment

```bash
terraform plan -var-file=ibm.tfvars
```

Review the plan to ensure it matches your expectations.

### Deploy Your Infrastructure

```bash
terraform apply -var-file=ibm.tfvars
```

Type `yes` when prompted to confirm the deployment.

## Step 4: Access Your Resources

### VPC and Compute Resources

Access your virtual server instances via SSH:

```bash
# Use the output from terraform apply to get the IP address
ssh -i ~/.ssh/your-key.pem root@<instance_ip>
```

### Kubernetes Cluster

Configure kubectl to access your IKS cluster:

```bash
# Get credentials for your Kubernetes cluster
ibmcloud ks cluster config --cluster <cluster_name>

# Verify connection
kubectl get nodes
```

### Databases

Access your database instances:

```bash
# Get connection string from IBM Cloud console
# Or from terraform output
ibmcloud resource service-instance <database_name>
```

## Step 5: Multi-Cloud Integration

If you're deploying across multiple cloud providers:

1. Enable the desired providers in your `.env` file:
   ```
   ENABLE_AWS=true
   ENABLE_GCP=false
   ENABLE_AZURE=false
   ENABLE_OCI=false
   ENABLE_IBM=true
   ENABLE_CLOUDFLARE=false
   ```

2. Create provider-specific `.tfvars` files:
   - `aws.tfvars`
   - `ibm.tfvars`
   - etc.

3. Deploy using all enabled providers:
   ```bash
   terraform apply \
     -var-file=aws.tfvars \
     -var-file=ibm.tfvars
   ```

## Step 6: MindsDB Integration

To use MindsDB with IBM Cloud:

1. Configure MindsDB to access IBM Cloud resources:
   ```bash
   # Set up MindsDB connection to IBM Cloud resources
   make -C mindsdb-main up
   ```

2. Access the MindsDB dashboard:
   ```
   http://localhost:47334
   ```

3. Configure IBM Cloud as a data source in MindsDB:
   - Use the Cloudant NoSQL database connector
   - Use Cloud Object Storage for file-based datasets

## Common Configurations

### Networking

```hcl
# Basic VPC with subnets
vpc_name = "application-vpc"
vpc_subnets = [
  {
    name = "web-subnet"
    cidr = "10.10.10.0/24"
    zone = "us-south-1"
    public_gateway = true
  },
  {
    name = "app-subnet"
    cidr = "10.10.20.0/24"
    zone = "us-south-2"
    public_gateway = false
  },
  {
    name = "db-subnet"
    cidr = "10.10.30.0/24"
    zone = "us-south-3"
    public_gateway = false
  }
]
```

### Compute

```hcl
# Virtual server instances
instances = [
  {
    name     = "web-server"
    profile  = "cx2-2x4"
    image    = "ibm-ubuntu-20-04-minimal-amd64-2"
    zone     = "us-south-1"
    subnet_name = "web-subnet"
  },
  {
    name     = "app-server"
    profile  = "cx2-4x8"
    image    = "ibm-ubuntu-20-04-minimal-amd64-2"
    zone     = "us-south-2"
    subnet_name = "app-subnet"
  }
]
```

### Kubernetes

```hcl
# Kubernetes cluster
enable_kubernetes = true
kubernetes_cluster_name = "application-cluster"
kubernetes_worker_count = 3
kubernetes_machine_type = "bx2-4x16"
kubernetes_version = "1.26"
```

### Storage

```hcl
# Cloud Object Storage
cos_instances = [
  {
    name = "app-storage"
    plan = "standard"
    buckets = [
      {
        name = "app-assets"
        storage_class = "standard"
      },
      {
        name = "app-backups"
        storage_class = "standard"
      }
    ]
  }
]
```

## Troubleshooting

### Common Issues

1. **API Key Permissions**
   - Ensure your API key has adequate permissions
   - Try creating a new API key with Administrator role

2. **Resource Quota Limits**
   - Check if you've reached service limits
   - Request quota increases if needed

3. **Terraform Provider Version**
   - Ensure you're using the correct IBM Cloud provider version
   - Run `terraform init -upgrade` to update providers

### Getting Help

- IBM Cloud Support: https://cloud.ibm.com/unifiedsupport/supportcenter
- IBM Cloud Documentation: https://cloud.ibm.com/docs
- Community Forums: https://community.ibm.com/community/user/cloud/home
- Stack Overflow: https://stackoverflow.com/questions/tagged/ibm-cloud

## Next Steps

After your initial deployment:

1. **Set Up Monitoring**: Configure IBM Cloud Monitoring
2. **Implement Logging**: Set up IBM Cloud Activity Tracker
3. **Configure Backup**: Implement automated backups
4. **Security Hardening**: Review security best practices
5. **CI/CD Setup**: Automate deployments (see CI_CD_SETUP.md)
6. **Cost Optimization**: Review and optimize resources

Refer to our [DEPLOYMENT_CHECKLIST.md](./DEPLOYMENT_CHECKLIST.md) for a comprehensive guide to production-ready deployments.
