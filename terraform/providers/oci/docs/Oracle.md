# Oracle Cloud Infrastructure (OCI) Provider

This directory contains Terraform modules and configurations for deploying the Nexpo application on Oracle Cloud Infrastructure.

## Features

- **Container Engine for Kubernetes (OKE)**: Managed Kubernetes cluster
- **Oracle Container Registry**: Private container registry
- **Oracle Functions**: Serverless compute platform
- **Autonomous Database**: Self-managing database
- **Object Storage**: Scalable object storage
- **Load Balancer**: Application and network load balancing
- **Virtual Cloud Network (VCN)**: Isolated network environment
- **Identity and Access Management**: Fine-grained access control
- **OCI Monitoring**: Comprehensive monitoring and alerting

## Directory Structure

```
oci/
├── modules/
│   ├── networking/     # VCN, subnets, security lists, route tables
│   ├── compute/        # OKE, Container Instances, VM instances
│   ├── storage/        # Object Storage, Block Volumes, File Storage
│   ├── database/       # Autonomous Database, MySQL HeatWave
│   ├── functions/      # Oracle Functions serverless
│   ├── registry/       # Oracle Container Registry
│   ├── load-balancer/  # Load Balancer configurations
│   ├── monitoring/     # OCI Monitoring, Logging, Events
│   └── security/       # IAM policies, encryption, secrets
├── environments/
│   ├── development/   # Development environment
│   ├── staging/       # Staging environment
│   └── production/    # Production environment
├── docs/
│   ├── QUICK_START.md
│   ├── DEPLOYMENT_CHECKLIST.md
│   └── CI_CD_SETUP.md
├── scripts/
│   ├── setup-oci-cli.sh
│   └── deploy.sh
├── terraform.tfvars.example
├── versions.tf
└── README.md
```

## Prerequisites

1. **OCI Account**: Sign up at [cloud.oracle.com](https://cloud.oracle.com)
2. **OCI CLI**: Install from [OCI CLI Documentation](https://docs.oracle.com/en-us/iaas/Content/API/SDKDocs/cliinstall.htm)
3. **Terraform**: Version 1.0 or higher
4. **kubectl**: For Kubernetes management
5. **Docker**: For container operations

## Quick Start

See [docs/QUICK_START.md](docs/QUICK_START.md) for detailed setup instructions.

### 1. Configure OCI CLI

```bash
oci setup config
```

### 2. Set Environment Variables

```bash
export TF_VAR_tenancy_ocid="ocid1.tenancy.oc1..xxxxx"
export TF_VAR_user_ocid="ocid1.user.oc1..xxxxx"
export TF_VAR_fingerprint="xx:xx:xx:xx:xx:xx:xx:xx"
export TF_VAR_private_key_path="~/.oci/oci_api_key.pem"
export TF_VAR_region="us-phoenix-1"
```

### 3. Deploy Infrastructure

```bash
cd environments/development
terraform init
terraform plan
terraform apply
```

## Module Documentation

### Networking Module
- Creates VCN with public/private subnets
- Configures security lists and route tables
- Sets up NAT and Internet gateways
- Implements network segmentation

### Compute Module
- Deploys Oracle Container Engine for Kubernetes (OKE)
- Configures node pools with auto-scaling
- Sets up Container Instances for microservices
- Integrates with Auth0 for authentication

### Storage Module
- Creates Object Storage buckets
- Configures lifecycle policies
- Sets up cross-region replication
- Implements encryption at rest

### Database Module
- Provisions Autonomous Database
- Configures automatic backups
- Sets up private endpoints
- Implements data encryption

### Functions Module
- Deploys serverless functions
- Configures API Gateway integration
- Sets up event triggers
- Implements function versioning

### Registry Module
- Creates private container registry
- Configures retention policies
- Sets up vulnerability scanning
- Implements access policies

### Load Balancer Module
- Creates flexible load balancer
- Configures SSL termination
- Sets up health checks
- Implements path-based routing

### Monitoring Module
- Sets up OCI Monitoring
- Configures custom metrics
- Creates alarms and notifications
- Implements log aggregation

### Security Module
- Creates IAM policies
- Configures compartments
- Sets up dynamic groups
- Implements least privilege access

## Cost Optimization

OCI offers several cost-saving features:

1. **Always Free Tier**: Includes compute, storage, and database resources
2. **Flexible Compute**: Pay only for what you use
3. **Reserved Capacity**: Save up to 50% with commitments
4. **Auto-scaling**: Scale resources based on demand
5. **Cost Analysis**: Built-in cost tracking and budgets

## Security Best Practices

1. **Use Compartments**: Isolate resources by environment
2. **Enable Encryption**: Use OCI Vault for key management
3. **Network Security**: Implement security lists and NSGs
4. **Access Control**: Use IAM policies and MFA
5. **Audit Logging**: Enable OCI Audit for compliance

## Integration with Auth0

The OCI deployment integrates seamlessly with Auth0:

1. **Environment Variables**: Auth0 credentials stored in OCI Vault
2. **API Gateway**: Validates Auth0 JWTs
3. **Functions**: Exchange Auth0 tokens for custom tokens
4. **Kubernetes**: Auth0 integration via environment variables

## Monitoring and Observability

- **OCI Monitoring**: Infrastructure and application metrics
- **Logging Service**: Centralized log management
- **Events Service**: Real-time event processing
- **Application Performance Monitoring**: End-to-end tracing

## Support

- [OCI Documentation](https://docs.oracle.com/en-us/iaas/Content/home.htm)
- [OCI Support](https://support.oracle.com/)
- [OCI Community](https://community.oracle.com/hub/cloud-infrastructure)

## License

This configuration is part of the Nexpo monorepo project.
