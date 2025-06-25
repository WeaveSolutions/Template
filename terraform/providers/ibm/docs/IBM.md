# IBM Cloud Provider for Multi-Cloud Infrastructure

This module provides comprehensive IBM Cloud infrastructure management for your multi-cloud deployment. It integrates seamlessly with the existing AWS, GCP, Azure, OCI, and Cloudflare providers in your Terraform configuration.

## Features

- **VPC Networking**: Subnets, security groups, ACLs, and gateways
- **Compute Options**: Virtual servers (VPC), Bare Metal, and dedicated hosts
- **Kubernetes Service**: Managed Kubernetes with worker pools and cluster addons
- **Storage Solutions**: Cloud Object Storage, Block Storage, and File Storage
- **Database Services**: Managed PostgreSQL, MySQL, MongoDB, Redis, and Cloudant NoSQL
- **Power Virtual Servers**: AIX and IBM i workloads on IBM Power architecture
- **Security Services**: Key Protect, Certificate Manager, and Security Groups
- **Schematics**: IBM Cloud's Terraform-as-a-Service offering

## Prerequisites

- IBM Cloud account with proper permissions
- IBM Cloud API key with sufficient access
- Terraform 1.0.0+
- IBM Cloud CLI (recommended for troubleshooting)

## Quick Start

1. Copy `terraform.tfvars.example` to `terraform.tfvars` and add your IBM Cloud credentials:

```bash
cp terraform.tfvars.example terraform.tfvars
```

2. Update the `terraform.tfvars` file with your IBM Cloud API key and desired configuration.

3. Enable the IBM Cloud provider in your main Terraform configuration:

```hcl
# In the root terraform.tfvars file
enable_ibm = true
```

4. Initialize and apply your Terraform configuration:

```bash
terraform init
terraform plan
terraform apply
```

## Module Structure

```
ibm/
├── modules/
│   ├── networking/       # VPC, subnets, security groups
│   ├── compute/          # Virtual server instances
│   ├── kubernetes/       # IBM Kubernetes Service (IKS)
│   ├── storage/          # Object, block, and file storage
│   ├── database/         # Managed database instances
│   ├── cloudant/         # Cloudant NoSQL database
│   ├── power_vs/         # Power Virtual Servers
│   ├── security/         # Key management and certificates
│   └── schematics/       # IBM's Terraform-as-a-Service
├── environments/         # Environment-specific configurations
│   ├── development/      # Development environment
│   ├── staging/          # Staging environment
│   └── production/       # Production environment
├── docs/                 # Additional documentation
├── scripts/              # Helper scripts
├── main.tf               # Main provider configuration
├── variables.tf          # Input variables
├── outputs.tf            # Output values
└── terraform.tfvars.example  # Example variable values
```

## Networking

The networking module configures your IBM Cloud VPC infrastructure:

```hcl
module "networking" {
  source = "./modules/networking"
  
  vpc_name = "my-vpc"
  subnets = [
    {
      name = "subnet-1"
      cidr = "10.10.10.0/24"
      zone = "us-south-1"
    }
  ]
  security_groups = [
    {
      name = "web-sg"
      rules = [
        {
          name      = "allow-http"
          direction = "inbound"
          remote    = "0.0.0.0/0"
          tcp = {
            port_min = 80
            port_max = 80
          }
        }
      ]
    }
  ]
}
```

## Compute

The compute module provisions virtual server instances in your VPC:

```hcl
module "compute" {
  source = "./modules/compute"
  
  instances = [
    {
      name     = "web-server"
      profile  = "cx2-2x4"
      image    = "ibm-ubuntu-20-04-minimal-amd64-2"
      zone     = "us-south-1"
      subnet_id = module.networking.subnet_ids["subnet-1"]
    }
  ]
}
```

## Kubernetes Service

Deploy managed Kubernetes clusters:

```hcl
module "kubernetes" {
  source = "./modules/kubernetes"
  
  cluster_name      = "my-cluster"
  worker_count      = 3
  machine_type      = "bx2-4x16"
  kube_version      = "1.26"
  resource_group_id = ibm_resource_group.group.id
  vpc_id            = module.networking.vpc_id
  subnet_ids        = module.networking.subnet_ids
}
```

## Storage

Manage various storage solutions:

```hcl
module "storage" {
  source = "./modules/storage"
  
  cos_instances = [
    {
      name = "app-storage"
      plan = "standard"
      buckets = [
        {
          name = "app-assets"
          storage_class = "standard"
        }
      ]
    }
  ]
  
  block_volumes = [
    {
      name = "data-volume"
      profile = "general-purpose"
      capacity = 100
      zone = "us-south-1"
    }
  ]
}
```

## Database Services

Create managed database instances:

```hcl
module "database" {
  source = "./modules/database"
  
  db_instances = [
    {
      name = "postgres-db"
      type = "databases-for-postgresql"
      plan = "standard"
      version = "12"
    }
  ]
}
```

## Cloudant NoSQL Database

Deploy Cloudant NoSQL databases:

```hcl
module "cloudant" {
  source = "./modules/cloudant"
  
  cloudant_instances = [
    {
      name = "app-nosql-db"
      plan = "standard"
      databases = [
        {
          name = "users"
          partitioned = false
        }
      ]
    }
  ]
}
```

## Power Virtual Servers

Deploy AIX or IBM i workloads on IBM Power architecture:

```hcl
module "power_vs" {
  source = "./modules/power_vs"
  
  power_vs_zone = "dal12"
  power_vs_instances = [
    {
      name = "power-instance"
      image_name = "IBMi-74-05-POWER9-virtual-server"
      processors = 0.5
      memory = 8
      proc_type = "shared"
      sys_type = "s922"
      network_ids = [module.power_vs.network_ids["power-network"]]
      ssh_key_name = "power-ssh-key"
    }
  ]
  
  power_vs_networks = [
    {
      name = "power-network"
      cidr = "192.168.0.0/24"
    }
  ]
}
```

## Security

Manage encryption keys and certificates:

```hcl
module "security" {
  source = "./modules/security"
  
  kms_instances = [
    {
      name = "app-key-protect"
      plan = "tiered-pricing"
      keys = [
        {
          name = "app-encryption-key"
          standard_key = false
          rotation = 3
        }
      ]
    }
  ]
  
  certificates = [
    {
      name = "app-cert"
      domains = ["example.com", "*.example.com"]
      auto_renew = true
    }
  ]
}
```

## Integration with Multi-Cloud Setup

This IBM Cloud provider integrates with your existing multi-cloud setup:

1. In your main `terraform/main.tf`:

```hcl
variable "enable_ibm" {
  description = "Enable IBM Cloud provider"
  type        = bool
  default     = false
}

module "ibm" {
  count  = var.enable_ibm ? 1 : 0
  source = "./providers/ibm"
  
  # Pass through common variables
  project_name = var.project_name
  environment  = var.environment
  ibmcloud_api_key = var.ibmcloud_api_key
}
```

2. In `terraform/variables.tf`:

```hcl
variable "ibmcloud_api_key" {
  description = "IBM Cloud API key"
  type        = string
  default     = ""
  sensitive   = true
}
```

3. In `terraform/terraform.tfvars.example`:

```hcl
# IBM Cloud Configuration
enable_ibm        = false
ibmcloud_api_key  = ""
```

## Best Practices

- **Resource Groups**: Use resource groups to organize resources by environment
- **Naming Convention**: Adopt a consistent naming convention for all resources
- **Tagging**: Apply tags to resources for better governance and cost tracking
- **Security**: Use Key Protect for encryption and manage access with IAM
- **Backup**: Configure automatic backups for databases and important volumes
- **Monitoring**: Enable monitoring and logging for all production resources

## Troubleshooting

- **API Rate Limiting**: IBM Cloud may rate limit API calls; use `terraform apply -parallelism=10`
- **Resource Quotas**: Ensure your account has sufficient quota for requested resources
- **VPC Limitations**: Be aware of VPC service limits (security groups, rules, etc.)
- **Key Rotation**: When rotating API keys, update Terraform state with the new key

## IBM Cloud Specific Features

### Power VS

The Power VS module allows you to run AIX and IBM i workloads on IBM Power architecture, which is unique to IBM Cloud. This is ideal for migrating legacy workloads to the cloud while maintaining compatibility.

### Schematics

IBM Cloud Schematics is a Terraform-as-a-Service offering that allows you to use Terraform without installing it locally. This module can also manage Schematics workspaces.

### Cloud Satellite

IBM Cloud Satellite allows you to deploy IBM Cloud services to any environment - on-premises, edge, or other clouds. This can be configured using the appropriate modules.

## Resources

- [IBM Cloud Terraform Provider Documentation](https://registry.terraform.io/providers/IBM-Cloud/ibm/latest/docs)
- [IBM Cloud Documentation](https://cloud.ibm.com/docs)
- [IBM Cloud CLI](https://cloud.ibm.com/docs/cli)
