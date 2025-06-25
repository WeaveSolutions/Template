# DigitalOcean Infrastructure Provider

This module provides infrastructure provisioning for DigitalOcean cloud platform using Terraform.

## Overview

DigitalOcean is a cloud infrastructure provider that offers virtual machines (Droplets), managed databases, Kubernetes, App Platform, and various other cloud services designed for developers.

## Features

- **Droplets**: Virtual machines with SSD storage
- **App Platform**: Platform-as-a-Service for deploying applications
- **Managed Databases**: PostgreSQL, MySQL, Redis, MongoDB
- **Kubernetes**: Managed Kubernetes clusters
- **Spaces**: Object storage compatible with S3
- **Load Balancers**: Distribute traffic across multiple Droplets
- **Networking**: VPC, Floating IPs, Firewalls

## Directory Structure

```
digitalocean/
├── environments/
│   ├── development/
│   │   ├── main.tf
│   │   ├── variables.tf
│   │   ├── outputs.tf
│   │   └── terraform.tfvars.example
│   ├── staging/
│   │   ├── main.tf
│   │   ├── variables.tf
│   │   ├── outputs.tf
│   │   └── terraform.tfvars.example
│   └── production/
│       ├── main.tf
│       ├── variables.tf
│       ├── outputs.tf
│       └── terraform.tfvars.example
├── modules/
│   ├── app-platform/
│   ├── compute/
│   ├── database/
│   ├── kubernetes/
│   ├── networking/
│   └── storage/
├── variables.tf
├── outputs.tf
└── README.md
```

## Environment Configuration

Each environment (development, staging, production) includes:

- **App Platform**: Deploy web applications and APIs
- **Droplets**: Virtual machines for custom applications
- **Managed Databases**: PostgreSQL for application data
- **Spaces**: Object storage for static assets
- **VPC**: Isolated network for security

## Quick Start

1. **Configure Environment Variables**:
   ```bash
   export DIGITALOCEAN_ACCESS_TOKEN="your-access-token"
   export DIGITALOCEAN_REGION="nyc3"
   ```

2. **Initialize Terraform**:
   ```bash
   cd terraform/providers/digitalocean/environments/development
   terraform init
   ```

3. **Plan Infrastructure**:
   ```bash
   terraform plan -var-file="terraform.tfvars"
   ```

4. **Apply Configuration**:
   ```bash
   terraform apply
   ```

## Environment Variables

### Required
- `DIGITALOCEAN_ACCESS_TOKEN`: DigitalOcean API access token
- `DIGITALOCEAN_REGION`: Default deployment region (e.g., nyc3, sfo3, ams3)

### Optional
- `DIGITALOCEAN_VPC_ID`: Existing VPC ID (if not creating new)
- `DIGITALOCEAN_PROJECT_ID`: Project ID for resource organization
- `DIGITALOCEAN_DROPLET_SIZE`: Default Droplet size (s-1vcpu-1gb)

## Supported Regions

- **North America**: nyc1, nyc3, sfo2, sfo3, tor1
- **Europe**: ams2, ams3, fra1, lon1
- **Asia**: sgp1, blr1
- **Others**: Refer to DigitalOcean documentation for latest regions

## Security Considerations

- Use VPC for network isolation
- Configure firewalls for Droplet access control
- Enable monitoring and alerting
- Use managed databases for automatic backups
- Implement proper IAM through projects and teams

## Cost Management

- **Droplets**: Hourly billing with monthly caps
- **App Platform**: Pay per container/hour
- **Databases**: Fixed monthly pricing
- **Spaces**: Storage + bandwidth charges
- **Load Balancers**: Fixed monthly pricing

## Monitoring and Logging

DigitalOcean provides built-in monitoring for:
- Droplet metrics (CPU, memory, disk, network)
- App Platform application logs
- Database performance metrics
- Load balancer health checks

## Best Practices

1. **Use App Platform** for stateless applications
2. **Implement proper tagging** for resource organization
3. **Use VPC** for network security
4. **Enable monitoring** for all resources
5. **Regular snapshots** for Droplets
6. **Use managed databases** instead of self-hosted

## Support

- [DigitalOcean Documentation](https://docs.digitalocean.com/)
- [Terraform DigitalOcean Provider](https://registry.terraform.io/providers/digitalocean/digitalocean/latest/docs)
- [Community Support](https://www.digitalocean.com/community)
