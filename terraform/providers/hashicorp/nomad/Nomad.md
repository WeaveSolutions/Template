# HashiCorp Nomad Infrastructure Provider

This module provides infrastructure provisioning for HashiCorp Nomad using Terraform.

## Overview

HashiCorp Nomad is a flexible workload orchestrator that enables an organization to deploy and manage applications across on-premises and cloud infrastructure at scale.

## Features

- **Job Scheduling**: Deploy and manage containerized and non-containerized applications
- **Multi-Region Support**: Manage workloads across multiple regions and data centers
- **Service Discovery**: Integration with Consul for service discovery
- **Load Balancing**: Built-in load balancing and health checks
- **Resource Management**: Efficient resource allocation and constraint-based scheduling
- **Rolling Updates**: Zero-downtime deployments with rolling updates

## Directory Structure

```
nomad/
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
├── jobs/
│   ├── nextjs-app.nomad
│   ├── api-service.nomad
│   └── database-backup.nomad
├── variables.tf
├── outputs.tf
└── README.md
```

## Environment Configuration

Each environment includes:

- **Next.js Application**: Frontend web application deployment
- **API Services**: Backend microservices deployment
- **Background Jobs**: Scheduled tasks and data processing
- **Service Mesh**: Integration with Consul Connect

## Quick Start

1. **Configure Environment Variables**:
   ```bash
   export NOMAD_ADDR="https://nomad.example.com:4646"
   export NOMAD_TOKEN="your-nomad-token"
   export NOMAD_REGION="global"
   ```

2. **Initialize Terraform**:
   ```bash
   cd terraform/providers/hashicorp/nomad/environments/development
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
- `NOMAD_ADDR`: Nomad server address (e.g., https://nomad.example.com:4646)
- `NOMAD_TOKEN`: Nomad authentication token

### Optional
- `NOMAD_REGION`: Nomad region (default: global)
- `NOMAD_NAMESPACE`: Nomad namespace (default: default)
- `NOMAD_CACERT`: Path to CA certificate for TLS verification
- `NOMAD_CAPATH`: Path to directory of CA certificates

## Job Specifications

### Next.js Application
```hcl
job "nextjs-app" {
  region = "global"
  datacenters = ["dc1"]
  
  group "web" {
    count = 3
    
    network {
      port "http" {
        static = 3000
      }
    }
    
    task "nextjs" {
      driver = "docker"
      
      config {
        image = "node:18-alpine"
        ports = ["http"]
      }
      
      resources {
        cpu    = 500
        memory = 512
      }
    }
  }
}
```

### API Service
```hcl
job "api-service" {
  region = "global"
  datacenters = ["dc1"]
  
  group "api" {
    count = 2
    
    network {
      port "http" {
        to = 8000
      }
    }
    
    task "api" {
      driver = "docker"
      
      config {
        image = "your-api-image:latest"
        ports = ["http"]
      }
    }
  }
}
```

## Networking

Nomad provides several networking options:

- **Bridge Networks**: Container-to-container communication
- **Host Networks**: Direct access to host networking
- **CNI Plugins**: Advanced networking with third-party plugins
- **Service Mesh**: Integration with Consul Connect

## Security

- **ACLs**: Role-based access control
- **TLS**: Encrypted communication between components
- **Policies**: Fine-grained resource access control
- **Vault Integration**: Secret management with HashiCorp Vault

## Monitoring and Logging

- **Telemetry**: Built-in metrics collection
- **Prometheus Integration**: Metrics scraping and alerting
- **Log Aggregation**: Centralized logging with external systems
- **Health Checks**: Application and service health monitoring

## Best Practices

1. **Use Job Templates**: Create reusable job specifications
2. **Resource Constraints**: Set appropriate CPU and memory limits
3. **Health Checks**: Implement comprehensive health checks
4. **Rolling Updates**: Use update strategies for zero-downtime deployments
5. **Service Discovery**: Leverage Consul for service registration
6. **Secrets Management**: Use Vault for sensitive data

## Integration with Other HashiCorp Tools

### Consul
- Service discovery and configuration
- Health checking and monitoring
- Key-value store for configuration

### Vault
- Secret management and encryption
- Dynamic credentials for databases
- Certificate management

## Support

- [Nomad Documentation](https://www.nomadproject.io/docs)
- [Terraform Nomad Provider](https://registry.terraform.io/providers/hashicorp/nomad/latest/docs)
- [HashiCorp Learn](https://learn.hashicorp.com/nomad)
