# HashiCorp Consul Deployment with Terraform - Nexpo Edition

This guide provides comprehensive Terraform instructions for deploying HashiCorp Consul as the service discovery and configuration backbone for the Nexpo microservices ecosystem.

## Table of Contents

1. [Architecture Overview](#architecture-overview)
2. [Prerequisites](#prerequisites)
3. [Terraform Configuration](#terraform-configuration)
4. [Deployment Steps](#deployment-steps)
5. [Service Registration](#service-registration)
6. [Kong Gateway Integration](#kong-gateway-integration)

## Architecture Overview

Consul serves as the central nervous system for Nexpo's distributed architecture:

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   Kong Gateway  │───▶│   Consul Cluster │◀───│  Polyglot APIs  │
│   (Port 8000)   │    │   (Port 8500)    │    │  (Ports 8020+)  │
└─────────────────┘    └─────────────────┘    └─────────────────┘
```

## Prerequisites

### Required Tools

```bash
# Install Terraform CLI
choco install terraform  # Windows
brew install terraform   # macOS
apt-get install terraform # Ubuntu

# Install Consul CLI
choco install consul      # Windows
brew install consul       # macOS
apt-get install consul    # Ubuntu

# Verify installations
terraform --version
consul --version
```

### Environment Variables

Add to your `.env` file:

```bash
# Consul Configuration
ENABLE_CONSUL=true
CONSUL_HTTP_ADDR=localhost:8500
CONSUL_DATACENTER=nexpo-dc1
CONSUL_ENCRYPT_KEY=your-32-char-base64-encryption-key
CONSUL_TOKEN=your-consul-acl-token

# Integration Settings
CONSUL_KONG_INTEGRATION=true
CONSUL_NOMAD_INTEGRATION=true
CONSUL_VAULT_INTEGRATION=true
```

## Terraform Configuration

### 1. Provider Configuration

Create `terraform/consul/providers.tf`:

```hcl
terraform {
  required_providers {
    consul = {
      source  = "hashicorp/consul"
      version = "~> 2.18"
    }
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
  }
  required_version = ">= 1.0"
}

provider "consul" {
  address    = var.consul_address
  datacenter = var.consul_datacenter
  token      = var.consul_token
}

provider "aws" {
  region = var.aws_region
  
  default_tags {
    tags = {
      Project     = "Nexpo"
      Environment = var.environment
      ManagedBy   = "Terraform"
      Component   = "Consul"
    }
  }
}
```

### 2. Variables Configuration

Create `terraform/consul/variables.tf`:

```hcl
variable "environment" {
  description = "Deployment environment (dev/staging/prod)"
  type        = string
  default     = "dev"
  validation {
    condition     = contains(["dev", "staging", "prod"], var.environment)
    error_message = "Environment must be dev, staging, or prod."
  }
}

variable "consul_address" {
  description = "Consul server address"
  type        = string
  default     = "localhost:8500"
}

variable "consul_datacenter" {
  description = "Consul datacenter name"
  type        = string
  default     = "nexpo-dc1"
}

variable "consul_token" {
  description = "Consul ACL token"
  type        = string
  sensitive   = true
}

# Nexpo Services Configuration
variable "nexpo_services" {
  description = "Nexpo microservices configuration"
  type = map(object({
    port        = number
    path        = string
    health_path = string
    tags        = list(string)
  }))
  default = {
    "kong-gateway" = {
      port        = 8000
      path        = "/"
      health_path = "/status"
      tags        = ["api-gateway", "proxy", "nexpo"]
    }
    "api-typescript" = {
      port        = 8020
      path        = "/api/v1/typescript"
      health_path = "/health"
      tags        = ["api", "typescript", "express", "nexpo"]
    }
    "api-python" = {
      port        = 8030
      path        = "/api/v1/python"
      health_path = "/health"
      tags        = ["api", "python", "fastapi", "nexpo"]
    }
    "mindsdb" = {
      port        = 4040
      path        = "/api"
      health_path = "/api/status"
      tags        = ["database", "ai", "ml", "nexpo"]
    }
  }
}
```

### 3. Main Configuration

Create `terraform/consul/main.tf`:

```hcl
# Kong Gateway Service Registration
resource "consul_service" "kong_gateway" {
  name    = "kong-gateway"
  node    = "nexpo-node-${var.environment}"
  port    = 8000
  tags    = ["api-gateway", "proxy", "nexpo", var.environment]
  address = "localhost"
  
  check {
    http     = "http://localhost:8000/status"
    interval = "10s"
    timeout  = "3s"
  }
  
  meta = {
    version     = "latest"
    environment = var.environment
    component   = "api-gateway"
    project     = "nexpo"
  }
}

# Polyglot API Services Registration
resource "consul_service" "nexpo_apis" {
  for_each = {
    for name, config in var.nexpo_services :
    name => config
    if name != "kong-gateway"
  }
  
  name    = each.key
  node    = "nexpo-node-${var.environment}"
  port    = each.value.port
  tags    = concat(each.value.tags, [var.environment])
  address = "localhost"
  
  check {
    http     = "http://localhost:${each.value.port}${each.value.health_path}"
    interval = "10s"
    timeout  = "5s"
  }
  
  meta = {
    version     = "latest"
    environment = var.environment
    path        = each.value.path
    project     = "nexpo"
  }
}

# Key-Value Store for Configuration
resource "consul_keys" "nexpo_config" {
  datacenter = var.consul_datacenter
  
  # Global configuration
  key {
    path  = "nexpo/config/environment"
    value = var.environment
  }
  
  key {
    path  = "nexpo/config/auth0/domain"
    value = "nexpo-${var.environment}.auth0.com"
  }
  
  key {
    path  = "nexpo/config/kong/admin_url"
    value = "http://localhost:8001"
  }
  
  key {
    path  = "nexpo/config/mindsdb/url"
    value = "http://localhost:4040"
  }
  
  # Feature flags
  key {
    path  = "nexpo/features/auth0_enabled"
    value = "true"
  }
  
  key {
    path  = "nexpo/features/kong_enabled"
    value = "true"
  }
  
  key {
    path  = "nexpo/features/mindsdb_enabled"
    value = "true"
  }
}
```

## Deployment Steps

### 1. Initialize Terraform

```bash
cd terraform/consul
terraform init
```

### 2. Plan Deployment

```bash
# Set sensitive variables
export TF_VAR_consul_token="your-consul-acl-token"

# Plan the deployment
terraform plan -var="environment=dev"
```

### 3. Deploy Infrastructure

```bash
# Apply the configuration
terraform apply -var="environment=dev" -auto-approve
```

### 4. Verify Deployment

```bash
# Check Consul cluster status
consul members

# List registered services
consul catalog services

# Check specific service health
consul health service kong-gateway
consul health service api-typescript
```

## Service Registration

### Manual Service Registration

```bash
# Register a new API service
consul services register -name="api-custom" \
  -port=9000 \
  -address="localhost" \
  -tag="api" \
  -tag="custom" \
  -tag="nexpo" \
  -check-http="http://localhost:9000/health" \
  -check-interval="10s"
```

## Kong Gateway Integration

### Kong Service Discovery Configuration

```bash
# Configure Kong to use Consul
curl -i -X POST http://localhost:8001/services/ \
  --data "name=api-typescript" \
  --data "url=http://consul-dns:8020" \
  --data "tags=nexpo,typescript"

# Create route
curl -i -X POST http://localhost:8001/services/api-typescript/routes \
  --data "paths[]=/api/v1/typescript" \
  --data "name=typescript-route"
```

### Kong Consul Sync Script

Create `scripts/kong-consul-sync.sh`:

```bash
#!/bin/bash

# Sync Nexpo services from Consul to Kong
SERVICES=("api-typescript" "api-python" "api-go" "api-rust" "mindsdb")

for service in "${SERVICES[@]}"; do
  echo "Syncing $service..."
  
  # Get service details from Consul
  SERVICE_INFO=$(consul catalog service $service -format=json)
  PORT=$(echo $SERVICE_INFO | jq -r '.[0].ServicePort')
  
  # Register with Kong
  curl -s -X POST http://localhost:8001/services/ \
    --data "name=$service" \
    --data "url=http://localhost:$PORT" \
    --data "tags=nexpo,consul" || true
done

echo "Kong-Consul sync completed"
```

---

This Terraform configuration provides a robust foundation for integrating HashiCorp Consul with the Nexpo ecosystem, enabling service discovery, health monitoring, and configuration management for all microservices.