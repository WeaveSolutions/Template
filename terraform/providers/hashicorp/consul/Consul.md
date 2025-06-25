# HashiCorp Consul Infrastructure Provider

This module provides service discovery, configuration, and service mesh capabilities using HashiCorp Consul.

## Overview

HashiCorp Consul is a multi-cloud service networking platform that provides service discovery, service mesh, traffic management, and automated updates to network infrastructure device configuration.

## Features

- **Service Discovery**: Automatic service registration and health checking
- **Service Mesh**: Secure service-to-service communication with mTLS
- **Configuration Management**: Distributed key-value store for configuration
- **Traffic Management**: Load balancing, traffic splitting, and failover
- **Network Infrastructure**: Automated network device configuration
- **Multi-Datacenter**: Built-in support for multiple datacenters

## Directory Structure

```
consul/
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
├── services/
│   ├── nextjs-app.json
│   ├── api-service.json
│   └── database.json
├── variables.tf
├── outputs.tf
└── README.md
```

## Environment Configuration

Each environment includes:

- **Service Registration**: Automatic registration of application services
- **Health Checks**: HTTP and TCP health checks for services
- **Key-Value Store**: Configuration management for applications
- **Service Mesh**: Secure communication between services
- **Load Balancing**: Traffic distribution across service instances

## Quick Start

1. **Configure Environment Variables**:
   ```bash
   export CONSUL_HTTP_ADDR="https://consul.example.com:8500"
   export CONSUL_HTTP_TOKEN="your-consul-token"
   ```

2. **Initialize Terraform**:
   ```bash
   cd terraform/providers/hashicorp/consul/environments/development
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

## Service Discovery

### Service Registration
```hcl
# Register Next.js application
resource "consul_service" "nextjs_app" {
  name = "nextjs-app"
  port = 3000
  
  check {
    http     = "http://localhost:3000/api/health"
    interval = "30s"
  }
}
```

### Service Lookup
```bash
# Discover services via DNS
dig @consul.service.consul nextjs-app.service.consul

# Discover services via HTTP API
curl http://consul.example.com:8500/v1/catalog/service/nextjs-app
```

## Service Mesh

### Connect Configuration
```hcl
# Enable Connect for service mesh
resource "consul_config_entry" "proxy_defaults" {
  kind = "proxy-defaults"
  name = "global"
  
  config_json = jsonencode({
    Config = {
      protocol = "http"
    }
  })
}
```

### Service Intentions
```hcl
# Allow API to database communication
resource "consul_config_entry" "api_to_db" {
  kind = "service-intentions"
  name = "database"
  
  config_json = jsonencode({
    Sources = [{
      Name   = "api-service"
      Action = "allow"
    }]
  })
}
```

## Configuration Management

### Key-Value Store
```bash
# Store application configuration
consul kv put config/app/database_url "postgresql://..."
consul kv put config/app/log_level "info"

# Read configuration
consul kv get config/app/database_url
```

### Consul Template
```bash
# Template for application configuration
{{key "config/app/database_url"}}
{{key "config/app/log_level"}}
```

## Load Balancing

### Service Resolver
```hcl
resource "consul_config_entry" "api_resolver" {
  kind = "service-resolver"
  name = "api-service"
  
  config_json = jsonencode({
    DefaultSubset = "v1"
    Subsets = {
      v1 = {
        Filter = "Service.Meta.version == v1"
      }
    }
  })
}
```

### Service Splitter
```hcl
resource "consul_config_entry" "api_splitter" {
  kind = "service-splitter"
  name = "api-service"
  
  config_json = jsonencode({
    Splits = [
      {
        Weight        = 90
        ServiceSubset = "v1"
      },
      {
        Weight        = 10
        ServiceSubset = "v2"
      }
    ]
  })
}
```

## Health Checks

### HTTP Health Checks
```json
{
  "check": {
    "id": "api-health",
    "name": "API Health Check",
    "http": "http://localhost:8000/health",
    "interval": "30s",
    "timeout": "5s"
  }
}
```

### TCP Health Checks
```json
{
  "check": {
    "id": "db-health",
    "name": "Database Health Check",
    "tcp": "localhost:5432",
    "interval": "30s",
    "timeout": "5s"
  }
}
```

## Integration Examples

### Next.js Application
```javascript
// Service discovery with consul
const consul = require('consul')();

// Discover API service
const services = await consul.health.service('api-service');
const apiUrl = `http://${services[0].Service.Address}:${services[0].Service.Port}`;
```

### API Service
```javascript
// Register with Consul
const consul = require('consul')();

await consul.agent.service.register({
  name: 'api-service',
  port: 8000,
  check: {
    http: 'http://localhost:8000/health',
    interval: '30s'
  }
});
```

### Docker Integration
```yaml
# docker-compose.yml
version: '3.8'
services:
  consul:
    image: consul:latest
    ports:
      - "8500:8500"
    command: agent -server -ui -node=server-1 -bootstrap-expect=1 -client=0.0.0.0
```

## Security

### ACL Configuration
```bash
# Bootstrap ACL system
consul acl bootstrap

# Create policy
consul acl policy create \
  -name "api-service" \
  -description "Policy for API service" \
  -rules @api-policy.hcl

# Create token
consul acl token create \
  -description "Token for API service" \
  -policy-name "api-service"
```

### TLS Configuration
```bash
# Generate CA certificate
consul tls ca create

# Generate server certificates
consul tls cert create -server -dc dc1
```

## Monitoring

### Metrics
- **Prometheus Integration**: Export Consul metrics
- **Grafana Dashboards**: Visualize service health and performance
- **Custom Metrics**: Application-specific metrics

### Logging
```bash
# Enable debug logging
consul agent -log-level=DEBUG

# Structured logging
consul agent -log-json
```

## Environment Differences

### Development
- **Single Node**: Simple single-node deployment
- **UI Enabled**: Web UI for easy development
- **No ACLs**: Simplified access for development
- **Local Storage**: File-based storage

### Staging
- **Multi-Node**: 3-node cluster for testing
- **ACLs Enabled**: Test security configurations
- **Service Mesh**: Test Connect features
- **External Storage**: Cloud storage backends

### Production
- **Multi-Datacenter**: Multiple datacenter setup
- **Full Security**: ACLs, TLS, and encryption
- **High Availability**: Redundant deployment
- **Monitoring**: Comprehensive monitoring and alerting

## Best Practices

1. **Service Health**: Implement comprehensive health checks
2. **Security**: Use ACLs and TLS in production
3. **Monitoring**: Monitor service health and performance
4. **Backup**: Regular backup of Consul data
5. **Versioning**: Version your service configurations
6. **Documentation**: Document service dependencies

## Cost Optimization

- **Open Source**: Free for basic features
- **Enterprise**: Licensed features for advanced use cases
- **Cloud Managed**: Consul as a service options
- **Resource Sizing**: Right-size based on service count

## Support

- [Consul Documentation](https://www.consul.io/docs)
- [Terraform Consul Provider](https://registry.terraform.io/providers/hashicorp/consul/latest/docs)
- [HashiCorp Support](https://support.hashicorp.com/)
