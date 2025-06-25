# HashiCorp Vault Infrastructure Provider

This module provides secrets management and encryption services using HashiCorp Vault.

## Overview

HashiCorp Vault is a tool for securely accessing secrets, managing encryption keys, and handling identity-based access.

## Features

- **Dynamic Secrets**: Generate secrets on-demand with configurable TTL
- **Data Encryption**: Encrypt data in transit and at rest
- **Identity-based Access**: Authenticate and authorize users and applications
- **Audit Logging**: Detailed audit logs for compliance
- **Secret Versioning**: Track and manage secret versions
- **Auto-unseal**: Automatic unsealing with cloud KMS

## Directory Structure

```
vault/
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
├── policies/
│   ├── app-policy.hcl
│   ├── database-policy.hcl
│   └── admin-policy.hcl
├── variables.tf
├── outputs.tf
└── README.md
```

## Environment Configuration

Each environment includes:

- **Secret Engines**: Key-value, database, AWS, PKI
- **Authentication Methods**: JWT, GitHub, userpass
- **Policies**: Role-based access control
- **Audit Devices**: File and syslog audit logging

## Quick Start

1. **Configure Environment Variables**:
   ```bash
   export VAULT_ADDR="https://vault.example.com:8200"
   export VAULT_TOKEN="your-vault-token"
   ```

2. **Initialize Terraform**:
   ```bash
   cd terraform/providers/hashicorp/vault/environments/development
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

## Secret Engines

### Key-Value Store
- **Version 2**: Versioned secret storage
- **Path**: `secret/`
- **Use Case**: Application configuration, API keys

### Database Secrets
- **Dynamic Credentials**: Generate database users on demand
- **Supported Databases**: PostgreSQL, MySQL, MongoDB
- **Use Case**: Application database access

### Cloud Secrets
- **AWS**: Dynamic AWS credentials
- **GCP**: Service account keys
- **Azure**: Service principal credentials

### PKI Engine
- **Certificate Authority**: Issue and manage certificates
- **Root CA**: Self-signed root certificate
- **Intermediate CA**: Signed by root CA

## Authentication Methods

### JWT/OIDC
```bash
# Configure JWT auth for applications
vault auth enable jwt
vault write auth/jwt/config \
    bound_issuer="https://auth.example.com" \
    jwks_url="https://auth.example.com/.well-known/jwks.json"
```

### GitHub
```bash
# Configure GitHub auth for developers
vault auth enable github
vault write auth/github/config organization=your-org
```

### Userpass
```bash
# Configure username/password auth
vault auth enable userpass
vault write auth/userpass/users/developer \
    password=secure-password \
    policies=developer-policy
```

## Policies

### Application Policy
```hcl
# Read application secrets
path "secret/data/app/*" {
  capabilities = ["read"]
}

# Generate database credentials
path "database/creds/app-role" {
  capabilities = ["read"]
}
```

### Database Policy
```hcl
# Manage database secrets engine
path "database/*" {
  capabilities = ["create", "read", "update", "delete", "list"]
}
```

### Admin Policy
```hcl
# Full access to Vault
path "*" {
  capabilities = ["create", "read", "update", "delete", "list", "sudo"]
}
```

## Secret Management

### Application Secrets
```bash
# Store application configuration
vault kv put secret/app/config \
    database_url="postgresql://..." \
    api_key="secret-key"

# Read application configuration
vault kv get secret/app/config
```

### Database Credentials
```bash
# Generate database credentials
vault read database/creds/app-role
```

### Certificate Management
```bash
# Generate certificate
vault write pki/issue/app-role \
    common_name="app.example.com" \
    ttl="24h"
```

## Integration Examples

### Next.js Application
```javascript
// Using vault-node client
const vault = require('node-vault')({
  endpoint: process.env.VAULT_ADDR,
  token: process.env.VAULT_TOKEN
});

// Read application secrets
const secrets = await vault.read('secret/data/app/config');
const config = secrets.data.data;
```

### API Service
```javascript
// Generate database credentials
const dbCreds = await vault.read('database/creds/app-role');
const connectionString = `postgresql://${dbCreds.data.username}:${dbCreds.data.password}@db:5432/app`;
```

### Certificate Renewal
```bash
#!/bin/bash
# Automated certificate renewal
vault write pki/issue/app-role \
    common_name="$APP_DOMAIN" \
    ttl="24h" \
    format=pem > /tmp/cert.pem
```

## Security Best Practices

1. **Least Privilege**: Grant minimal necessary permissions
2. **Regular Rotation**: Rotate secrets and tokens regularly
3. **Audit Logging**: Enable comprehensive audit logging
4. **Network Security**: Use TLS for all communications
5. **Token TTL**: Set appropriate token time-to-live
6. **Backup**: Regular backup of Vault data

## Monitoring

### Health Checks
```bash
# Check Vault status
vault status

# Check seal status
vault operator raft list-peers
```

### Metrics
- **Prometheus Integration**: Export metrics to Prometheus
- **Grafana Dashboards**: Visualize Vault metrics
- **Alert Rules**: Monitor critical Vault events

### Audit Logs
```bash
# Enable file audit device
vault audit enable file file_path=/vault/logs/audit.log

# Enable syslog audit device
vault audit enable syslog tag="vault" facility="AUTH"
```

## Disaster Recovery

### Backup
```bash
# Take snapshot (Vault Enterprise)
vault operator raft snapshot save backup.snap

# Restore snapshot
vault operator raft snapshot restore backup.snap
```

### High Availability
- **Raft Storage**: Integrated storage with HA
- **Load Balancing**: Distribute requests across nodes
- **Auto-unseal**: Automatic unsealing with cloud KMS

## Environment Differences

### Development
- **Single Node**: Simple single-node deployment
- **In-Memory Storage**: Fast, non-persistent storage
- **Dev Mode**: Auto-unsealed, pre-configured
- **Basic Policies**: Simplified access control

### Staging
- **Multi-Node**: 3-node cluster for testing HA
- **File Storage**: Persistent file-based storage
- **Manual Unseal**: Practice unseal procedures
- **Production Policies**: Full policy set

### Production
- **Multi-Node Cluster**: 5-node cluster for high availability
- **Cloud Storage**: Cloud-native storage backends
- **Auto-unseal**: Automatic unsealing with KMS
- **Enterprise Features**: Advanced features if licensed

## Cost Optimization

- **Open Source**: Free for basic features
- **Enterprise**: Licensed features for advanced use cases
- **Cloud Managed**: Vault as a service options
- **Resource Sizing**: Right-size compute and storage

## Support

- [Vault Documentation](https://www.vaultproject.io/docs)
- [Terraform Vault Provider](https://registry.terraform.io/providers/hashicorp/vault/latest/docs)
- [HashiCorp Support](https://support.hashicorp.com/)
