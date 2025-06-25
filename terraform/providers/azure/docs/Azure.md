# Azure Infrastructure for Nexpo

This directory contains the Azure-specific Terraform configurations for deploying the Nexpo application on Microsoft Azure.

## 🏗️ Architecture Overview

The Azure infrastructure includes:

- **Networking**: Virtual Network with public/private subnets, NAT Gateway
- **Compute**: Container Apps for serverless container hosting
- **Storage**: Storage Accounts for blobs, queues, and tables
- **Database**: Azure Database for PostgreSQL with optional replicas
- **CDN**: Azure CDN for global content delivery
- **Security**: Key Vault, Network Security Groups, DDoS Protection
- **Monitoring**: Application Insights, Log Analytics, Azure Monitor

## 📁 Directory Structure

```
azure/
├── modules/                 # Reusable Azure modules
│   ├── networking/         # VNet, subnets, NSGs, NAT Gateway
│   ├── compute/           # Container Apps, Container Registry
│   ├── storage/           # Storage accounts, containers, queues
│   ├── database/          # PostgreSQL, Cosmos DB
│   ├── cdn/               # CDN profiles and endpoints
│   ├── monitoring/        # App Insights, Log Analytics
│   └── security/          # Key Vault, Managed Identities
├── environments/           # Environment configurations
│   ├── development/        # Development environment
│   ├── staging/            # Staging environment
│   └── production/         # Production environment
├── terraform.tfvars.example  # Example configuration
└── README.md             # This file
```

## 🚀 Quick Start

### Prerequisites

1. **Azure CLI**: Install and authenticate
   ```bash
   az login
   az account set --subscription "Your Subscription Name"
   ```

2. **Terraform**: Version 1.5.0 or later
   ```bash
   terraform version
   ```

3. **Docker**: For building container images
   ```bash
   docker --version
   ```

### Configuration

1. **Copy the example configuration**:
   ```bash
   cp terraform.tfvars.example ../../azure.tfvars
   ```

2. **Edit the configuration**:
   ```bash
   # Edit ../../azure.tfvars with your Azure-specific settings
   vim ../../azure.tfvars
   ```

3. **Key configurations to update**:
   - `location`: Your preferred Azure region
   - `resource_group_name`: Resource group name
   - `action_group_emails`: Email for alerts
   - `postgresql_sku`: Database size
   - `container_cpu` and `container_memory`: Container resources

### Deployment

From the root terraform directory:

```bash
# Initialize Terraform
terraform init

# Plan the deployment
terraform plan -var="enable_azure=true" \
               -var="environment=dev" \
               -var-file="azure.tfvars"

# Apply the configuration
terraform apply -var="enable_azure=true" \
                -var="environment=dev" \
                -var-file="azure.tfvars"
```

## 🔧 Module Details

### Networking Module
- Creates Virtual Network with address space
- Public subnets for load balancers
- Private subnets for containers
- Database subnets with service endpoints
- Network Security Groups with rules
- NAT Gateway for outbound connectivity

### Compute Module
- Container Registry for Docker images
- Container Apps Environment
- Container Apps with auto-scaling
- Managed Identities for authentication
- Integration with Key Vault

### Storage Module
- General-purpose v2 storage accounts
- Blob containers for assets and uploads
- Queue storage for async processing
- Table storage for session management
- Lifecycle management policies

### Database Module
- Azure Database for PostgreSQL Flexible Server
- High availability with zone redundancy (production)
- Automated backups and point-in-time restore
- Read replicas for scaling
- Private endpoint for security

### CDN Module
- CDN profile with global PoPs
- Custom domains with SSL
- Caching rules and optimization
- Compression and minification
- Integration with storage accounts

### Monitoring Module
- Application Insights for APM
- Log Analytics workspace
- Custom dashboards and workbooks
- Alert rules and action groups
- Cost analysis and budgets

### Security Module
- Key Vault for secrets management
- Managed Identities for passwordless auth
- Network isolation with private endpoints
- DDoS Protection Standard (production)
- Azure Policy for compliance

## 📊 Environment Configurations

### Development
- Basic tier services
- Single zone deployment
- Minimal replicas
- Standard monitoring

### Staging
- Standard tier services
- Zone redundant where available
- Auto-scaling enabled
- Enhanced monitoring

### Production
- Premium tier services
- Multi-zone high availability
- Read replicas for database
- Full security features
- Advanced monitoring

## 💰 Cost Optimization

### Development Environment
- Container Apps: ~$10/month (consumption plan)
- PostgreSQL: ~$25/month (Basic tier)
- Storage: ~$5/month
- **Total**: ~$40-50/month

### Production Environment
- Container Apps: ~$100-300/month (based on usage)
- PostgreSQL: ~$200-500/month (with HA)
- CDN: ~$20-100/month
- Storage: ~$10-50/month
- **Total**: ~$350-1000/month

### Cost Saving Tips
1. Use Azure Reservations for consistent workloads
2. Enable auto-shutdown for dev resources
3. Use Spot instances where applicable
4. Set up cost alerts and budgets
5. Review Azure Advisor recommendations

## 🔒 Security Best Practices

1. **Network Security**
   - Private endpoints for PaaS services
   - Network Security Groups on all subnets
   - DDoS Protection on public IPs

2. **Identity & Access**
   - Managed Identities over passwords
   - Azure AD integration
   - RBAC for fine-grained control

3. **Data Protection**
   - Encryption at rest by default
   - Customer-managed keys in Key Vault
   - SSL/TLS for all connections

4. **Compliance**
   - Azure Policy for governance
   - Security Center for recommendations
   - Defender for Cloud for threats

## 🆘 Troubleshooting

### Common Issues

1. **Container Apps deployment fails**
   - Check Container Registry access
   - Verify image architecture (linux/amd64)
   - Review container logs in Log Analytics

2. **Database connection issues**
   - Verify firewall rules
   - Check private endpoint DNS
   - Ensure connection string in Key Vault

3. **High costs**
   - Review Cost Analysis
   - Check for unused resources
   - Enable auto-shutdown policies

### Useful Commands

```bash
# List Container Apps
az containerapp list --resource-group rg-Nexpo-dev

# View PostgreSQL servers
az postgres flexible-server list --resource-group rg-Nexpo-dev

# List storage accounts
az storage account list --resource-group rg-Nexpo-dev

# View recent alerts
az monitor alert list --resource-group rg-Nexpo-dev
```

## 📚 Additional Resources

- [Container Apps Documentation](https://docs.microsoft.com/azure/container-apps/)
- [PostgreSQL Flexible Server](https://docs.microsoft.com/azure/postgresql/flexible-server/)
- [Azure CDN Documentation](https://docs.microsoft.com/azure/cdn/)
- [Azure Well-Architected Framework](https://docs.microsoft.com/azure/architecture/framework/)

## 🚀 CI/CD Integration

The infrastructure supports GitHub Actions workflows for:
- Building and pushing images to Container Registry
- Deploying to Container Apps
- Running database migrations
- Purging CDN cache
- Blue-green deployments

See the main project's `.github/workflows` for examples.
