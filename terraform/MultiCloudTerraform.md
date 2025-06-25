# Multi-Cloud Terraform Infrastructure

This directory contains Terraform configurations for deploying the Nexpo application across multiple cloud providers. The infrastructure is organized by provider, allowing you to choose one or more cloud platforms for deployment.

## 🏗️ Architecture Overview

The infrastructure supports deployment to:
- **AWS** (Amazon Web Services)
- **GCP** (Google Cloud Platform)  
- **Azure** (Microsoft Azure)
- **OCI** (Oracle Cloud Infrastructure)
- **IBM Cloud** (IBM Cloud Platform)
- **DigitalOcean** (DigitalOcean Cloud)
- **Cloudflare** (Cloudflare Workers & Pages)
- **HashiCorp Nomad** (Container Orchestration)
- **HashiCorp Vault** (Secrets Management)
- **HashiCorp Consul** (Service Discovery & Mesh)
- **Heroku** (Platform as a Service)
- **Auth0** (Identity & Access Management)
- **MindsDB** (AI Database) - Work in progress (WIP)

Each provider includes:
- **Networking**: VPC/VNet, subnets, load balancers
- **Storage**: Object storage, file systems
- **Database**: Managed PostgreSQL, NoSQL options
- **Compute**: Container/serverless platforms
- **CDN**: Content delivery networks
- **Monitoring**: Dashboards, alerts, logging
- **Security**: WAF, encryption, secrets management

## 📁 Directory Structure

```
terraform/
├── main.tf                 # Main configuration (provider selection)
├── variables.tf           # Common variables
├── terraform.tfvars.example # Example configuration
├── README.md              # This file
├── providers/
│   ├── aws/              # AWS infrastructure
│   │   ├── modules/      # AWS-specific modules
│   │   ├── environments/ # Development/staging/production configs
│   │   └── README.md     # AWS-specific docs
│   ├── gcp/              # GCP infrastructure
│   │   ├── modules/      # GCP-specific modules
│   │   ├── environments/ # Development/staging/production configs
│   │   └── README.md     # GCP-specific docs
│   ├── azure/            # Azure infrastructure
│   │   ├── modules/      # Azure-specific modules
│   │   ├── environments/ # Development/staging/production configs
│   │   └── README.md     # Azure-specific docs
│   ├── oci/              # OCI infrastructure
│   │   ├── modules/      # OCI-specific modules
│   │   ├── environments/ # Development/staging/production configs
│   │   └── README.md     # OCI-specific docs
│   ├── ibm/              # IBM Cloud infrastructure
│   │   ├── modules/      # IBM-specific modules
│   │   ├── environments/ # Development/staging/production configs
│   │   └── README.md     # IBM-specific docs
│   ├── digitalocean/     # DigitalOcean infrastructure
│   │   ├── modules/      # DigitalOcean-specific modules
│   │   ├── environments/ # Development/staging/production configs
│   │   └── README.md     # DigitalOcean-specific docs
│   ├── cloudflare/       # Cloudflare infrastructure
│   │   ├── modules/      # Cloudflare-specific modules
│   │   ├── environments/ # Development/staging/production configs
│   │   └── README.md     # Cloudflare-specific docs
│   ├── hashicorp/        # HashiCorp stack
│   │   ├── nomad/        # Container orchestration
│   │   │   ├── environments/ # Development/staging/production configs
│   │   │   ├── jobs/     # Nomad job templates
│   │   │   └── README.md # Nomad-specific docs
│   │   ├── vault/        # Secrets management
│   │   │   ├── environments/ # Development/staging/production configs
│   │   │   ├── policies/ # Vault policy files
│   │   │   └── README.md # Vault-specific docs
│   │   └── consul/       # Service discovery & mesh
│   │       ├── environments/ # Development/staging/production configs
│   │       ├── services/ # Service definitions
│   │       └── README.md # Consul-specific docs
│   ├── heroku/           # Heroku PaaS
│   │   ├── environments/ # Development/staging/production configs
│   │   └── README.md     # Heroku-specific docs
│   └── auth0/            # Auth0 identity management
│       ├── environments/ # Development environment (single-tenant)
│       └── README.md     # Auth0-specific docs
└── microservices/        # Microservices configurations
    ├── auth/            # Auth0 integration service
    ├── user/            # User management service
    ├── kpi-engine/      # KPI calculation service
    ├── ai-advisor/      # AI recommendations
    ├── notifications/   # Email/SMS service
    ├── reports/         # Report generation
    ├── data-sync/       # Data integration
    ├── api-gateway/     # Mobile API gateway
    └── payments/        # Stripe/Qonversion service
```

## 🚀 Quick Start

### 1. Choose Your Provider(s)

The project uses feature flags from `.env.example` to enable/disable providers. Set these environment variables:

```bash
# Core Cloud Providers
ENABLE_AWS=true
ENABLE_GCP=false
ENABLE_AZURE=false
ENABLE_OCI=false
ENABLE_IBM=false
ENABLE_DIGITALOCEAN=false
ENABLE_CLOUDFLARE=false

# HashiCorp Stack
ENABLE_HASHICORP_NOMAD=false
ENABLE_HASHICORP_VAULT=false
ENABLE_HASHICORP_CONSUL=false

# Platform as a Service
ENABLE_HEROKU=false

# Identity & Auth
ENABLE_AUTH0=false
```

### 2. Configure Provider-Specific Settings

Create provider-specific variable files based on enabled providers:

**For AWS:**
```bash
cp providers/aws/environments/development/terraform.tfvars.example aws.tfvars
# Edit aws.tfvars with your AWS settings
```

**For GCP:**
```bash
cp providers/gcp/environments/development/terraform.tfvars.example gcp.tfvars
# Edit gcp.tfvars with your GCP settings
```

**For Azure:**
```bash
cp providers/azure/environments/development/terraform.tfvars.example azure.tfvars
# Edit azure.tfvars with your Azure settings
```

**For OCI:**
```bash
cp providers/oci/environments/development/terraform.tfvars.example oci.tfvars
# Edit oci.tfvars with your OCI settings
```

**For IBM Cloud:**
```bash
cp providers/ibm/environments/development/terraform.tfvars.example ibm.tfvars
# Edit ibm.tfvars with your IBM Cloud settings
```

**For DigitalOcean:**
```bash
cp providers/digitalocean/environments/development/terraform.tfvars.example digitalocean.tfvars
# Edit digitalocean.tfvars with your DigitalOcean settings
```

**For Cloudflare:**
```bash
cp providers/cloudflare/environments/development/terraform.tfvars.example cloudflare.tfvars
# Edit cloudflare.tfvars with your Cloudflare settings
```

**For HashiCorp Nomad:**
```bash
cp providers/hashicorp/nomad/environments/development/terraform.tfvars.example nomad.tfvars
# Edit nomad.tfvars with your Nomad cluster settings
```

**For HashiCorp Vault:**
```bash
cp providers/hashicorp/vault/environments/development/terraform.tfvars.example vault.tfvars
# Edit vault.tfvars with your Vault cluster settings
```

**For HashiCorp Consul:**
```bash
cp providers/hashicorp/consul/environments/development/terraform.tfvars.example consul.tfvars
# Edit consul.tfvars with your Consul cluster settings
```

**For Heroku:**
```bash
cp providers/heroku/environments/development/terraform.tfvars.example heroku.tfvars
# Edit heroku.tfvars with your Heroku settings
```

**For Auth0:**
```bash
cp providers/auth0/environments/development/terraform.tfvars.example auth0.tfvars
# Edit auth0.tfvars with your Auth0 tenant settings
```

### 3. Initialize and Deploy

```bash
# Initialize Terraform
terraform init

# Plan deployment (only enabled providers will be included)
terraform plan

# Apply changes
terraform apply
```

## 🎯 Provider-Specific Documentation

### Cloud Providers
- **[AWS Documentation](./providers/aws/README.md)** - ECS, RDS, CloudFront setup
- **[GCP Documentation](./providers/gcp/README.md)** - Cloud Run, Cloud SQL setup  
- **[Azure Documentation](./providers/azure/README.md)** - Container Apps, PostgreSQL setup
- **[OCI Documentation](./providers/oci/README.md)** - OKE, Autonomous DB, Container Registry setup
- **[IBM Cloud Documentation](./providers/ibm/README.md)** - Code Engine, Databases for PostgreSQL, CDN setup
- **[DigitalOcean Documentation](./providers/digitalocean/README.md)** - App Platform, Databases, Spaces setup, Droplets
- **[Cloudflare Documentation](./providers/cloudflare/README.md)** - Workers, Pages, D1 Database, R2 Storage setup

### HashiCorp Stack
- **[HashiCorp Nomad Documentation](./providers/hashicorp/nomad/README.md)** - Container orchestration, job scheduling
- **[HashiCorp Vault Documentation](./providers/hashicorp/vault/README.md)** - Secrets management, dynamic credentials
- **[HashiCorp Consul Documentation](./providers/hashicorp/consul/README.md)** - Service discovery, service mesh, KV store

### Platform as a Service
- **[Heroku Documentation](./providers/heroku/README.md)** - Dyno management, add-ons, pipelines

### Identity & Authentication
- **[Auth0 Documentation](./providers/auth0/README.md)** - Single sign-on, multi-tenant applications

## 🧩 Microservices Architecture

The infrastructure supports a microservices architecture with the following services:

| Service | Description | Technologies |
|---------|-------------|--------------|
| **Auth Service** | Auth0 integration, RBAC, CRA (Central Rank Authority) | Node.js, Express |
| **User Service** | User CRUD operations | Node.js, Supabase |
| **KPI Engine** | Business metrics calculation | Python, Apache Druid |
| **AI Advisor** | OKR/goal recommendations | Python, OpenAI/Gemini |
| **Notifications** | Email/SMS delivery | Node.js, Brevo |
| **Report Builder** | Report generation | Python, Looker Studio |
| **Data Sync** | Third-party integrations | n8n/Zapier/Make |
| **API Gateway** | Mobile-optimized API | Node.js, gRPC |
| **Payments** | Subscription management | Node.js, Stripe/Qonversion |

## 🔧 Common Operations

### Deploying to Multiple Providers

The GitHub Actions workflow automatically deploys only enabled providers:

```yaml
# Example: Deploy to AWS, HashiCorp Vault, and Heroku
ENABLE_AWS=true
ENABLE_HASHICORP_VAULT=true  
ENABLE_HEROKU=true
# All other providers set to false
```

### Environment-Specific Deployment

```bash
# Deploy to specific environment
terraform workspace select production
terraform apply -var="environment=production"
```

### Destroying Infrastructure

```bash
# Destroy specific provider (if using modules)
terraform destroy -target=module.aws

# Destroy everything
terraform destroy
```

## 📊 Cost Optimization

Each provider configuration includes:
- **Development**: Minimal resources, no HA, hobby tiers
- **Staging**: Production-like with reduced capacity
- **Production**: Full HA, auto-scaling, enhanced security

### Provider Cost Characteristics
- **AWS/GCP/Azure**: Pay-per-use with reserved instance discounts
- **Heroku**: Fixed dyno pricing, simple scaling
- **DigitalOcean**: Predictable droplet pricing
- **HashiCorp**: Self-managed or cloud offerings
- **Auth0**: Per-user pricing with free tier

## 🔒 Security Best Practices

- All secrets stored in HashiCorp Vault or provider secret managers
- Encryption at rest and in transit
- Network isolation with private subnets
- WAF/DDoS protection enabled
- Least privilege IAM policies
- Auth0 for centralized identity management
- Consul Connect for service mesh security

## 🆘 Troubleshooting

1. **Provider Authentication**: Ensure CLI tools are configured
   - AWS: `aws configure`
   - GCP: `gcloud auth application-default login`
   - Azure: `az login`
   - OCI: `oci setup config`
   - IBM Cloud: `ibmcloud login`
   - DigitalOcean: `doctl auth init`
   - Cloudflare: Set `CLOUDFLARE_API_TOKEN`
   - Heroku: `heroku login`
   - Auth0: Use Management API credentials

2. **State Management**: Each provider can use its own backend
   - AWS: S3 + DynamoDB
   - GCP: Cloud Storage
   - Azure: Storage Account
   - OCI: Object Storage
   - IBM Cloud: Cloud Object Storage
   - DigitalOcean: Spaces
   - Cloudflare: Workers KV
   - HashiCorp: Consul backend or cloud backends

3. **Module Errors**: Check provider-specific README files

4. **Feature Flag Issues**: Ensure environment variables match `.env.example`

## 🚀 CI/CD Integration

The project includes a GitHub Actions workflow (`.github/workflows/terraform.yml`) that:
- Reads feature flags from environment variables
- Conditionally deploys only enabled providers
- Supports development and production environments
- Includes cost estimation and security scanning

## 📝 Contributing

When adding new features:
1. Update the appropriate provider modules
2. Ensure compatibility across all environments
3. Update provider-specific documentation
4. Test in development environment first
5. Update feature flags in `.env.example`
6. Update GitHub Actions workflow if needed

## 📄 License

This infrastructure code follows the same license as the main project.

## Supported Cloud Providers

### Core Cloud Infrastructure

#### Amazon Web Services (AWS)
- **Enable Flag**: `ENABLE_AWS=true`
- **Modules**: VPC, EC2, RDS, Lambda, API Gateway, CloudFront, S3, DynamoDB, DocumentDB, ElastiCache, Neptune, Timestream
- **Auth0 Integration**: IAM database authentication, JWT validation via API Gateway, Lambda authorizers
- **Database Services**:
  - **RDS**: PostgreSQL, MySQL, MariaDB, Oracle, SQL Server with IAM authentication
  - **DynamoDB**: NoSQL with fine-grained access control using IAM policies
  - **DocumentDB**: MongoDB-compatible with VPC security groups
  - **ElastiCache**: Redis and Memcached with Auth0 token-based access
  - **Neptune**: Graph database with SPARQL/Gremlin authorization
  - **Timestream**: Time-series database with user context filtering

#### Google Cloud Platform (GCP)
- **Enable Flag**: `ENABLE_GCP=true`
- **Modules**: VPC, Compute Engine, Cloud Run, Firebase, Cloud SQL, Cloud Storage, Cloud Functions
- **Auth0 Integration**: Cloud IAM with Auth0 JWT validation, Firebase custom tokens
- **Database Services**:
  - **Cloud SQL**: PostgreSQL, MySQL, SQL Server with Cloud IAM integration
  - **Firestore**: NoSQL document database with security rules
  - **Firebase Realtime Database**: Real-time synchronization with user-based rules
  - **Cloud Spanner**: Globally distributed relational database
  - **Cloud Bigtable**: Wide-column NoSQL for analytics
  - **Cloud Memorystore**: Redis and Memcached with VPC-native security

#### Microsoft Azure
- **Enable Flag**: `ENABLE_AZURE=true`
- **Modules**: Resource Groups, Virtual Network, Virtual Machines, App Service, Functions, Storage, Cosmos DB
- **Auth0 Integration**: Azure AD federation, JWT validation in App Service
- **Database Services**:
  - **Azure SQL Database**: Relational database with Azure AD integration
  - **Cosmos DB**: Multi-model NoSQL with JWT validation in stored procedures
  - **Azure Database for PostgreSQL/MySQL**: Managed databases with RBAC
  - **Azure Cache for Redis**: In-memory caching with token validation
  - **Azure Digital Twins**: Graph database for IoT with device authorization

#### Oracle Cloud Infrastructure (OCI)
- **Enable Flag**: `ENABLE_OCI=true`
- **Modules**: VCN, Compute, Load Balancer, Object Storage, Autonomous Database
- **Auth0 Integration**: OCI IAM with Auth0 user provisioning
- **Database Services**:
  - **Autonomous Database**: Self-managing Oracle database
  - **MySQL Database Service**: Managed MySQL with OCI IAM
  - **NoSQL Database**: Document and key-value store
  - **Analytics Cloud**: Data warehouse with row-level security

#### IBM Cloud
- **Enable Flag**: `ENABLE_IBM=true`
- **Modules**: VPC, VSI, Kubernetes Service, Cloud Functions, Object Storage, Databases
- **Auth0 Integration**: IBM Cloud IAM with Auth0 user mapping
- **Database Services**:
  - **Db2 on Cloud**: Enterprise relational database
  - **Cloudant**: Apache CouchDB-based NoSQL
  - **Databases for PostgreSQL/MongoDB**: Managed databases
  - **Redis**: In-memory database with connection authentication

#### DigitalOcean
- **Enable Flag**: `ENABLE_DIGITALOCEAN=true`
- **Modules**: Droplets, Kubernetes, App Platform, Spaces, Managed Databases
- **Auth0 Integration**: App Platform environment variables, connection pooling
- **Database Services**:
  - **Managed Databases**: PostgreSQL, MySQL, Redis
  - **App Platform**: Database integration with environment variables
  - **Spaces**: Object storage with presigned URL generation

### Specialized Database Providers

#### Supabase
- **Enable Flag**: `ENABLE_SUPABASE=true`
- **Integration Pattern**: PostgreSQL with Row-Level Security (RLS) policies
- **Auth0 Features**:
  - RLS policies validating Auth0 JWT claims
  - Realtime subscriptions authenticated via Auth0 tokens
  - Edge Functions with Auth0 context for database operations
  - Storage buckets with user metadata and role-based access

#### Firebase
- **Enable Flag**: `ENABLE_FIREBASE=true` 
- **Integration Pattern**: NoSQL with security rules engine
- **Auth0 Features**:
  - Security rules validating Auth0 custom claims
  - Firebase rules for JWT token validation
  - Cloud Storage with user context permissions
  - Cloud Functions with token exchange patterns

#### MongoDB Atlas
- **Enable Flag**: `ENABLE_MONGODB=true`
- **Integration Pattern**: Managed MongoDB with Atlas App Services
- **Auth0 Features**:
  - Role-based access control synchronized with Auth0 roles
  - Field-level permissions using Auth0 metadata
  - Atlas Search with user context filtering
  - Change Streams with webhook integration

#### MindsDB
- **Enable Flag**: `ENABLE_MINDSDB=true`
- **Integration Pattern**: AI-powered analytics platform
- **Auth0 Features**:
  - Machine learning models with user context
  - Natural language queries with permission validation
  - User-specific predictions based on metadata
  - Automated data transformation with audit trails

### Platform and Orchestration Providers

#### HashiCorp Suite
- **Enable Flags**: 
  - `ENABLE_HASHICORP_NOMAD=true` - Container orchestration
  - `ENABLE_HASHICORP_VAULT=true` - Secrets management
  - `ENABLE_HASHICORP_CONSUL=true` - Service discovery
- **Auth0 Integration**: 
  - Vault dynamic credentials with Auth0 user context
  - Nomad job authorization based on Auth0 roles
  - Consul service mesh with JWT validation

#### Heroku
- **Enable Flag**: `ENABLE_HEROKU=true`
- **Features**: Application deployment, add-ons, pipelines
- **Auth0 Integration**: Config vars, Heroku Postgres with connection pooling

#### Cloudflare
- **Enable Flag**: `ENABLE_CLOUDFLARE=true`
- **Features**: CDN, Workers, R2 Storage, D1 Database
- **Auth0 Integration**: Workers KV for session storage, JWT validation at edge

## Database Authorization Architecture

### Multi-Tenant Database Strategies

1. **Database-per-Tenant**
   - Separate database instances for each tenant
   - Complete data isolation at infrastructure level
   - Auth0 app_metadata stores tenant database mapping
   - Dynamic connection routing based on JWT claims

2. **Schema-per-Tenant**
   - Single database with isolated schemas
   - Logical separation within shared infrastructure
   - Schema selection based on Auth0 organization ID
   - Reduced infrastructure costs with maintained isolation

3. **Row-Level Security (RLS)**
   - Shared tables with tenant_id columns
   - Database policies enforce data boundaries
   - Auth0 JWT claims passed to database context
   - Most cost-effective for large-scale multi-tenancy

### Authorization Implementation Patterns

1. **Proxy Architecture (Kong Gateway)**
   - JWT validation at API gateway layer
   - Request enrichment with Auth0 user context
   - Database connection pooling per tenant
   - Rate limiting based on Auth0 subscription tiers

2. **Middleware Integration (Prisma)**
   - ORM-level permission enforcement
   - Query modification based on Auth0 roles
   - Automatic tenant filtering in queries
   - Audit logging with user attribution

3. **Service Mesh Security**
   - mTLS between microservices
   - Auth0 token propagation in service calls
   - Circuit breaking for database protection
   - Observability with user context tracing

## Configuration

### Environment Variables

Configure your cloud providers and databases using feature flags in your `.env` file:

```env
# Cloud Provider Feature Flags
ENABLE_AWS=true
ENABLE_GCP=false
ENABLE_AZURE=false
ENABLE_OCI=false
ENABLE_IBM=false
ENABLE_DIGITALOCEAN=false

# Database Provider Feature Flags
ENABLE_POSTGRES=true
ENABLE_MONGODB=false
ENABLE_SUPABASE=false
ENABLE_COSMOSDB=false
ENABLE_SQLSERVER=false
ENABLE_DB2=false

# Default Database Provider
DEFAULT_DATABASE_PROVIDER=postgres

# Auth0 Configuration
AUTH0_DOMAIN=your-tenant.auth0.com
AUTH0_CLIENT_ID=your-client-id
AUTH0_CLIENT_SECRET=your-client-secret
AUTH0_AUDIENCE=https://api.your-app.com
AUTH0_MANAGEMENT_API_TOKEN=your-management-api-token

# Kong Gateway Configuration (if using proxy architecture)
KONG_ENABLE=true
KONG_DATABASE=postgres
KONG_AUTH0_DOMAIN=your-tenant.auth0.com
KONG_AUTH0_AUDIENCE=https://api.your-app.com
```

### Terraform Variables

Each provider directory includes a `terraform.tfvars.example` file with provider-specific variables:

```hcl
# Example AWS variables
project_name = "nexpo"
environment = "production"
region = "us-east-1"

# Auth0 Integration
auth0_domain = "your-tenant.auth0.com"
auth0_audience = "https://api.your-app.com"

# Database Configuration
database_instance_class = "db.t3.medium"
database_allocated_storage = 100
enable_database_encryption = true
enable_iam_database_auth = true
```

## Usage

{{ ... }}
