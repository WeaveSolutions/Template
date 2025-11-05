# Nexpo Terraform Infrastructure

This directory contains comprehensive Terraform configurations for deploying the Nexpo polyglot microservices architecture across multiple cloud providers with integrated CI/CD, Auth0 authentication, and Kong API Gateway management.

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

## 🚀 Backend Services (Ports 7000-7090)

The Nexpo project includes 8 polyglot API backends with feature flag control:

| Backend | Framework | Port | Language Runtime | Feature Flag |
|---------|-----------|------|------------------|-------------|
| TypeScript | Express | 7000 | Node.js 18+ | `enable_typescript_api` |
| Python | FastAPI | 7010 | Python 3.11+ | `enable_python_api` |
| Go | Beego | 7020 | Go 1.21+ | `enable_go_api` |
| Rust | Actix | 7030 | Rust 1.70+ | `enable_rust_api` |
| Scala | Play | 7040 | Scala 2.13/JDK 17+ | `enable_scala_api` |
| Java | Play | 7050 | Java 17+ | `enable_java_api` |
| R | Plumber | 7060 | R 4.3+ | `enable_r_api` |
| Julia | Genie | 7070 | Julia 1.9+ | `enable_julia_api` |
| PHP | Laravel | 7080 | PHP 8.2+ | `enable_php_api` |
| C++ | Drogon | 7090 | C++ 20+ | `enable_cpp_api` |
| .NET | ASP.NET Core | 7100 | .NET 8.0+ | `enable_dotnet_api` |
| Haskell | Servant | 7110 | GHC 9.4+ | `enable_haskell_api` |

As of now, each API backend is deployed to AWS ECS Fargate.

### TO DO
- [] Add provider specific deployment for each API backend

### Backend Features

All backends maintain feature parity with:
- **Auth0 JWT validation** with express-openid-connect/equivalent
- **MindsDB query proxy** endpoint integration
- **User profile endpoints** with authentication
- **Linked accounts endpoints** for social providers
- **Token exchange endpoints** for mobile apps
- **Health checks** and monitoring
- **CORS configuration** for web applications
- **Swagger/OpenAPI documentation** auto-generation
- **Docker support** with multi-stage builds
- **Environment-based configuration**

### API Endpoints

Standard endpoints implemented across all backends:
- `GET /` or `/health` - Health check
- `GET /api/user/profile` - Get user profile (auth required)
- `GET /api/user/linked-accounts` - Get linked accounts (auth required)
- `POST /api/auth/token` - Token exchange
- `GET /api/data/query` - MindsDB query proxy (auth required)
- `GET /docs` or `/swagger` - API documentation

## 📁 Directory Structure

```
terraform/
├── main.tf                 # Main configuration (provider selection)
├── variables.tf           # Common variables
├── terraform.tfvars.example # Example configuration
├── README.md              # This file
├── APITerraform.md        # CI/CD deployment guide (consolidated)
├── MultiCloudTerraform.md # Multi-cloud setup guide (consolidated)
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
    ├── user/             # User management service
    ├── payments/         # Payment processing service
    ├── notifications/    # Notification service
    └── kpi-engine/       # KPI analytics service
```

## 🚀 Quick Start

### 1. Set Cloud Provider Feature Flags

Configure which cloud providers to enable in your `.env` file:

```env
# Cloud Provider Feature Flags
ENABLE_AWS=true
ENABLE_GCP=false
ENABLE_AZURE=false
ENABLE_OCI=false
ENABLE_IBM=false
ENABLE_DIGITALOCEAN=false
ENABLE_CLOUDFLARE=false
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

### 3. Initialize and Deploy

```bash
# Initialize Terraform
terraform init

# Plan deployment
terraform plan

# Deploy infrastructure
terraform apply
```

## ⚙️ Configuration

### Feature Flags

Enable/disable backends by setting variables in `terraform.tfvars`:

```hcl
# Enable TypeScript and Python APIs only
enable_typescript_api = true
enable_python_api = true

# All others remain disabled (false)
enable_go_api = false
enable_rust_api = false
enable_scala_api = false
enable_java_api = false
enable_r_api = false
enable_julia_api = false
```

### Environment Settings

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

### Backend Environment Variables

Control which backends are deployed using environment variables:

```bash
# In .env file
BACKEND_TYPESCRIPT_ENABLED=true
BACKEND_PYTHON_ENABLED=true
BACKEND_GO_ENABLED=false
BACKEND_RUST_ENABLED=true
BACKEND_SCALA_ENABLED=false
BACKEND_JAVA_ENABLED=true
BACKEND_R_ENABLED=false
BACKEND_JULIA_ENABLED=false
```

## 🏗️ Infrastructure Architecture

### AWS Architecture
- **VPC**: Dedicated network with public/private subnets
- **ECS Cluster**: Fargate cluster for running containers
- **Application Load Balancer**: Routes traffic to backend services
- **Target Groups**: Health-checked endpoints for each backend
- **Security Groups**: Allow traffic on backend ports (7000-7090)
- **CloudWatch Logs**: Centralized logging for all services
- **Auto Scaling**: Configurable scaling for each service

### Multi-Cloud Architecture
- **Service Discovery**: HashiCorp Consul for cross-cloud service discovery
- **Secrets Management**: HashiCorp Vault for centralized secrets
- **Load Balancing**: Cloud-native load balancers with Kong API Gateway
- **Container Orchestration**: ECS Fargate, GKE, AKS, or Nomad
- **Database**: Cloud-managed PostgreSQL with Auth0 integration
- **Monitoring**: CloudWatch, Stackdriver, or Prometheus

## 🔐 Security & Authentication

### Auth0 Integration

- **Central Identity Provider**: Single Auth0 tenant for all environments
- **JWT Validation**: All backends validate Auth0 JWT tokens
- **Role-Based Access**: Fine-grained permissions via Auth0 roles
- **Multi-Factor Authentication**: Built-in MFA support
- **Social Providers**: Google, GitHub, Facebook, Twitter integration
- **Custom Claims**: User metadata in JWT tokens

### Database Authorization Architecture

#### Multi-Tenant Database Strategies

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

#### Authorization Implementation Patterns

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

### Security Best Practices

- All secrets stored in HashiCorp Vault or provider secret managers
- Encryption at rest and in transit
- Network isolation with private subnets
- WAF/DDoS protection enabled
- Least privilege IAM policies
- Auth0 for centralized identity management
- Consul Connect for service mesh security

## 🔄 CI/CD Pipeline

### GitHub Actions Integration

The CI/CD pipeline automatically:

- **Builds and tests** 8 polyglot API backends (TypeScript, Python, Go, Rust, Scala, Java, R, Julia)
- **Manages Auth0 tenant configurations** for authentication and authorization
- **Configures Kong API Gateway** for API management and routing
- **Deploys to cloud providers** with infrastructure as code
- **Provides monitoring and health checks** for all services

### Workflow Features

**Environment-Specific Deployment**:
- **Staging Environment**: Triggered on pushes to `develop` branch
- **Production Environment**: Triggered on pushes to `main` branch
- **Manual Deployment**: Workflow dispatch with environment selection

**Build Pipeline**:
1. **Code Quality**: ESLint, Prettier, type checking
2. **Security Scanning**: Dependency vulnerability scanning
3. **Container Building**: Multi-stage Docker builds
4. **Registry Push**: ECR/Docker Hub image publishing
5. **Infrastructure Deployment**: Terraform apply
6. **Service Health Checks**: Automated validation

### Prerequisites for CI/CD

Before setting up the deployment pipeline, ensure you have:

#### 1. GitHub Repository Access
- Repository with admin access for GitHub Actions and Secrets
- GitHub Actions enabled for your organization/account

#### 2. Auth0 Tenants
- **Staging Auth0 tenant** (e.g., `nexpo-staging.auth0.com`)
- **Production Auth0 tenant** (e.g., `nexpo-prod.auth0.com`)
- Auth0 Deploy CLI application credentials for both tenants

#### 3. Kong Gateway Instances
- **Staging Kong Gateway** with Admin API access
- **Production Kong Gateway** with Admin API access
- Kong decK CLI compatible versions

#### 4. Cloud Provider Access
- Cloud account with appropriate permissions (ECS, VPC, LoadBalancer, etc.)
- CLI tools configured with appropriate IAM roles
- Container registries for all 8 backend services

### GitHub Secrets Configuration

Configure the following secrets in your GitHub repository settings:

#### Docker Registry Secrets
```bash
DOCKER_USERNAME=your-dockerhub-username
DOCKER_PASSWORD=your-dockerhub-token
```

#### Auth0 Secrets (Staging)
```bash
AUTH0_DOMAIN_STAGING=nexpo-staging.auth0.com
AUTH0_CLIENT_ID_STAGING=your-auth0-client-id
AUTH0_CLIENT_SECRET_STAGING=your-auth0-client-secret
```

#### Auth0 Secrets (Production)
```bash
AUTH0_DOMAIN_PRODUCTION=nexpo-prod.auth0.com
AUTH0_CLIENT_ID_PRODUCTION=your-auth0-client-id
AUTH0_CLIENT_SECRET_PRODUCTION=your-auth0-client-secret
```

#### Kong Gateway Secrets (Staging)
```bash
KONG_ADMIN_URL_STAGING=https://admin-api-staging.kong.com
KONG_ADMIN_TOKEN_STAGING=your-kong-admin-token
```

#### Kong Gateway Secrets (Production)
```bash
KONG_ADMIN_URL_PRODUCTION=https://admin-api-production.kong.com
KONG_ADMIN_TOKEN_PRODUCTION=your-kong-admin-token
```

#### AWS Secrets
```bash
AWS_ACCESS_KEY_ID=your-aws-access-key
AWS_SECRET_ACCESS_KEY=your-aws-secret-key
AWS_REGION=us-east-1
```

## ☁️ Supported Cloud Providers

### Core Cloud Infrastructure

#### Amazon Web Services (AWS)
- **Enable Flag**: `ENABLE_AWS=true`
- **Modules**: VPC, ECS Fargate, RDS, S3, CloudFront, Lambda, API Gateway
- **Auth0 Integration**: Cognito user pool import, Lambda authorizers, RDS IAM auth
- **Database Services**:
  - **RDS PostgreSQL**: Multi-AZ with Auth0 user mapping
  - **DynamoDB**: NoSQL with fine-grained access control
  - **DocumentDB**: MongoDB-compatible with IAM authentication
  - **ElastiCache**: Redis with Auth0 session storage

#### Google Cloud Platform (GCP)
- **Enable Flag**: `ENABLE_GCP=true`
- **Modules**: VPC, Compute Engine, Cloud Run, Firebase, Cloud SQL, Cloud Storage, Cloud Functions
- **Auth0 Integration**: Cloud IAM with Auth0 JWT validation, Firebase custom tokens
- **Database Services**:
  - **Cloud SQL**: PostgreSQL, MySQL, SQL Server with Cloud IAM integration
  - **Firestore**: NoSQL document database with security rules
  - **Firebase Realtime Database**: Real-time synchronization with user-based rules
  - **Cloud Bigtable**: Wide-column store with IAM policies

#### Microsoft Azure
- **Enable Flag**: `ENABLE_AZURE=true`
- **Modules**: Virtual Network, Container Instances, App Service, Cosmos DB, Storage
- **Auth0 Integration**: Azure AD B2C integration, managed identity for services
- **Database Services**:
  - **Azure Database for PostgreSQL**: Flexible server with Azure AD authentication
  - **Cosmos DB**: Multi-model database with RBAC
  - **Azure SQL Database**: Managed SQL with Azure AD integration
  - **Azure Cache for Redis**: In-memory cache with Auth0 session support

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

## 📊 Monitoring and Troubleshooting

### Health Endpoints

Each backend exposes health check endpoints:
- `GET /health` or `GET /` - Basic health check
- `GET /api/health/detailed` - Detailed health information
- `GET /metrics` - Prometheus metrics (where available)

### Monitoring Integration

**Kong Monitoring**:
- Prometheus metrics collection
- Request/response logging
- Performance metrics

**Cloud Provider Monitoring**:
- AWS CloudWatch: ECS task health and performance
- GCP Stackdriver: Application logs aggregation  
- Azure Monitor: Custom metric dashboards

**Auth0 Monitoring**:
- Authentication success/failure rates
- User login patterns
- Security event logging

### Common Issues

**Deployment Failures**:
1. Check GitHub Actions logs for specific error messages
2. Verify all required secrets are configured
3. Ensure Auth0 and Kong credentials are valid
4. Check cloud provider service events for deployment issues

**Service Health Issues**:
1. Review cloud provider logs for application errors
2. Check Kong gateway logs for routing issues
3. Verify Auth0 JWT validation is working
4. Test database and external service connectivity

**Configuration Issues**:
1. Validate Auth0 tenant.yaml syntax
2. Check Kong configuration file format
3. Ensure environment variables are properly set
4. Verify Docker images are building correctly

### Debugging Commands

```bash
# Check workflow status
gh workflow list

# View specific workflow run
gh run view <run-id>

# Check ECS service status (AWS)
aws ecs describe-services --cluster nexpo-cluster --services nexpo-typescript-api

# Test Kong gateway routing
curl -H "Authorization: Bearer <jwt-token>" https://api-staging.nexpo.com/api/v1/typescript/health

# Validate Auth0 configuration
a0deploy import --config .auth0/config.json --format yaml --output_folder .auth0

# Test Kong configuration
deck validate --state kong-staging.yaml
```

## 🔧 Common Operations

### Deploying to Multiple Providers

The GitHub Actions workflow automatically deploys only enabled providers:

```yaml
# Example: Deploy to AWS, HashiCorp Vault, and Heroku
ENABLE_AWS=true
ENABLE_HASHICORP_VAULT=true  
ENABLE_HEROKU=true

# All other providers remain disabled
ENABLE_GCP=false
ENABLE_AZURE=false
```

### Manual Workflow Generation

```bash
# Generate all workflows
node scripts/generate-workflows.js

# Generate specific backend workflow
node scripts/generate-workflows.js generate typescript

# List existing workflows
pnpm run workflows:list

# Remove specific workflow
pnpm run workflows:remove typescript
```

### Terraform Operations

```bash
# Generate Terraform configurations
node scripts/generate-terraform.js

# Initialize all providers
terraform init

# Plan multi-cloud deployment
terraform plan -var-file="aws.tfvars" -var-file="gcp.tfvars"

# Apply infrastructure
terraform apply

# Destroy specific provider
terraform destroy -target=module.aws
```

## 📄 Documentation Links

### Provider-Specific Documentation
- **[AWS Documentation](./providers/aws/README.md)** - ECS, RDS, Lambda, API Gateway setup
- **[GCP Documentation](./providers/gcp/README.md)** - Cloud Run, Firebase, Cloud SQL setup
- **[Azure Documentation](./providers/azure/README.md)** - Container Instances, Cosmos DB, App Service setup
- **[OCI Documentation](./providers/oci/README.md)** - Compute, Autonomous Database, Load Balancer setup
- **[IBM Documentation](./providers/ibm/README.md)** - VSI, Db2, Kubernetes Service setup
- **[DigitalOcean Documentation](./providers/digitalocean/README.md)** - App Platform, Databases, Spaces setup, Droplets
- **[Cloudflare Documentation](./providers/cloudflare/README.md)** - Workers, Pages, D1 Database, R2 Storage setup

### HashiCorp Stack
- **[HashiCorp Nomad Documentation](./providers/hashicorp/nomad/README.md)** - Container orchestration, job scheduling
- **[HashiCorp Vault Documentation](./providers/hashicorp/vault/README.md)** - Secrets management, dynamic credentials
- **[HashiCorp Consul Documentation](./providers/hashicorp/consul/README.md)** - Service discovery, service mesh

### Platform Documentation
- **[Heroku Documentation](./providers/heroku/README.md)** - PaaS deployment, add-ons, pipelines
- **[Auth0 Documentation](./providers/auth0/README.md)** - Identity management, tenant configuration

### Microservices

| **Service** | **Purpose** | **Technology Stack** |
|-------------|-------------|---------------------|
| **User** | User management, profiles | Node.js, Auth0, PostgreSQL |
| **Payments** | Subscription management | Node.js, Stripe/Qonversion |
| **Notifications** | Push/email/SMS | Node.js, Firebase, Twilio |
| **KPI Engine** | Analytics and reporting | Python, TimescaleDB |
| **Data Sync** | Third-party integrations | n8n/Zapier/Make |
| **API Gateway** | Mobile-optimized API | Node.js, gRPC |
| **Payments** | Subscription management | Node.js, Stripe/Qonversion |

## 🆘 Troubleshooting

1. **Provider Authentication**: Ensure CLI tools are configured
   - AWS: `aws configure`
   - GCP: `gcloud auth application-default login`
   - Azure: `az login`
   - OCI: `oci setup config`

2. **Feature Flag Issues**: Double-check environment variables
   - Verify `.env` file contains correct flags
   - Ensure GitHub secrets match environment requirements

3. **Resource Conflicts**: Check for existing resources
   - Use unique naming conventions
   - Clean up previous deployments if needed

4. **Network Connectivity**: Verify cloud provider networking
   - Check VPC/subnet configurations
   - Ensure security groups allow necessary traffic

## 🔄 Development Workflow

When adding new features:
1. Update the appropriate provider modules
2. Ensure compatibility across all environments
3. Update provider-specific documentation
4. Test in development environment first
5. Update feature flags in `.env.example`
6. Update GitHub Actions workflow if needed

## 📄 Outputs

After deployment, Terraform will output:

- Load balancer DNS name
- VPC ID
- ECS cluster name (AWS)
- Service names for enabled backends
- Database connection strings
- Auth0 configuration details
- Kong Gateway endpoints

## 🧹 Cleanup

To destroy all infrastructure:

```bash
terraform destroy
```

To destroy specific provider:

```bash
terraform destroy -target=module.aws
```

## 📄 Prerequisites

- **Terraform** >= 1.0
- **Cloud Provider CLIs** configured with appropriate permissions
- **Docker** for container builds
- **Node.js** >= 18 for workflow generation scripts
- **GitHub CLI** for workflow management
- **Auth0 CLI** for tenant management
- **Kong decK CLI** for API gateway configuration

## 📄 License

This infrastructure code follows the same license as the main project.

---

This comprehensive infrastructure setup provides enterprise-grade deployment automation for the Nexpo polyglot microservices architecture with integrated Auth0 authentication, Kong API gateway management, and multi-cloud support.
