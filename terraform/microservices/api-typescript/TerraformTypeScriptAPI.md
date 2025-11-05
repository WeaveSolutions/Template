# TypeScript API Service Infrastructure

This directory contains the Terraform configuration for deploying the TypeScript API service, which provides core business logic and API endpoints for the Nexpo platform.

## Overview
This module manages the TypeScript API service infrastructure for the Nexpo platform. It provisions the necessary AWS resources including ECS Fargate tasks, Application Load Balancer integration, Auto Scaling, CloudWatch logging, and PostHog analytics integration.

## 🚀 Service Overview

The TypeScript API Service provides:
- **Core REST API endpoints** for the Nexpo platform
- **Auth0 JWT validation** for secure API access
- **Database integration** with PostgreSQL and Redis
- **Auto-scaling** based on CPU and memory metrics
- **Health checks** and monitoring
- **PostHog analytics** for usage tracking
- **Service discovery** integration
- **Load balancer** integration with path-based routing

## 🏗️ Architecture

- **Kong API Gateway**: Primary API gateway with caching, rate limiting, and JWT validation
- **ECS Fargate**: Containerized TypeScript API service running on AWS Fargate
- **Application Load Balancer**: Routes traffic to Kong and backend services
- **Kong Proxy Cache**: Response caching for improved performance
- **Auto Scaling**: Automatically scales based on CPU and memory utilization
- **CloudWatch**: Centralized logging and monitoring
- **Service Discovery**: AWS Cloud Map for service registration
- **PostHog**: Analytics and event tracking
- **Auth0**: JWT token validation through Kong

```
┌─────────────────┐     ┌──────────────────┐     ┌─────────────────┐
│  Mobile/Web App │────▶│ Kong API Gateway │────▶│ TypeScript API  │
└─────────────────┘     │ + Proxy Cache    │     │ Service (ECS)   │
                        └──────────────────┘     └─────────────────┘
                                │                        │
                                │                        │
                                ▼                        ▼
                        ┌─────────────┐          ┌─────────────┐
                        │ Rate Limits │          │ PostgreSQL  │
                        │ JWT Validation│        │ + Redis     │
                        │ Response Cache│        │ Database    │
                        └─────────────┘          └─────────────┘
```

## 📁 Directory Structure

```
api-typescript/
├── main.tf              # Main service configuration
├── variables.tf         # Service variables
├── outputs.tf          # Service outputs
├── TypeScriptAPI.md    # This documentation file
└── modules/            # Sub-modules
    └── posthog/        # PostHog analytics integration
        ├── main.tf
        ├── variables.tf
        ├── outputs.tf
        └── AuthAnalytics.md
```

## 🚀 Quick Start

### Prerequisites

1. **AWS Account** with appropriate permissions
2. **Terraform** >= 1.0 installed
3. **Docker image** for TypeScript API available
4. **Auth0 tenant** configured
5. **Database** (PostgreSQL) and **Redis** instances

### Deployment

1. **Configure variables** in `terraform.tfvars`:
```hcl
# Enable TypeScript API
enable_typescript_api = true

# Project configuration
project_name = "nexpo"
environment = "production"

# Container configuration
container_image = "nexpo/api-typescript:latest"
desired_count = 2
cpu = 512
memory = 1024

# Auth0 configuration
auth0_domain = "your-tenant.auth0.com"
auth0_audience = "https://api.nexpo.com"

# Database configuration
database_url = "postgresql://user:pass@host:5432/nexpo"
redis_url = "redis://host:6379"
```

2. **Deploy the infrastructure**:
```bash
terraform init
terraform plan
terraform apply
```

### Configuration Examples

#### Basic Configuration
```hcl
module "typescript_api" {
  source = "./microservices/api/api-typescript"
  
  enable_typescript_api = true
  project_name = "nexpo"
  environment = "production"
  
  # Network configuration
  vpc_id = module.vpc.vpc_id
  subnet_ids = module.vpc.private_subnet_ids
  security_group_ids = [module.security_groups.backend_sg_id]
  
  # ECS configuration
  cluster_id = module.ecs.cluster_id
  cluster_name = module.ecs.cluster_name
  
  # Load balancer configuration
  listener_arn = module.alb.listener_arn
  
  # Service discovery
  service_discovery_namespace_id = module.service_discovery.namespace_id
  
  # Auth0 configuration
  auth0_domain = var.auth0_domain
  auth0_audience = var.auth0_audience
  auth0_client_secret_arn = module.secrets.auth0_client_secret_arn
  
  # Database configuration
  database_url = module.database.connection_string
  database_password_arn = module.secrets.database_password_arn
  redis_url = module.redis.connection_string
  
  # Secrets
  jwt_secret_arn = module.secrets.jwt_secret_arn
}
```

#### Production Configuration with Auto Scaling
```hcl
module "typescript_api" {
  source = "./microservices/api/api-typescript"
  
  # Service configuration
  enable_typescript_api = true
  project_name = "nexpo"
  environment = "production"
  
  # Container configuration
  container_image = "nexpo/api-typescript:v1.2.3"
  desired_count = 3
  cpu = 1024
  memory = 2048
  
  # Auto scaling configuration
  enable_auto_scaling = true
  min_capacity = 2
  max_capacity = 20
  cpu_target_value = 60
  memory_target_value = 70
  
  # Health check configuration
  health_check_path = "/health"
  health_check_interval = 30
  health_check_timeout = 5
  health_check_retries = 3
  
  # PostHog analytics
  enable_posthog = true
  posthog_organization_id = var.posthog_organization_id
  posthog_host = "https://app.posthog.com"
  
  # Additional configuration...
}
## 🔧 Features

### Core API Features
- **RESTful API endpoints** following OpenAPI 3.0 specification
- **JWT authentication** with Auth0 integration via Kong
- **Request validation** and error handling
- **Rate limiting** and throttling via Kong Gateway
- **CORS support** for web applications
- **API versioning** with `/api/v1/typescript/*` routing

### Kong Gateway Features
- **Proxy caching** for improved response times
- **JWT validation** at the gateway level
- **Rate limiting** per consumer/IP with Kong plugins
- **Request/response transformation**
- **Load balancing** across multiple backend instances
- **Circuit breaker** for fault tolerance
- **API analytics** and monitoring
- **Plugin ecosystem** for extensibility

### Infrastructure Features
- **Auto-scaling** based on CPU/memory metrics
- **Health checks** with graceful shutdowns
- **Blue-green deployments** via ECS rolling updates
- **Service discovery** with AWS Cloud Map
- **Kong clustering** for high availability
- **CloudWatch integration** for logging and monitoring

### Analytics & Monitoring
- **PostHog integration** for user behavior tracking
- **Performance monitoring** with custom metrics
- **Error tracking** and alerting
- **Request tracing** for debugging
- **Business intelligence** dashboards

## 📊 PostHog Analytics Integration

The TypeScript API includes comprehensive PostHog analytics for tracking:

### API Usage Analytics
- **Endpoint usage** tracking
- **Response time** monitoring
- **Error rate** tracking
- **User journey** analysis

### Business Metrics
- **Feature adoption** rates
- **User engagement** metrics
- **Performance** benchmarks
- **Custom events** tracking

### Configuration
```hcl
# Enable PostHog analytics
enable_posthog = true
posthog_organization_id = "your-org-id"
posthog_host = "https://app.posthog.com"

# Analytics settings
analytics_rollout_percentage = 100
sampling_rate = 1.0
data_retention_days = 30

# Performance settings
batch_size = 100
flush_interval_ms = 5000
```

## 🔐 Security Configuration

### Auth0 Integration
- **JWT token validation** on all API endpoints
- **Role-based access control** (RBAC)
- **Multi-tenant support** with organization scoping
- **API key authentication** for service-to-service calls

### Security Best Practices
- **Input validation** and sanitization
- **SQL injection** prevention
- **XSS protection** headers
- **Rate limiting** per user/IP
- **HTTPS enforcement**

## 📈 Performance Optimization

### Auto Scaling Configuration
```hcl
# Auto scaling settings
enable_auto_scaling = true
min_capacity = 2
max_capacity = 20
cpu_target_value = 70
memory_target_value = 80

# Health check settings
health_check_interval = 30
health_check_timeout = 5
health_check_retries = 3
```

### Resource Optimization
- **Containerized deployment** with Docker
- **Efficient memory usage** with Node.js optimization
- **Database connection pooling**
- **Redis caching** for frequently accessed data
- **CDN integration** for static assets

## 🌐 API Endpoints

The TypeScript API provides the following endpoint categories:

### Health & Status
- `GET /health` - Service health check
- `GET /health/detailed` - Detailed health information
- `GET /metrics` - Prometheus metrics
- `GET /status` - Service status and version

### Authentication
- `POST /auth/login` - User authentication
- `POST /auth/logout` - User logout
- `POST /auth/refresh` - Token refresh
- `GET /auth/profile` - User profile

### Core Business Logic
- `GET /api/v1/typescript/users` - User management
- `GET /api/v1/typescript/organizations` - Organization management
- `GET /api/v1/typescript/projects` - Project management
- `GET /api/v1/typescript/analytics` - Analytics endpoints

## 🔍 Monitoring & Troubleshooting

### CloudWatch Logs
- **Application logs** with structured logging
- **Error tracking** with stack traces
- **Performance metrics** and timing
- **Security events** logging

### Health Checks
```bash
# Check service health
curl -f http://localhost:7000/health

# Detailed health check
curl -f http://localhost:7000/health/detailed

# Prometheus metrics
curl -f http://localhost:7000/metrics
```

### Common Issues
1. **Container startup failures** - Check CloudWatch logs
2. **Database connection issues** - Verify security groups
3. **Auth0 token validation** - Check JWT configuration
4. **Load balancer health checks** - Verify target group settings

## 🚀 Deployment Guide

### Step 1: Infrastructure Setup
```bash
# Navigate to the TypeScript API directory
cd terraform/microservices/api-typescript

# Initialize Terraform
terraform init

# Review the deployment plan
terraform plan
```

### Step 2: Configure Variables
Create a `terraform.tfvars` file:
```hcl
# Service configuration
enable_typescript_api = true
project_name = "nexpo"
environment = "production"

# Container configuration
container_image = "nexpo/api-typescript:latest"
desired_count = 3
cpu = 512
memory = 1024

# Database configuration
database_url = "postgresql://user:pass@host:5432/nexpo"
redis_url = "redis://host:6379"

# Auth0 configuration
auth0_domain = "your-tenant.auth0.com"
auth0_audience = "https://api.nexpo.com"
```

### Step 3: Deploy
```bash
# Apply the infrastructure
terraform apply

# Verify deployment
aws ecs describe-services --cluster nexpo-cluster --services nexpo-typescript-api
```

### Step 4: Test API
```bash
# Test health endpoint
curl -f https://api.nexpo.com/api/v1/typescript/health

# Test authenticated endpoint
curl -H "Authorization: Bearer YOUR_JWT_TOKEN" \
     https://api.nexpo.com/api/v1/typescript/users

## 🔧 Environment Variables

The TypeScript API service uses the following environment variables:

### Required Variables

| Variable | Description | Example |
|----------|-------------|---------|
| `AUTH0_DOMAIN` | Auth0 tenant domain | `myapp.auth0.com` |
| `AUTH0_AUDIENCE` | Auth0 API identifier | `https://api.myapp.com` |
| `DATABASE_URL` | PostgreSQL connection string | `postgresql://...` |
| `REDIS_URL` | Redis connection string | `redis://...` |
| `KONG_ADMIN_URL` | Kong Admin API URL | `http://kong-admin:8001` |
| `NODE_ENV` | Environment mode | `production` |

### Optional Variables

| Variable | Description | Default |
|----------|-------------|---------|----------|
| `PORT` | Server port | `7000` |
| `LOG_LEVEL` | Logging level | `info` |
| `JWT_SECRET` | JWT signing secret | Auto-generated |
| `RATE_LIMIT_WINDOW` | Rate limit window (ms) | `900000` |
| `RATE_LIMIT_MAX` | Max requests per window | `100` |
| `HEALTH_CHECK_TIMEOUT` | Health check timeout | `5000` |
| `CORS_ORIGIN` | CORS allowed origins | `*` |
| `KONG_PROXY_CACHE_TTL` | Kong cache TTL (seconds) | `300` |
| `KONG_RATE_LIMIT_MINUTE` | Kong rate limit per minute | `100` |
| `KONG_RATE_LIMIT_HOUR` | Kong rate limit per hour | `1000` |
| `POSTHOG_API_KEY` | PostHog API key | Optional |
| `POSTHOG_HOST` | PostHog host URL | `https://app.posthog.com` |

## 🏗️ Infrastructure Resources

The Terraform configuration creates the following AWS resources:

### Core Resources
- **ECS Task Definition** - Container specification
- **ECS Service** - Service management and scaling
- **Target Group** - Load balancer target group
- **Listener Rule** - ALB routing rules
- **Service Discovery** - AWS Cloud Map service
- **CloudWatch Log Group** - Centralized logging

### IAM Resources
- **Execution Role** - ECS task execution permissions
- **Task Role** - Application runtime permissions
- **Policy Attachments** - Required AWS service permissions
}
Response: { "success": true, "user_id": "auth0|..." }

POST /auth/logout
Headers: { "Authorization": "Bearer <token>" }
Response: { "success": true, "logout_url": "..." }

POST /auth/refresh
Body: { "refresh_token": "..." }
Response: { "access_token": "...", "expires_in": 3600 }

POST /auth/forgot-password
Body: { "email": "user@example.com" }
Response: { "success": true, "message": "Password reset email sent" }
```

### Token Management

```
POST /auth/validate

## 🤝 Contributing

When making changes to the TypeScript API infrastructure:

1. **Update Variables** - Add new variables to `variables.tf`
2. **Update Outputs** - Add new outputs to `outputs.tf`
3. **Update Documentation** - Update this README file
4. **Test Changes** - Verify in development environment
5. **Update Main Module** - Update the main Terraform configuration

## 📝 License

This infrastructure configuration is part of the Nexpo template project.
