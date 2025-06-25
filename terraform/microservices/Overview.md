# Microservices Infrastructure

This directory contains Terraform configurations for deploying microservices that complement the Nexpo application. Each service is designed to be cloud-agnostic and can be deployed to any of the supported providers (AWS, GCP, Azure, OCI, IBM, Cloudflare).

## 📋 Services Overview

| Service | Purpose | Key Technologies | Status |
|---------|---------|------------------|--------|
| **[Auth](./auth)** | Auth0 integration, RBAC mapping, session tokens | Node.js, Express, Auth0 | 🟡 Planned |
| **[User](./user)** | User profile CRUD, preferences | Node.js, Supabase/Firebase | 🟡 Planned |
| **[KPI Engine](./kpi-engine)** | Business metrics calculation | Python, Apache Druid, TimescaleDB | 🟡 Planned |
| **[AI Advisor](./ai-advisor)** | OKR suggestions, goal recommendations | Python, OpenAI/Gemini APIs | 🟡 Planned |
| **[Notifications](./notifications)** | Email/SMS delivery, push notifications | Node.js, Brevo, FCM | 🟡 Planned |
| **[Reports](./reports)** | Report generation and scheduling | Python, Looker Studio API | 🟡 Planned |
| **[Data Sync](./data-sync)** | Third-party integrations | n8n | 🟡 Planned |
| **[API Gateway](./api-gateway)** | Mobile-optimized API, GraphQL | Node.js, Apollo, gRPC | 🟡 Planned |
| **[Payments](./payments)** | Subscription management | Node.js, Polar, Stripe, Qonversion | 🟡 Planned |

## 🏗️ Architecture Principles

### 1. **Cloud-Agnostic Design**
- Services use managed container platforms (ECS/Cloud Run/Container Apps)
- Databases use provider-managed services (RDS/Cloud SQL/Azure Database)
- Message queues use provider services (SQS/Pub-Sub/Service Bus)

### 2. **Service Communication**
- REST APIs for synchronous communication
- Message queues for asynchronous events
- gRPC for high-performance internal communication
- GraphQL gateway for mobile clients

### 3. **Security**
- Service-to-service authentication via mutual TLS
- API keys stored in secret managers
- Network isolation with service mesh (optional)
- Rate limiting and DDoS protection

### 4. **Scalability**
- Horizontal auto-scaling based on metrics
- Database read replicas for read-heavy services
- Caching layers (Redis/Memcached)
- CDN for static assets

## 🚀 Deployment Options

### Option 1: Monolithic Deployment
Deploy all services as a single application for simplicity:
```bash
cd microservices/monolith
terraform apply
```

### Option 2: Individual Services
Deploy services independently for better scalability:
```bash
cd microservices/auth
terraform apply

cd microservices/user
terraform apply
```

### Option 3: Service Groups
Deploy related services together:
```bash
# Core services (auth, user, notifications)
cd microservices/core
terraform apply

# Analytics services (kpi-engine, reports, ai-advisor)
cd microservices/analytics
terraform apply
```

## 📊 Service Details

### Auth Service
- **Purpose**: Centralized authentication and authorization
- **Features**: 
  - Auth0 integration
  - JWT token generation/validation
  - Role-based access control (RBAC)
  - Session management
- **Endpoints**: `/auth/login`, `/auth/logout`, `/auth/refresh`, `/auth/validate`

### User Service
- **Purpose**: User profile and preferences management
- **Features**:
  - User CRUD operations
  - Profile pictures via CDN
  - Preferences storage
  - Activity tracking
- **Database**: PostgreSQL with user_profiles, user_preferences tables

### KPI Engine
- **Purpose**: Calculate and aggregate business metrics
- **Features**:
  - Real-time metric calculation
  - Historical data aggregation
  - Custom KPI definitions
  - Scheduled reports
- **Technologies**: Apache Druid for OLAP, TimescaleDB for time-series

### AI Advisor
- **Purpose**: Provide intelligent recommendations
- **Features**:
  - OKR generation based on KPIs
  - Goal suggestions
  - Performance insights
  - Predictive analytics
- **APIs**: OpenAI GPT-4, Google Gemini, Anthropic Claude

### Notifications Service
- **Purpose**: Multi-channel communication
- **Features**:
  - Email via Brevo
  - SMS via Twilio
  - Push notifications via FCM/APNS
  - In-app notifications
- **Queue**: Provider message queue for reliability

### Reports Service
- **Purpose**: Generate and distribute reports
- **Features**:
  - Scheduled report generation
  - Multiple formats (PDF, Excel, CSV)
  - Email distribution
  - Looker Studio integration
- **Storage**: Object storage for generated reports

### Data Sync Service
- **Purpose**: Integrate with external systems
- **Features**:
  - Webhook endpoints
  - API polling
  - Data transformation
  - Error handling and retries
- **Platforms**: n8n (self-hosted), Zapier, Make.com

### API Gateway
- **Purpose**: Unified API for mobile clients
- **Features**:
  - GraphQL endpoint
  - Request aggregation
  - Response caching
  - Rate limiting
- **Protocols**: REST, GraphQL, gRPC

### Payments Service
- **Purpose**: Handle subscriptions and payments
- **Features**:
  - Stripe integration
  - Qonversion for mobile
  - Subscription management
  - Payment webhooks
- **Security**: PCI compliance considerations

## 🔧 Common Infrastructure

Each service shares common infrastructure components:

```hcl
# Shared resources created once per provider
module "shared" {
  source = "../shared"
  
  # Networking
  vpc_id     = var.vpc_id
  subnet_ids = var.subnet_ids
  
  # Security
  kms_key_id = var.kms_key_id
  
  # Monitoring
  log_group = var.log_group
}
```

## 📈 Monitoring and Observability

All services include:
- Structured logging to provider log service
- Distributed tracing with OpenTelemetry
- Custom metrics and dashboards
- Health check endpoints
- Performance monitoring

## 🔒 Security Considerations

1. **Network Security**
   - Services in private subnets
   - API Gateway as single entry point
   - Network policies for service isolation

2. **Data Security**
   - Encryption at rest and in transit
   - Sensitive data in secret managers
   - Regular security scanning

3. **Access Control**
   - Service accounts with minimal permissions
   - API key rotation
   - Audit logging

## 🚦 Getting Started

1. **Choose Services**: Decide which microservices you need
2. **Configure Providers**: Set up cloud provider credentials
3. **Deploy Shared Resources**: Create VPC, databases, queues
4. **Deploy Services**: Use Terraform to deploy selected services
5. **Configure Endpoints**: Update application with service URLs

## 📝 Development Guidelines

When developing new microservices:
1. Follow the existing directory structure
2. Include comprehensive README
3. Add health check endpoints
4. Implement structured logging
5. Include Terraform configurations
6. Add monitoring dashboards
7. Document API endpoints
8. Include example .env files

## 🆘 Troubleshooting

Common issues and solutions:
- **Service Discovery**: Use provider service mesh or Consul
- **Performance**: Enable caching and optimize queries
- **Costs**: Use auto-scaling and spot instances
- **Debugging**: Check centralized logs and traces
