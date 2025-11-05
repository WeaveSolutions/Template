# Nexpo Polyglot API Backends

This document provides an overview of all implemented API backends in the Nexpo template, showcasing how the same API specification can be implemented across **8 different programming languages** and frameworks with **feature flag control** and **complete DevOps infrastructure**.

To start building microservices, create a Concept.md in the desired folder then create the functionality in the `microservices/api/api-[language]/src` directory.
To build for development use Docker for each API backend with your required programming language.
To deploy for staging and production use Terraform to deploy the enabled API backends to the cloud.

## 🏗️ Architecture Overview

All backends implement the same REST API specification with:
- 🔐 **Auth0 JWT authentication** with JWKS validation
- 📊 **MindsDB integration** for unified data access
- 📚 **Swagger/OpenAPI documentation** with interactive UI
- 🛡️ **Security best practices** (CORS, CSP, rate limiting, Helmet)
- 🐳 **Docker multi-stage builds** with non-root users
- 🎛️ **Feature flag control** for development and deployment
- 🚀 **GitHub Actions CI/CD** with security scanning
- 🏗️ **Terraform infrastructure** for AWS ECS deployment
- 📝 **Comprehensive documentation** and health checks

## 🎛️ Feature Flag Control

**Master switches** in `.env` enable/disable backends globally:
```bash
ENABLE_API_TYPESCRIPT=true   # TypeScript Express (Port 8020)
ENABLE_API_PYTHON=true       # Python FastAPI (Port 8030)
ENABLE_API_GO=true           # Go Beego (Port 8040)
ENABLE_API_RUST=true         # Rust Actix (Port 8050)
ENABLE_API_SCALA=true        # Scala Play (Port 8060)
ENABLE_API_JAVA=true         # Java Play (Port 8070)
ENABLE_API_R=true            # R Plumber (Port 8080)
ENABLE_API_JULIA=true        # Julia Genie (Port 8090)
```

## 🚀 Development Workflow

### Quick Start
```bash
# 1. Enable backends you want to develop
cp .env.example .env
# Edit .env to enable desired backends

# 2. Start only enabled backends with hot reload
pnpm dev:backend

# 3. Make changes - auto-restart on file save
# 4. Test at http://localhost:PORT/swagger
```

### Available Scripts
- `pnpm dev` - Start fullstack
- `pnpm dev:backend` - Start all enabled backends
- `pnpm dev:microservices` - Frontend + backends + MindsDB
- `pnpm workflows:generate` - Generate GitHub Actions
- `pnpm terraform:generate` - Generate AWS infrastructure

## 📊 Implemented Backends

### 1. TypeScript Express API (Port 8020) ✅
- **Location**: `microservices/api/api-typescript/`
- **Runtime**: Node.js 20 + TypeScript
- **Framework**: Express.js with middleware stack
- **Key Libraries**: express-openid-connect, winston, helmet, swagger-jsdoc
- **Special Features**: Session management, audit logging, CRA auth integration
- **Dev Command**: `pnpm dev` (tsx watch)
- **Swagger**: `/swagger`
- **Status**: ✅ Production Ready

### 2. Python FastAPI (Port 8030) ✅
- **Location**: `microservices/api/api-python/`
- **Runtime**: Python 3.11+
- **Framework**: FastAPI with async support
- **Key Libraries**: fastapi, python-jose, pydantic, structlog, httpx
- **Special Features**: Automatic API docs, type validation, async operations
- **Dev Command**: `uvicorn main:app --reload --port 8030`
- **Swagger**: `/docs` and `/redoc`
- **Status**: ✅ Production Ready

### 3. Go Beego API (Port 8040) ✅
- **Location**: `microservices/api/api-go/`
- **Runtime**: Go 1.21+
- **Framework**: Beego web framework
- **Key Libraries**: beego/v2, golang-jwt, swaggo
- **Special Features**: High performance, compiled binary, automatic routing
- **Dev Command**: `go run .` or `air` (with auto-reload)
- **Swagger**: `/swagger`
- **Status**: ✅ Production Ready

### 4. Rust Actix API (Port 8050) ✅
- **Location**: `microservices/api/api-rust/`
- **Runtime**: Rust stable
- **Framework**: Actix Web with async support
- **Key Libraries**: actix-web, jsonwebtoken, utoipa, reqwest
- **Special Features**: Memory safety, zero-cost abstractions, blazing performance
- **Dev Command**: `cargo run` or `cargo watch -x run`
- **Swagger**: `/swagger-ui`
- **Status**: ✅ Production Ready

### 5. Scala Play API (Port 8060) ✅
- **Location**: `microservices/api/api-scala/`
- **Runtime**: Scala 2.13 + JDK 17
- **Framework**: Play Framework with async actions
- **Key Libraries**: play-json, pdi-jwt, play-swagger
- **Special Features**: Functional programming, actor model, async operations
- **Dev Command**: `sbt run`
- **Swagger**: `/swagger.json`
- **Status**: ✅ Production Ready

### 6. Java Play API (Port 8070) ✅
- **Location**: `microservices/api/api-java/`
- **Runtime**: Java 17 + JDK
- **Framework**: Play Framework for Java
- **Key Libraries**: play-java, java-jwt, swagger-annotations
- **Special Features**: Enterprise-grade, strong typing, JVM ecosystem
- **Dev Command**: `sbt run`
- **Swagger**: `/swagger.json`
- **Status**: ✅ Production Ready

### 7. R Plumber API (Port 8080) ✅
- **Location**: `microservices/api/api-R/`
- **Runtime**: R 4.3+
- **Framework**: Plumber package for R APIs
- **Key Libraries**: plumber, jose, httr2, logger
- **Special Features**: Statistical computing, data science integration
- **Dev Command**: `Rscript app.R`
- **Swagger**: Auto-generated OpenAPI docs
- **Status**: ✅ Production Ready

### 8. Julia Genie API (Port 8090) ✅
- **Location**: `microservices/api/api-julia/`
- **Runtime**: Julia 1.9+
- **Framework**: Genie.jl web framework
- **Key Libraries**: Genie, JSONWebTokens, HTTP, JSON3
- **Special Features**: High-performance computing, scientific computing
- **Dev Command**: `julia app.jl`
- **Swagger**: Auto-generated API documentation
- **Status**: ✅ Production Ready

## API Endpoints

All backends implement these endpoints:

| Method | Path | Description | Auth |
|--------|------|-------------|------|
| GET | `/` | Service info | No |
| GET | `/health` | Health check | No |
| GET | `/swagger` or `/docs` | API documentation | No |
| POST | `/api/auth/token` | Token exchange | No |
| GET | `/api/user/profile` | User profile | Yes |
| GET | `/api/user/linked-accounts` | Linked accounts | Yes |
| POST | `/api/data/query` | MindsDB query | Yes |

## 🚀 Development & Deployment

### 💻 Local Development (Recommended)

**Use the smart backend manager with feature flags:**

```bash
# 1. Configure which backends to run
cp .env.example .env
# Edit .env - set ENABLE_API_TYPESCRIPT=true, etc.

# 2. Start only enabled backends (with hot reload)
pnpm dev:backend

# 3. Or start full stack (frontend + backends + MindsDB)
pnpm dev:microservices
```

**The backend manager will:**
- ✅ **Only start enabled backends** based on feature flags
- 🔄 **Auto-restart on file changes** (hot reload)
- 🎨 **Color-coded console output** for each backend
- ⚙️ **Handle graceful shutdown** with Ctrl+C

### 🐳 Docker Development (Individual Backends)

Each backend includes Docker support:

```bash
# Navigate to specific backend
cd microservices/api/api-typescript  # or any backend

# Copy and configure environment
cp .env.example .env
# Edit .env with Auth0 and MindsDB settings

# Build and run with Docker
docker build -t nexpo-api-typescript .
docker run -p 7000:7000 --env-file .env nexpo-api-typescript
```

### 🛠️ Runtime Requirements by Backend

| Backend | Runtime | Package Manager | Hot Reload |
|---------|---------|-----------------|------------|
| TypeScript | Node.js 20+ | pnpm | tsx watch |
| Python | Python 3.11+ | pip/poetry | uvicorn --reload |
| Go | Go 1.21+ | go modules | air (optional) |
| Rust | Rust stable | cargo | cargo watch |
| Scala | JDK 17+ | sbt | sbt ~run |
| Java | JDK 17+ | sbt | sbt ~run |
| R | R 4.3+ | CRAN | Manual restart |
| Julia | Julia 1.9+ | Pkg | Manual restart |

### 🎭 Manual Backend Development
pnpm start          # Node.js
python main.py     # Python
go run main.go     # Go
cargo run          # Rust
sbt run           # Scala/Java
```

## Environment Variables

All backends use similar environment variables:

```env
# Server configuration
{LANGUAGE}_API_PORT=80XX
APPLICATION_SECRET=your-secret-key

# Auth0 configuration
AUTH0_DOMAIN=your-domain.auth0.com
AUTH0_AUDIENCE=https://api.nexpo.com
AUTH0_CLIENT_ID=your-client-id
AUTH0_CLIENT_SECRET=your-client-secret

# MindsDB configuration
MINDSDB_URL=http://localhost:47334
MINDSDB_TIMEOUT=30s

# CORS configuration
CORS_ORIGINS=http://localhost:3000

# Logging
LOG_LEVEL=INFO
```

## 🚀 DevOps & CI/CD

### 🤖 GitHub Actions Workflows

**Auto-generate CI/CD pipelines for all backends:**

```bash
# Generate GitHub Actions workflows
pnpm workflows:generate

# List generated workflows
pnpm workflows:list

# Remove specific workflow
pnpm workflows:remove typescript
```

**Each workflow includes:**
- 💵 **Runtime-specific setup** and caching
- 🚀 **Build and test** steps
- 🛡️ **Security scanning** with CodeQL
- 🐳 **Docker build and push** to registry
- 🎭 **Deployment** to staging and production
- 🎛️ **Feature flag integration** (only enabled backends deploy)

### 🏗️ Infrastructure as Code (Terraform)

**Generate complete AWS infrastructure:**

```bash
# Generate Terraform files
pnpm terraform:generate

cd terraform/
# Configure which backends to deploy
cp terraform.tfvars.example terraform.tfvars
# Edit terraform.tfvars to enable desired backends

# Deploy infrastructure
terraform init
terraform plan
terraform apply
```

**Generated AWS Resources:**
- 🌍 **VPC with public subnets** and internet gateway
- 📺 **Application Load Balancer** with SSL termination
- 🐳 **ECS Fargate cluster** for containerized backends
- 🛡️ **Security groups** with least-privilege access
- 📊 **CloudWatch logging** for centralized monitoring
- 🎛️ **Feature flag variables** to enable/disable backends
- 📦 **Modular design** for easy backend addition/removal

### 🔄 Deployment Strategy

1. **Feature Flags Control Everything:**
   - Development: `.env` file
   - CI/CD: GitHub Actions workflows
   - Production: Terraform variables

2. **Polyglot Backend Pipeline:**
   ```
   Code Push → GitHub Actions → Build/Test/Scan → Docker Registry → ECS Deployment
   ```

3. **Zero-Downtime Deployments:**
   - ECS rolling updates
   - Health check validation
   - Automatic rollback on failure

## 🧪 Testing the APIs

### Health Check
```bash
curl http://localhost:80XX/health
```

### Get Access Token
First, obtain a JWT token from Auth0, then:

### User Profile
```bash
curl http://localhost:80XX/api/user/profile \
  -H "Authorization: Bearer <token>"
```

### MindsDB Query
```bash
curl -X POST http://localhost:80XX/api/data/query \
  -H "Authorization: Bearer <token>" \
  -H "Content-Type: application/json" \
  -d '{"query": "SELECT * FROM users LIMIT 10"}'
```

## 🏆 Architecture Benefits

This polyglot approach with feature flag control demonstrates:

1. **🌍 Language Diversity**: Same API specification across **8 different languages**
2. **🎛️ Smart Control**: Feature flags enable/disable backends in dev, CI/CD, and production
3. **🚀 Performance Spectrum**: From blazing Rust/Go to data-science R/Julia
4. **👥 Team Flexibility**: Teams can use their preferred language and framework
5. **📦 Microservices Ready**: Each backend deploys independently with auto-scaling
6. **🛠️ DevOps First**: Complete CI/CD and infrastructure automation
7. **🔄 Hot Reloading**: Most backends support live development with file watching
8. **📋 Documentation**: Swagger/OpenAPI docs auto-generated for all backends

## 🗡️ Quick Reference

| Need | Recommended Backend | Reason |
|------|-------------------|--------|
| **Web APIs** | TypeScript, Python | Great ecosystem, fast development |
| **High Performance** | Rust, Go | Memory safety, compiled speed |
| **Enterprise** | Java, Scala | JVM ecosystem, battle-tested |
| **Data Science** | R, Julia | Statistical computing, ML integration |
| **Rapid Prototyping** | Python, TypeScript | Quick iteration, extensive libraries |
| **Systems Programming** | Rust, Go | Memory control, concurrent processing |

## 🚀 Next Steps

### 👶 Getting Started
1. **🎛️ Configure Feature Flags**: `cp .env.example .env` and enable desired backends
2. **🚀 Start Development**: `pnpm dev:backend` for hot-reload development
3. **🔍 Test APIs**: Visit `http://localhost:PORT/swagger` for each enabled backend

### 🎨 Customization
4. **🔐 Configure Auth0**: Set up JWT authentication with your Auth0 tenant
5. **📊 Set Up MindsDB**: Install MindsDB for unified data access
6. **🐳 Containerize**: Use provided Dockerfiles for each backend

### 🏭 Production Deployment
7. **🤖 Generate CI/CD**: `pnpm workflows:generate` for GitHub Actions
8. **🏗️ Deploy Infrastructure**: `pnpm terraform:generate` for AWS ECS
9. **📋 Monitor**: Add observability (CloudWatch, Prometheus, Grafana)

### 🔍 Advanced Features
10. **📡 API Gateway**: Route requests to different backends based on load
11. **🔄 A/B Testing**: Use feature flags to test backend performance
12. **📈 Analytics**: Compare language/framework performance metrics

## Contributing

To add a new backend:

1. Create directory: `microservices/api/api-{language}/`
2. Implement the API specification
3. Add Swagger/OpenAPI documentation
4. Create Dockerfile and docker-compose.yml
5. Write comprehensive README
6. Add to this summary document

## License

All backends are MIT licensed and part of the Nexpo template project.
