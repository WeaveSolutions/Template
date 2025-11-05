# Port Configuration Guide

This document outlines the standardized port allocation scheme for the Nexpo template, implementing a **MindsDB-centric database architecture** for simplified database access and management.

## 🎯 **MindsDB-Centric Architecture Overview**

Instead of exposing individual database ports, Nexpo uses **MindsDB as a unified database gateway** on port `4040`. This approach:

- ✅ **Eliminates Port Conflicts**: No need to manage multiple database ports
- ✅ **Simplifies Configuration**: Single connection point for all databases
- ✅ **Unified Interface**: SQL access to PostgreSQL, MongoDB, Firebase, Supabase
- ✅ **AI/ML Ready**: Built-in machine learning capabilities
- ✅ **Security Enhanced**: Internal database connections only

## 📋 **Port Allocation Table**

| Service | Port | Protocol | Purpose | External Access |
|---------|------|----------|---------|----------------|
| **Next.js Frontend** | `3000` | HTTP | Web application + Tauri Desktop (Primary) | ✅ Public |
| **Tauri Desktop (Svelte)** | `1420` | HTTP | Svelte Standalone Mode (Cross-Platform) | ✅ Public |
| **Tauri Mobile** | `19000` | HTTP | Mobile Development (iOS/Android) | ✅ Public |
| **Kong Gateway Proxy** | `8000` | HTTP/HTTPS | API Gateway (Primary Entry) | ✅ Public |
| **Kong Gateway Admin** | `8001` | HTTP | Gateway Administration | 🔒 Internal |
| **TypeScript Express API** | `7000` | HTTP | Express Backend Services | 🔒 Internal |
| **Python FastAPI** | `7010` | HTTP | FastAPI Backend (Python) | 🔒 Internal |
| **Go Beego API** | `7020` | HTTP | API Backend (Go) | 🔒 Internal |
| **Rust Actix API** | `7030` | HTTP | API Backend (Rust) | 🔒 Internal |
| **Scala Play API** | `7040` | HTTP | API Backend (Scala) | 🔒 Internal |
| **Java Play API** | `7050` | HTTP | API Backend (Java) | 🔒 Internal |
| **R Plumber API** | `7060` | HTTP | API Backend (R) | 🔒 Internal |
| **Julia Genie API** | `7070` | HTTP | API Backend (Julia) | 🔒 Internal |
| **PHP Laravel API** | `7080` | HTTP | API Backend (PHP) | 🔒 Internal |
| **C++ Drogon API** | `7090` | HTTP | API Backend (C++) | 🔒 Internal |
| **MindsDB Gateway (HTTP)** | `47335` | HTTP | Unified Database Interface | 🔒 Internal |
| **MindsDB MySQL API** | `47336` | MySQL | MySQL Protocol Access | 🔒 Internal |
| **MindsDB MongoDB API** | `47337` | MongoDB | MongoDB Protocol Access | 🔒 Internal |

## 🌐 **Polyglot API Backends**

The Nexpo template includes 10 agnostic API backends implementing the same API specification across different programming languages and frameworks:

### **Available Backends:**
- **Node.js Express** (Port 7000) - Production auth microservice with TypeScript
- **Python FastAPI** (Port 7010) - Async Python framework with automatic OpenAPI docs
- **Go Beego** (Port 7020) - High-performance Go framework with MVC architecture
- **Rust Actix** (Port 7030) - Ultra-fast Rust framework with actor model
- **Scala Play** (Port 7040) - Reactive framework for JVM with async capabilities
- **Java Play** (Port 7050) - Enterprise Java framework with Spring-like features
- **R Plumber** (Port 7060) - Statistical computing APIs with R language for data-intensive APIs
- **Julia Genie** (Port 7070) - High-performance scientific computing framework
- **PHP Laravel** (Port 7080) - Modern PHP framework with Eloquent ORM and robust ecosystem
- **C++ Drogon** (Port 7090) - High-performance C++ framework for modern web

### **Common Features:**
- ✅ Auth0 JWT validation with JWKS
- ✅ MindsDB query proxy endpoints
- ✅ Swagger/OpenAPI documentation
- ✅ CORS and security headers
- ✅ Docker containerization
- ✅ Health check endpoints
- ✅ Structured logging

### **Access Points:**
- **Swagger UI**: Available at `/swagger`, `/docs`, or `/swagger-ui` on each backend
- **Health Check**: Available at `/health` on each backend
- **API Docs**: OpenAPI specs at `/swagger.json` or `/openapi.json`

## 🏗️ **Architecture Diagram**

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           NEXPO ARCHITECTURE                               │
│                        (MindsDB-Centric Database)                          │
└─────────────────────────────────────────────────────────────────────────────┘

    ┌─────────────┐
    │   Client    │
    │ (Browser/   │
    │    App)     │
    └──────┬──────┘
           │ HTTP/HTTPS
           ▼
    ┌─────────────┐
    │  Next.js    │  Port 3000
    │  Frontend   │  (Public)
    └──────┬──────┘
           │ API Calls
           ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                        Kong API Gateway                                    │
│  ┌─────────────────┐                    ┌─────────────────────────────────┐ │
│  │  Kong Proxy     │  Port 8000         │     Kong Admin                  │ │
│  │  (Public Entry) │  (Primary)         │     Port 8001                   │ │
│  │                 │                    │     (Internal)                  │ │
│  └─────────────────┘                    └─────────────────────────────────┘ │
└─────────────────┬───────────────────────────────────────────────────────────┘
                  │ JWT Validation & Routing
                  ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                         Backend Services                                   │
│  ┌─────────────────┐    ┌─────────────────┐    ┌─────────────────────────┐ │
│  │  Backend API    │    │  Express Server │    │  Polyglot API Backends  │ │
│  │  Port 8010      │    │  Port 8020      │    │  Ports 8030-8090        │ │
│  │  (Core API)     │    │  (Utils/PDF)    │    │  (Python/Go/Rust/       │ │
│  │                 │    │                 │    │   Scala/Java/R/Julia)   │ │
│  └─────────────────┘    └─────────────────┘    └─────────────────────────┘ │
└─────────────────┬───────────────────────────────────────────────────────────┘
                  │ Database Queries
                  ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                      MindsDB Gateway (Port 4040)                           │
│                     Unified Database Interface                             │
├─────────────────────────────────────────────────────────────────────────────┤
│  Internal Database Connections (No External Ports):                        │
│  ┌───────────────┐ ┌──────────────┐ ┌──────────────┐ ┌─────────────────┐   │
│  │  PostgreSQL   │ │   MongoDB    │ │   Firebase   │ │    Supabase     │   │
│  │  (internal)   │ │  (internal)  │ │    (API)     │ │     (API)       │   │
│  └───────────────┘ └──────────────┘ └──────────────┘ └─────────────────┘   │
└─────────────────────────────────────────────────────────────────────────────┘
```

## 🔧 **Environment Configuration**

### **Development Environment:**
```bash
# Core Services
NEXT_PUBLIC_API_URL=http://localhost:8010/api
API_URL=http://localhost:8010
EXPRESS_PORT=8020

# Kong Gateway
KONG_PROXY_LISTEN=0.0.0.0:8000, 0.0.0.0:8443 ssl
KONG_ADMIN_LISTEN=0.0.0.0:8001

# MindsDB Gateway (Unified Database Access)
MINDSDB_HOST=localhost
MINDSDB_PORT=47334
MINDSDB_HTTP_PORT=47335
MINDSDB_MYSQL_PORT=47336
MINDSDB_MONGODB_PORT=47337
MINDSDB_URL=http://localhost:47335

# Internal Database Connections (No External Ports)
POSTGRES_HOST=localhost
POSTGRES_DATABASE=nexpo
MONGODB_URI=mongodb://localhost:27017/nexpo
FIREBASE_PROJECT_ID=nexpo-project
SUPABASE_URL=https://your-project.supabase.co
```

### **Production Environment:**
```bash
# Core Services (HTTPS Only)
NEXT_PUBLIC_API_URL=https://api.nexpo.com/api
API_URL=https://api.nexpo.com
EXPRESS_PORT=8020

# Kong Gateway (Production)
KONG_PROXY_LISTEN=0.0.0.0:443 ssl
KONG_ADMIN_LISTEN=127.0.0.1:8001

# MindsDB Gateway (Internal)
MINDSDB_HOST=mindsdb-service
MINDSDB_PORT=47334
MINDSDB_HTTP_PORT=47335
MINDSDB_URL=http://mindsdb-service:47335

# Database Connections (Internal/Managed)
POSTGRES_HOST=rds-postgres-cluster
MONGODB_URI=mongodb+srv://cluster.mongodb.net/nexpo
FIREBASE_PROJECT_ID=nexpo-production
SUPABASE_URL=https://prod-project.supabase.co
```

## 📊 **Database Access Pattern**

### **Traditional Multi-Port Approach (Removed):**
```bash
# ❌ OLD: Multiple database ports to manage
PostgreSQL: 5432
MongoDB: 27017
Redis: 6379
MySQL: 3306
# Problems: Port conflicts, complex firewall rules, security exposure
```

### **MindsDB Gateway Approach (Current):**
```sql
-- ✅ NEW: Unified SQL interface through MindsDB
-- Configure databases via MindsDB interface:

CREATE DATABASE postgres_prod
WITH ENGINE = 'postgres',
PARAMETERS = {
    "host": "localhost",
    "port": 5432,
    "database": "nexpo_production",
    "user": "app_user",
    "password": "secure_password"
};

CREATE DATABASE mongo_analytics
WITH ENGINE = 'mongodb',
PARAMETERS = {
    "host": "mongodb://localhost:27017",
    "database": "analytics"
};

-- Query any database through unified interface:
SELECT * FROM postgres_prod.users WHERE active = true;
SELECT * FROM mongo_analytics.events WHERE date > '2024-01-01';

-- AI/ML operations:
SELECT user_id, PREDICT(churn_probability) 
FROM postgres_prod.users 
WHERE last_login < '2024-01-01';
```

## 🐳 **Docker Compose Configuration**

```yaml
version: '3.8'
services:
  # Frontend
  nextjs:
    ports:
      - "3000:3000"
    
  # API Gateway
  kong:
    ports:
      - "8000:8000"    # Proxy
      - "8001:8001"    # Admin
      - "8443:8443"    # SSL Proxy
    
  # Backend Services
  backend-api:
    ports:
      - "8010:8010"
    
  express:
    ports:
      - "8020:8020"
    environment:
      EXPRESS_PORT: 8020
      NODE_ENV: development
  
  # Polyglot API Backends (Optional - Choose One or More)
  api-python:
    ports:
      - "8030:8030"
    environment:
      PYTHON_API_PORT: 8030
      
  api-go:
    ports:
      - "8040:8040"
    environment:
      GO_API_PORT: 8040
      
  api-rust:
    ports:
      - "8050:8050"
    environment:
      RUST_API_PORT: 8050
      
  api-scala:
    ports:
      - "8060:8060"
    environment:
      SCALA_API_PORT: 8060
      
  api-java:
    ports:
      - "8070:8070"
    environment:
      JAVA_API_PORT: 8070
      
  api-r:
    ports:
      - "8080:8080"
    environment:
      R_API_PORT: 8080
      
  api-julia:
    ports:
      - "8090:8090"
    environment:
      JULIA_API_PORT: 8090
    
  # Database Gateway
  mindsdb:
    ports:
      - "47335:47335"   # HTTP API
      - "47336:47336"   # MySQL API
      - "47337:47337"   # MongoDB API
    volumes:
      - mindsdb_data:/app/storage
    image: mindsdb/mindsdb:v25.6.2.0
    
  # Internal Databases (No External Ports)
  postgres:
    # No external port mapping - accessed via MindsDB
    environment:
      POSTGRES_DB: nexpo
      POSTGRES_USER: postgres
      POSTGRES_PASSWORD: password
    
  mongodb:
    # No external port mapping - accessed via MindsDB
    environment:
      MONGO_INITDB_DATABASE: nexpo
```

## ☁️ **Cloud Deployment Examples**

### **Terraform Configuration:**
```hcl
# Target Groups (Simplified)
resource "aws_lb_target_group" "kong_proxy" {
  name     = "kong-proxy"
  port     = 8000
  protocol = "HTTP"
  vpc_id   = var.vpc_id
}

resource "aws_lb_target_group" "backend_api" {
  name     = "backend-api"
  port     = 8010
  protocol = "HTTP"
  vpc_id   = var.vpc_id
}

resource "aws_lb_target_group" "mindsdb" {
  name     = "mindsdb-gateway"
  port     = 47335
  protocol = "HTTP"
  vpc_id   = var.vpc_id
  
  # Internal access only
  target_type = "ip"
}
```

### **Kubernetes Service Configuration:**
```yaml
apiVersion: v1
kind: Service
metadata:
  name: kong-proxy
spec:
  type: LoadBalancer
  ports:
    - port: 80
      targetPort: 8000
      name: http
    - port: 443
      targetPort: 8443
      name: https
---
apiVersion: v1
kind: Service
metadata:
  name: mindsdb-gateway
spec:
  type: ClusterIP  # Internal only
  ports:
    - port: 47335
      targetPort: 47335
      name: mindsdb-http
    - port: 47336
      targetPort: 47336
      name: mindsdb-mysql
    - port: 47337
      targetPort: 47337
      name: mindsdb-mongodb
```

## 🔒 **Security Considerations**

### **Network Security:**
- **Public Access**: Only Kong Gateway (8000) and Next.js (3000)
- **Internal Services**: Backend API, Express Server, MindsDB
- **Database Security**: No direct database exposure
- **Firewall Rules**: Simplified port management

### **Database Security:**
```bash
# MindsDB handles all database authentication
MINDSDB_USER=secure_user
MINDSDB_PASSWORD=complex_password

# Individual database credentials are internal to MindsDB
POSTGRES_INTERNAL_USER=internal_user
MONGODB_INTERNAL_AUTH=internal_auth
```

## 🧪 **Development Workflow**

### **1. Start Core Services:**
```bash
# Start MindsDB Gateway
docker run -p 47335:47335 -p 47336:47336 -p 47337:47337 mindsdb/mindsdb:v25.6.2.0

# Or use the project script
pnpm start:mindsdb

# Start Kong Gateway
docker run -p 8000:8000 -p 8001:8001 kong:latest

# Start Backend API
pnpm run dev:backend  # Port 7000

# Start Frontend
pnpm run dev  # Port 3000
```

### **2. Configure Databases via MindsDB:**
```bash
# Access MindsDB HTTP interface
curl http://localhost:47335

# Add databases programmatically
curl -X POST http://localhost:47335/api/databases \
  -d '{"name": "postgres_dev", "engine": "postgres", ...}'
```

### **3. Health Checks:**
```bash
# Kong Gateway
curl http://localhost:8000/health

# Backend API
curl http://localhost:8010/api/health

# Express Server
curl http://localhost:8020/health

# Polyglot API Backends
curl http://localhost:8030/health  # Python FastAPI
curl http://localhost:8040/health  # Go Beego
curl http://localhost:8050/health  # Rust Actix
curl http://localhost:8060/health  # Scala Play
curl http://localhost:8070/health  # Java Play
curl http://localhost:8080/health  # R Plumber
curl http://localhost:8090/health  # Julia Genie
curl http://localhost:8100/health  # PHP Laravel

# MindsDB Gateway
curl http://localhost:47335/api/status
```

## 🚨 **Troubleshooting**

### **Port Conflicts:**
```bash
# Check for port conflicts
netstat -tulpn | grep -E "(3000|7000|8000|8001|8010|8020|47334|47335|47336|47337)"

# Kill processes on specific ports
npx kill-port 3000 7000 8000 8001 8010 47335 47336 47337
```

### **MindsDB Connection Issues:**
```bash
# Verify MindsDB is running
curl http://localhost:47335/api/status

# Check database connections
curl http://localhost:47335/api/databases

# Test database query
curl -X POST http://localhost:47335/api/sql/query \
  -d '{"query": "SELECT 1"}'
```

### **Kong Gateway Issues:**
```bash
# Check Kong admin API
curl http://localhost:8001/

# List configured services
curl http://localhost:8001/services

# Check proxy health
curl http://localhost:8000/health
```

## 📝 **Notes**

### **Migration from Multi-Port to MindsDB:**
1. **Before**: Each database required separate port configuration
2. **After**: Single MindsDB gateway handles all database access
3. **Benefits**: Reduced complexity, better security, unified interface
4. **AI/ML**: Built-in machine learning capabilities through MindsDB

### **Service Dependencies:**
```
Kong Gateway (8000) → Backend API (7000) → MindsDB Gateway (47335) → Databases
                   ↘ Express Server (8020) ↗
```

### **Environment Variables Updated:**
- ✅ **Removed**: Individual database ports (5432, 27017, 6379)
- ✅ **Added**: MindsDB gateway configuration (47335 HTTP, 47336 MySQL, 47337 MongoDB)
- ✅ **Updated**: API URLs to reflect Kong Gateway on 8000
- ✅ **Simplified**: Database connection strings point to MindsDB
