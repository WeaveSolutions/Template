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
| **Next.js Frontend** | `3000` | HTTP | Web application | ✅ Public |
| **Electron Frontend** | `5173` | HTTP | Desktop application | ✅ Public |
| **Kong Gateway Proxy** | `8000` | HTTP/HTTPS | API Gateway (Primary Entry) | ✅ Public |
| **Kong Gateway Admin** | `8001` | HTTP | Gateway Administration | 🔒 Internal |
| **Backend API** | `8010` | HTTP | Main API Services | 🔒 Internal |
| **Express Server** | `8020` | HTTP | Utilities & PDF Service | 🔒 Internal |
| **MindsDB Gateway** | `4040` | HTTP | Unified Database Interface | 🔒 Internal |

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
│  │   Backend API   │    │ Express Server  │    │    Auth Service         │ │
│  │   Port 8010     │    │   Port 8020     │    │    Port 3001            │ │
│  │   (Internal)    │    │   (Internal)    │    │    (Internal)           │ │
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
MINDSDB_PORT=4040
MINDSDB_URL=http://localhost:4040

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
MINDSDB_PORT=4040
MINDSDB_URL=http://mindsdb-service:4040

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
    
  express-server:
    ports:
      - "8020:8020"
    
  # Database Gateway
  mindsdb:
    ports:
      - "4040:4040"
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
  port     = 4040
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
    - port: 4040
      targetPort: 4040
      name: mindsdb
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
docker run -p 4040:4040 mindsdb/mindsdb:v25.6.2.0

# Start Kong Gateway
docker run -p 8000:8000 -p 8001:8001 kong:latest

# Start Backend API
npm run dev:backend  # Port 8010

# Start Frontend
npm run dev  # Port 3000
```

### **2. Configure Databases via MindsDB:**
```bash
# Access MindsDB interface
curl http://localhost:4040

# Add databases programmatically
curl -X POST http://localhost:4040/api/databases \
  -d '{"name": "postgres_dev", "engine": "postgres", ...}'
```

### **3. Health Checks:**
```bash
# Kong Gateway
curl http://localhost:8000/health

# Backend API
curl http://localhost:8010/api/health

# MindsDB Gateway
curl http://localhost:4040/api/status

# Express Server
curl http://localhost:8020/health
```

## 🚨 **Troubleshooting**

### **Port Conflicts:**
```bash
# Check for port conflicts
netstat -tulpn | grep -E "(3000|8000|8001|8010|8020|4040)"

# Kill processes on specific ports
npx kill-port 8000 8001 8010 8020 4040
```

### **MindsDB Connection Issues:**
```bash
# Verify MindsDB is running
curl http://localhost:4040/api/status

# Check database connections
curl http://localhost:4040/api/databases

# Test database query
curl -X POST http://localhost:4040/api/sql/query \
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
Kong Gateway (8000) → Backend API (8010) → MindsDB Gateway (4040) → Databases
                   ↘ Express Server (8020) ↗
```

### **Environment Variables Updated:**
- ✅ **Removed**: Individual database ports (5432, 27017, 6379)
- ✅ **Added**: MindsDB gateway configuration (4040)
- ✅ **Updated**: API URLs to reflect Kong Gateway on 8000
- ✅ **Simplified**: Database connection strings point to MindsDB
