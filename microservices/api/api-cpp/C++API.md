# C++ API Service (Drogon Framework)

A high-performance C++ REST API service built with the Drogon framework for the Nexpo platform.

## 🚀 Features

- **High Performance**: Built with C++ and Drogon framework for maximum performance
- **REST API**: Full REST API implementation with CRUD operations
- **Authentication**: JWT-based authentication with Auth0 integration
- **Kong Gateway**: API Gateway integration with caching and rate limiting
- **Health Checks**: Comprehensive health monitoring endpoints
- **CORS Support**: Cross-origin resource sharing for web applications
- **Auto-scaling**: ECS Fargate with auto-scaling capabilities
- **Logging**: Structured logging with CloudWatch integration
- **Monitoring**: PostHog analytics and CloudWatch metrics

## 🛠️ Tech Stack

- **Framework**: Drogon (C++ web framework)
- **Language**: C++17
- **Database**: PostgreSQL with connection pooling
- **Cache**: Redis for session storage
- **Authentication**: Auth0 JWT
- **API Gateway**: Kong
- **Container**: Docker
- **Orchestration**: AWS ECS Fargate
- **Monitoring**: CloudWatch + PostHog

## 📁 Project Structure

```
api-c++/
├── src/
│   └── main.cpp              # Main application entry point
├── include/                  # Header files
├── CMakeLists.txt           # CMake build configuration
├── config.json              # Drogon configuration
├── Dockerfile               # Container configuration
├── README.md                # This file
├── CppAPI.md               # Detailed documentation
├── main.tf                  # Terraform configuration
├── variables.tf             # Terraform variables
└── outputs.tf               # Terraform outputs
```

## 🔧 Prerequisites

- **C++ Compiler**: GCC 8+ or Clang 6+
- **CMake**: Version 3.16+
- **Drogon Framework**: Latest stable version
- **Docker**: For containerization
- **AWS CLI**: For deployment
- **Terraform**: For infrastructure provisioning

### Install Dependencies (Ubuntu/Debian)

```bash
sudo apt-get update
sudo apt-get install -y \
    build-essential \
    cmake \
    libssl-dev \
    zlib1g-dev \
    libbrotli-dev \
    libhiredis-dev \
    libpq-dev \
    libjsoncpp-dev \
    uuid-dev
```

### Install Drogon Framework

```bash
git clone https://github.com/drogonframework/drogon.git
cd drogon
mkdir build && cd build
cmake ..
make -j4
sudo make install
```

## 🏗️ Build & Run

### Local Development

1. **Clone and build**:
   ```bash
   mkdir build && cd build
   cmake ..
   make -j4
   ```

2. **Run the application**:
   ```bash
   ./cpp_api
   ```

3. **Test the API**:
   ```bash
   curl http://localhost:8110/health
   curl http://localhost:8110/api/v1/cpp/users
   ```

### Docker Build

```bash
docker build -t nexpo/api-cpp:latest .
docker run -p 8110:8110 nexpo/api-cpp:latest
```

## 📚 API Endpoints

### Health Check
- `GET /health` - Basic health status
- `GET /health/detailed` - Detailed health information

### Authentication
- `POST /api/v1/cpp/auth/login` - User login
- `POST /api/v1/cpp/auth/refresh` - Token refresh
- `POST /api/v1/cpp/auth/logout` - User logout

### Users
- `GET /api/v1/cpp/users` - List all users
- `POST /api/v1/cpp/users` - Create new user
- `GET /api/v1/cpp/users/{id}` - Get user by ID
- `PUT /api/v1/cpp/users/{id}` - Update user
- `DELETE /api/v1/cpp/users/{id}` - Delete user

### Example API Calls

```bash
# Health check
curl http://localhost:8100/health

# Get users
curl http://localhost:8100/api/v1/cpp/users

# Create user
curl -X POST http://localhost:8110/api/v1/cpp/users \
  -H "Content-Type: application/json" \
  -d '{"name": "John Doe", "email": "john@example.com"}'

# User login
curl -X POST http://localhost:8110/api/v1/cpp/auth/login \
  -H "Content-Type: application/json" \
  -d '{"email": "john@example.com", "password": "password"}'
```

## ⚙️ Configuration

### Environment Variables

```bash
# Server Configuration
PORT=8110
NODE_ENV=production
SERVICE_NAME=cpp-api

# Database
DATABASE_URL=postgresql://user:pass@host:5432/nexpo
REDIS_URL=redis://host:6379

# Authentication
AUTH0_DOMAIN=your-domain.auth0.com
AUTH0_AUDIENCE=https://api.nexpo.com
AUTH0_CLIENT_SECRET=your-client-secret
JWT_SECRET=your-jwt-secret

# Kong Gateway
KONG_ADMIN_URL=http://kong-admin:8001
KONG_PROXY_CACHE_TTL=300
KONG_RATE_LIMIT_MINUTE=100
KONG_RATE_LIMIT_HOUR=1000

# Drogon Framework
DROGON_THREAD_NUM=4
DROGON_LISTENER_COUNT=1
DROGON_ENABLE_COMPRESSION=true
DROGON_MAX_CONNECTIONS=1000
DROGON_DB_CONNECTION_NUMBER=10

# Analytics
POSTHOG_API_KEY=your-posthog-key
POSTHOG_HOST=https://app.posthog.com
```

### Drogon Configuration

The `config.json` file contains Drogon-specific configuration:

```json
{
  "app": {
    "threads_num": 4,
    "enable_session": true,
    "session_timeout": 3600,
    "max_connections": 1000,
    "enable_compression": true
  },
  "listeners": [
    {
      "address": "0.0.0.0",
      "port": 8110,
      "https": false
    }
  ],
  "db_clients": [
    {
      "name": "default",
      "rdbms": "postgresql",
      "host": "localhost",
      "port": 5432,
      "connection_number": 10
    }
  ]
}
```

## 🚀 Deployment

### Deploy with Terraform

1. **Configure variables**:
   ```bash
   cp terraform.tfvars.example terraform.tfvars
   # Edit terraform.tfvars
   ```

2. **Deploy infrastructure**:
   ```bash
   terraform init
   terraform plan
   terraform apply
   ```

3. **Verify deployment**:
   ```bash
   curl https://api.nexpo.com/api/v1/cpp/health
   ```

### Deploy with Docker

```bash
# Build and push to ECR
aws ecr get-login-password --region us-east-1 | docker login --username AWS --password-stdin <account>.dkr.ecr.us-east-1.amazonaws.com

docker build -t nexpo/api-cpp:latest .
docker tag nexpo/api-cpp:latest <account>.dkr.ecr.us-east-1.amazonaws.com/nexpo/api-cpp:latest
docker push <account>.dkr.ecr.us-east-1.amazonaws.com/nexpo/api-cpp:latest
```

## 📊 Monitoring

### Health Checks

The service provides comprehensive health checks:

```bash
# Basic health
curl http://localhost:8100/health

# Detailed health with dependencies
curl http://localhost:8110/health/detailed
```

### Metrics

- **Request Rate**: Requests per second
- **Response Time**: Average response time
- **Error Rate**: Error percentage
- **CPU Usage**: Container CPU utilization
- **Memory Usage**: Container memory utilization
- **Database Connections**: Active DB connections

### Logging

Logs are structured and sent to CloudWatch:

```cpp
LOG_INFO << "User created successfully, ID: " << userId;
LOG_WARN << "Rate limit exceeded for IP: " << clientIP;
LOG_ERROR << "Database connection failed: " << error.what();
```

## 🔒 Security

### Authentication

- **JWT Tokens**: Secure token-based authentication
- **Auth0 Integration**: Enterprise-grade authentication
- **Token Validation**: Automatic token validation middleware

### Network Security

- **CORS**: Configurable cross-origin resource sharing
- **Rate Limiting**: Protection against abuse
- **Input Validation**: Request validation and sanitization
- **SSL/TLS**: Encrypted communication

### Best Practices

- **Least Privilege**: Minimal required permissions
- **Secrets Management**: AWS Secrets Manager integration
- **Container Security**: Non-root user execution
- **Network Isolation**: VPC and security groups

## 🐛 Troubleshooting

### Common Issues

1. **Build Errors**:
   ```bash
   # Check dependencies
   pkg-config --libs jsoncpp
   pkg-config --cflags jsoncpp
   
   # Clean build
   rm -rf build && mkdir build && cd build
   cmake .. && make -j4
   ```

2. **Database Connection**:
   ```bash
   # Test database connectivity
   psql -h localhost -p 5432 -U nexpo_user -d nexpo
   
   # Check connection string
   echo $DATABASE_URL
   ```

3. **Container Issues**:
   ```bash
   # Check container logs
   docker logs <container-id>
   
   # Debug container
   docker exec -it <container-id> /bin/bash
   ```

### Debug Mode

Enable debug logging:

```cpp
app().setLogLevel(trantor::Logger::kDebug);
```

## 📈 Performance

### Optimization Tips

1. **Thread Configuration**:
   - Adjust `DROGON_THREAD_NUM` based on CPU cores
   - Monitor thread utilization

2. **Connection Pooling**:
   - Optimize `DROGON_DB_CONNECTION_NUMBER`
   - Monitor database connection usage

3. **Caching**:
   - Enable Kong proxy caching
   - Implement application-level caching

4. **Compression**:
   - Enable gzip/brotli compression
   - Optimize response sizes

### Benchmarking

```bash
# Load testing with wrk
wrk -t12 -c400 -d30s http://localhost:8110/api/v1/cpp/users

# Performance profiling
valgrind --tool=callgrind ./cpp_api
```

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests
5. Submit a pull request

### Development Guidelines

- Follow C++ best practices
- Use modern C++ features (C++17+)
- Write comprehensive tests
- Document public APIs
- Follow the existing code style

## 📄 License

This project is licensed under the MIT License. See the [LICENSE](../../../LICENSE) file for details.

## 🔗 Links

- [Drogon Framework](https://github.com/drogonframework/drogon)
- [Kong API Gateway](https://konghq.com/)
- [Auth0 Documentation](https://auth0.com/docs)
- [PostHog Analytics](https://posthog.com/)
- [AWS ECS Documentation](https://docs.aws.amazon.com/ecs/)

---

For more detailed information, see [CppAPI.md](./CppAPI.md).
