# Nexpo R API with Plumber

A production-ready R API backend implementing the Nexpo API specification using the Plumber framework.

## Features

- 🔐 **Auth0 JWT Authentication**: Secure API endpoints with JWT token validation
- 📊 **MindsDB Integration**: Query proxy for unified data access
- 📚 **Swagger Documentation**: Auto-generated API documentation
- 🔄 **CORS Support**: Configurable cross-origin resource sharing
- 📝 **Structured Logging**: Built-in logging with configurable levels
- 🐳 **Docker Ready**: Multi-stage Dockerfile for optimized containers
- ✅ **Health Checks**: Built-in health monitoring endpoints

## Quick Start

### Prerequisites

- R 4.3.0 or higher
- RStudio (optional but recommended)
- Docker (for containerized deployment)

### Local Development

1. **Clone and navigate to the project**:
   ```bash
   cd microservices/api/api-R
   ```

2. **Install dependencies**:
   ```R
   install.packages(c("plumber", "jose", "httr", "jsonlite", "logger"))
   ```

3. **Set up environment variables**:
   ```bash
   cp .env.example .env
   # Edit .env with your Auth0 and MindsDB credentials
   ```

4. **Run the API**:
   ```bash
   Rscript api.R
   ```

   Or in RStudio:
   ```R
   source("api.R")
   ```

The API will be available at `http://localhost:7060`

### Docker Deployment

1. **Build the Docker image**:
   ```bash
   docker build -t nexpo-api-r .
   ```

2. **Run with Docker Compose**:
   ```bash
   docker-compose up -d
   ```

## API Endpoints

### Public Endpoints

| Method | Path | Description |
|--------|------|-------------|
| GET | `/` | Service information |
| GET | `/health` | Health check |
| GET | `/__docs__/` | Swagger UI documentation |
| POST | `/api/auth/token` | Exchange auth code for tokens |

### Protected Endpoints (Require JWT)

| Method | Path | Description |
|--------|------|-------------|
| GET | `/api/user/profile` | Get user profile |
| GET | `/api/user/linked-accounts` | Get linked accounts |
| POST | `/api/data/query` | Execute MindsDB query |

## Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `R_API_PORT` | API server port | `8080` |
| `AUTH0_DOMAIN` | Auth0 domain | Required |
| `AUTH0_AUDIENCE` | Auth0 API audience | Required |
| `AUTH0_CLIENT_ID` | Auth0 client ID | Required |
| `AUTH0_CLIENT_SECRET` | Auth0 client secret | Required |
| `MINDSDB_URL` | MindsDB server URL | `http://localhost:47334` |
| `MINDSDB_TIMEOUT` | Query timeout (seconds) | `30` |
| `CORS_ORIGINS` | Allowed CORS origins | `*` |
| `LOG_LEVEL` | Logging level | `INFO` |

## Authentication

The API uses Auth0 JWT tokens for authentication. Include the token in the Authorization header:

```bash
curl -H "Authorization: Bearer YOUR_JWT_TOKEN" \
  http://localhost:7060/api/user/profile
```

## Examples

### Health Check
```bash
curl http://localhost:8080/health
```

Response:
```json
{
  "status": "healthy",
  "service": "nexpo-api-r",
  "version": "1.0.0",
  "timestamp": "2024-01-15T10:30:45+0000"
}
```

### Token Exchange
```bash
curl -X POST http://localhost:8080/api/auth/token \
  -H "Content-Type: application/json" \
  -d '{
    "grant_type": "authorization_code",
    "code": "AUTH_CODE",
    "redirect_uri": "http://localhost:3000/callback"
  }'
```

### MindsDB Query
```bash
curl -X POST http://localhost:8080/api/data/query \
  -H "Authorization: Bearer YOUR_JWT_TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "query": "SELECT * FROM users LIMIT 10",
    "timeout": 30
  }'
```

## Development

### Project Structure
```
api-R/
├── api.R                 # Main API file with endpoints
├── Dockerfile           # Multi-stage Docker build
├── docker-compose.yml   # Docker Compose configuration
├── .env.example        # Environment variable template
├── .gitignore          # Git ignore file
├── package.json        # pnpm scripts for consistency
└── README.md           # This file
```

### Adding New Endpoints

Add new endpoints using Plumber annotations:

```r
#* Get custom data
#* @get /api/custom/data
#* @tag Custom
#* @preempt auth_filter
#* @serializer json
function(req) {
  list(
    data = "Custom data",
    user = req$jwt_claims$sub
  )
}
```

### Logging

Use the logger package for structured logging:

```r
log_info("Processing request")
log_error("Error occurred: {error_message}")
log_debug("Debug information")
```

## Testing

### Manual Testing

1. **Start the API**:
   ```bash
   Rscript api.R
   ```

2. **Test endpoints**:
   ```bash
   # Health check
   curl http://localhost:8080/health
   
   # With authentication
   curl -H "Authorization: Bearer TOKEN" \
     http://localhost:8080/api/user/profile
   ```

### Running Tests

```R
# Install test dependencies
install.packages(c("testthat", "httptest"))

# Run tests
testthat::test_dir("tests/")
```

## Production Deployment

### Security Considerations

1. **Environment Variables**: Never commit `.env` files
2. **HTTPS**: Always use HTTPS in production
3. **Rate Limiting**: Implement rate limiting at the gateway level
4. **Monitoring**: Set up logging and monitoring

### Performance Optimization

1. **Connection Pooling**: Use connection pools for database access
2. **Caching**: Implement caching for frequently accessed data
3. **Async Operations**: Use future/promises for long-running operations

### Deployment Options

1. **Docker Swarm/Kubernetes**:
   ```yaml
   apiVersion: apps/v1
   kind: Deployment
   metadata:
     name: nexpo-api-r
   spec:
     replicas: 3
     selector:
       matchLabels:
         app: nexpo-api-r
   ```

2. **Cloud Platforms**:
   - AWS ECS/Fargate
   - Google Cloud Run
   - Azure Container Instances
   - DigitalOcean App Platform

## Troubleshooting

### Common Issues

1. **JWT Validation Errors**:
   - Check Auth0 configuration
   - Verify token expiration
   - Ensure correct audience/issuer

2. **MindsDB Connection**:
   - Verify MindsDB URL
   - Check network connectivity
   - Review query syntax

3. **Package Installation**:
   ```R
   # Clear package cache
   .libPaths()
   remove.packages("package_name")
   install.packages("package_name")
   ```

### Debug Mode

Enable debug logging:
```bash
export LOG_LEVEL=DEBUG
Rscript api.R
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Commit your changes
4. Push to the branch
5. Create a Pull Request

## License

This project is part of the Nexpo template and is licensed under the MIT License.
