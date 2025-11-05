# Nexpo API - Scala Play Framework Backend

Production-ready API backend built with Scala Play Framework 2.9, featuring Auth0 authentication, MindsDB integration, and comprehensive Swagger documentation.

## Features

- 🔐 **Auth0 JWT Authentication** with JWKS validation
- 📚 **Swagger/OpenAPI Documentation** with interactive UI
- 🗄️ **MindsDB Integration** for unified data access
- 🌐 **CORS Support** with configurable origins
- 📊 **Structured Logging** with Logback and JSON output
- 🏃 **Async/Non-blocking** request handling
- 🐳 **Docker Support** with multi-stage builds
- 🔒 **Security Best Practices** with CSRF protection and security headers
- 🧪 **Type Safety** with Scala's strong type system
- 📦 **Dependency Injection** with Play's built-in support
- 📦 **QUIC Support** Work in progress

## Prerequisites

- JDK 11 or higher
- Scala 2.13.12
- sbt 1.9.7 or higher
- Docker (optional)

## Setup

1. **Clone the repository and navigate to the directory:**
   ```bash
   cd microservices/api/api-scala
   ```

2. **Copy environment variables:**
   ```bash
   cp .env.example .env
   ```

3. **Configure Auth0:**
   - Update `AUTH0_DOMAIN`, `AUTH0_AUDIENCE`, `AUTH0_CLIENT_ID`, and `AUTH0_CLIENT_SECRET` in `.env`
   - Ensure your Auth0 application is configured for API access

4. **Install dependencies:**
   ```bash
   sbt update
   ```

## Development

### Running Locally

```bash
# Run in development mode with hot reload
sbt run

# Run with specific port
sbt run -Dhttp.port=8060

# Run with environment variables
export $(cat .env | xargs) && sbt run
```

### Building for Production

```bash
# Create production distribution
sbt dist

# Create Docker image
docker build -t nexpo-api-scala .
```

### Running with Docker

```bash
# Build and run
docker build -t nexpo-api-scala .
docker run -p 7040:7040 --env-file .env nexpo-api-scala

# Using docker-compose
docker-compose up
```

## API Documentation

### Swagger UI
- **Development**: http://localhost:7040/swagger
- **API Spec**: http://localhost:7040/api-docs

### OpenAPI Specification
The API follows OpenAPI 3.0 specification and includes:
- Complete endpoint documentation
- Request/response schemas
- Authentication requirements
- Error response formats

## API Endpoints

### Health Check
- `GET /` - Service info
- `GET /health` - Health status

### Authentication
- `POST /api/auth/token` - Exchange authorization code or refresh token

### User Management (Requires Auth)
- `GET /api/user/profile` - Get user profile
- `GET /api/user/linked-accounts` - Get linked social accounts

### Data Operations (Requires Auth)
- `POST /api/data/query` - Execute MindsDB queries

## Authentication

All protected endpoints require a Bearer token in the Authorization header:

```bash
curl -H "Authorization: Bearer YOUR_JWT_TOKEN" http://localhost:7040/api/user/profile
```

### Token Validation
- Tokens are validated against Auth0's JWKS endpoint
- Signature verification using RS256 algorithm
- Claims validation including issuer and audience
- Token expiration checking

## Configuration

### Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `SCALA_API_PORT` | Server port | `8060` |
| `NODE_ENV` | Environment (development/production) | `development` |
| `APPLICATION_SECRET` | Play application secret | - |
| `AUTH0_DOMAIN` | Auth0 domain | - |
| `AUTH0_AUDIENCE` | Auth0 API audience | - |
| `AUTH0_CLIENT_ID` | Auth0 client ID | - |
| `AUTH0_CLIENT_SECRET` | Auth0 client secret | - |
| `MINDSDB_URL` | MindsDB server URL | `http://localhost:47334` |
| `CORS_ORIGINS` | Allowed CORS origins | `http://localhost:3000` |
| `LOG_LEVEL` | Logging level | `INFO` |

### Application Configuration

The `conf/application.conf` file contains:
- HTTP server settings
- Security filters configuration
- CORS policy
- Logging configuration
- Database settings (if needed)

## Project Structure

```
api-scala/
├── app/
│   ├── controllers/      # HTTP controllers
│   ├── models/          # Data models and DTOs
│   ├── services/        # Business logic services
│   ├── actions/         # Custom action builders
│   └── modules/         # Dependency injection modules
├── conf/
│   ├── application.conf # Main configuration
│   ├── routes          # URL routing
│   └── logback.xml     # Logging configuration
├── public/             # Static assets
├── test/              # Test files
├── build.sbt          # Build configuration
├── Dockerfile         # Docker configuration
└── README.md          # This file
```

## Security

- **JWT Validation**: All tokens validated against Auth0 JWKS
- **CORS**: Configurable allowed origins
- **CSRF Protection**: Enabled for state-changing operations
- **Security Headers**: Added via Play filters
- **Input Validation**: Using Play's form validation
- **Error Handling**: Sanitized error messages
- **Non-root Docker**: Runs as unprivileged user

## Monitoring and Logging

- Structured JSON logging with Logback
- Request/response logging
- Error tracking with stack traces
- Performance metrics via JMX
- Health check endpoint for monitoring

## Testing

```bash
# Run all tests
sbt test

# Run specific test
sbt "testOnly *UserControllerSpec"

# Run with coverage
sbt coverage test coverageReport
```

## Deployment

### Production Checklist

1. ✅ Set strong `APPLICATION_SECRET` and `PLAY_HTTP_SECRET_KEY`
2. ✅ Configure Auth0 for production domain
3. ✅ Update CORS origins for production URLs
4. ✅ Enable HTTPS in production
5. ✅ Configure proper logging levels
6. ✅ Set up monitoring and alerts
7. ✅ Configure database connections (if used)
8. ✅ Review security headers

### Docker Deployment

```dockerfile
# Multi-stage build for minimal image size
# Final image ~200MB with JRE only
# Runs as non-root user 'scala'
```

## Integration with Nexpo

This backend integrates with:
- **Kong Gateway**: Register as upstream service
- **MindsDB**: For data queries and ML models
- **Auth0**: For authentication and user management
- **Frontend**: Via CORS-enabled REST API

## Contributing

1. Follow Scala style guide
2. Write tests for new features
3. Update Swagger annotations
4. Ensure no compiler warnings
5. Run `sbt scalafmt` before committing

## License

MIT License - see LICENSE file for details

## Support

For issues and questions:
- Check the [Swagger documentation](http://localhost:7040/swagger)
- Review application logs
- Contact the development team

---

Built with ❤️ using Scala and Play Framework
