# Nexpo API - Java Play Framework

Production-ready API backend built with Java Play Framework, featuring Auth0 JWT authentication, MindsDB integration, and comprehensive Swagger documentation.

## Features

- 🔐 **Auth0 JWT Authentication** with JWKS validation
- 📊 **MindsDB Integration** for unified data access
- 📚 **Swagger UI** for interactive API documentation
- 🛡️ **Security First** with CORS, CSP, and security headers
- 🔄 **Token Management** with refresh token support
- 🚀 **Production Ready** with Docker support and health checks
- 📝 **Structured Logging** with Logback
- ⚡ **Non-blocking I/O** with Play's async architecture
- 📦 **QUIC Support** Work in progress

## Prerequisites

- Java 17 or higher
- sbt 1.6.2 or higher
- Docker (optional)

## Quick Start

### Local Development

1. **Clone and navigate to the directory**:
   ```bash
   cd microservices/api/api-java
   ```

2. **Copy environment variables**:
   ```bash
   cp .env.example .env
   # Edit .env with your Auth0 and MindsDB configurations
   ```

3. **Run the application**:
   ```bash
   sbt run
   ```

   The API will be available at `http://localhost:8070`

### Docker

1. **Build the Docker image**:
   ```bash
   docker build -t nexpo-api-java .
   ```

2. **Run the container**:
   ```bash
   docker run -p 8070:8070 --env-file .env nexpo-api-java
   ```

## API Documentation

### Swagger UI

Access the interactive API documentation at:
- Local: http://localhost:8070/swagger
- Production: https://api.nexpo.com/java/swagger

### API Endpoints

| Method | Endpoint | Description | Auth Required |
|--------|----------|-------------|---------------|
| GET | `/` | Service info | No |
| GET | `/health` | Health check | No |
| GET | `/swagger.json` | OpenAPI spec | No |
| POST | `/api/auth/token` | Exchange tokens | No |
| GET | `/api/user/profile` | Get user profile | Yes |
| GET | `/api/user/linked-accounts` | List linked accounts | Yes |
| POST | `/api/data/query` | Query MindsDB | Yes |

## Authentication

All protected endpoints require a valid Auth0 JWT token in the Authorization header:

```http
Authorization: Bearer <your-jwt-token>
```

### Token Exchange

Exchange an authorization code for tokens:

```bash
curl -X POST http://localhost:8070/api/auth/token \
  -H "Content-Type: application/json" \
  -d '{
    "grant_type": "authorization_code",
    "code": "your-auth-code",
    "redirect_uri": "http://localhost:3000/callback"
  }'
```

### Refresh Token

Refresh an access token:

```bash
curl -X POST http://localhost:8070/api/auth/token \
  -H "Content-Type: application/json" \
  -d '{
    "grant_type": "refresh_token",
    "refresh_token": "your-refresh-token"
  }'
```

## MindsDB Integration

Query MindsDB using SQL:

```bash
curl -X POST http://localhost:8070/api/data/query \
  -H "Authorization: Bearer <token>" \
  -H "Content-Type: application/json" \
  -d '{
    "query": "SELECT * FROM users LIMIT 10",
    "timeout": 30
  }'
```

## Configuration

### Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `JAVA_API_PORT` | Server port | `8070` |
| `PLAY_HTTP_SECRET_KEY` | Play secret key | Required |
| `AUTH0_DOMAIN` | Auth0 domain | Required |
| `AUTH0_AUDIENCE` | Auth0 API audience | Required |
| `AUTH0_CLIENT_ID` | Auth0 client ID | Required |
| `AUTH0_CLIENT_SECRET` | Auth0 client secret | Required |
| `MINDSDB_URL` | MindsDB server URL | `http://localhost:47334` |
| `MINDSDB_TIMEOUT` | Query timeout | `30s` |
| `CORS_ORIGINS` | Allowed CORS origins | `http://localhost:3000` |
| `LOG_LEVEL` | Log level | `INFO` |

### Application Configuration

Main configuration is in `conf/application.conf`. Key settings:

- **Security filters**: CORS, CSRF, security headers
- **Logging**: Logback with structured JSON output
- **HTTP settings**: Request sizes, timeouts
- **Database**: Connection pool settings

## Development

### Project Structure

```
api-java/
├── app/
│   ├── actions/         # Custom Play actions (auth)
│   ├── controllers/     # HTTP controllers
│   ├── models/         # Data models
│   └── services/       # Business logic
├── conf/
│   ├── application.conf # Main configuration
│   ├── logback.xml     # Logging configuration
│   └── routes          # HTTP routes
├── project/            # SBT build configuration
├── test/              # Test files
└── build.sbt          # Build definition
```

### Building

```bash
# Compile
sbt compile

# Run tests
sbt test

# Create distribution
sbt dist

# Run with hot reload
sbt ~run
```

### Code Style

The project follows standard Java conventions:
- Google Java Style Guide
- Immutable data models where possible
- Dependency injection with `@Inject`
- Async/non-blocking operations

## Security

### Authentication Flow

1. Client obtains JWT from Auth0
2. Client sends JWT in Authorization header
3. AuthAction validates JWT with JWKS
4. Controller receives validated token claims

### Security Features

- **JWT Validation**: RSA signature verification with JWKS
- **CORS**: Configurable allowed origins
- **CSRF Protection**: Enabled for state-changing operations
- **Security Headers**: CSP, X-Frame-Options, etc.
- **Rate Limiting**: Via reverse proxy (Kong/Nginx)

## Testing

### Unit Tests

```bash
sbt test
```

### Integration Tests

```bash
sbt it:test
```

### Manual Testing

Use the Swagger UI or curl:

```bash
# Health check
curl http://localhost:8070/health

# Get user profile
curl http://localhost:8070/api/user/profile \
  -H "Authorization: Bearer <token>"
```

## Deployment

### Docker Deployment

```bash
# Build image
docker build -t nexpo-api-java:latest .

# Run with environment file
docker run -d \
  --name nexpo-api-java \
  -p 8070:8070 \
  --env-file .env \
  nexpo-api-java:latest
```

### Kubernetes

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: api-java
spec:
  replicas: 3
  selector:
    matchLabels:
      app: api-java
  template:
    metadata:
      labels:
        app: api-java
    spec:
      containers:
      - name: api-java
        image: nexpo-api-java:latest
        ports:
        - containerPort: 8070
        env:
        - name: PLAY_HTTP_SECRET_KEY
          valueFrom:
            secretKeyRef:
              name: api-secrets
              key: play-secret
        envFrom:
        - configMapRef:
            name: api-config
```

## Monitoring

### Health Check

```bash
curl http://localhost:8070/health
```

Response:
```json
{
  "status": "healthy",
  "service": "nexpo-api-java",
  "version": "1.0.0",
  "timestamp": "2024-01-20T10:30:00Z"
}
```

### Logs

Structured JSON logs are output to stdout/stderr:

```json
{
  "timestamp": "2024-01-20T10:30:00Z",
  "level": "INFO",
  "logger": "application",
  "message": "Server started on port 8070",
  "context": {
    "request_id": "123e4567-e89b-12d3-a456-426614174000"
  }
}
```

## Troubleshooting

### Common Issues

1. **JWT Validation Failures**
   - Check Auth0 domain and audience configuration
   - Ensure token hasn't expired
   - Verify JWKS endpoint is accessible

2. **MindsDB Connection Issues**
   - Verify MindsDB URL and port
   - Check network connectivity
   - Review MindsDB logs

3. **CORS Errors**
   - Add origin to CORS_ORIGINS environment variable
   - Check preflight request handling

### Debug Mode

Enable debug logging:

```bash
LOG_LEVEL=DEBUG sbt run
```

## Support

- **Documentation**: [Nexpo Docs](https://docs.nexpo.com)
- **Issues**: [GitHub Issues](https://github.com/nexpo/api-java/issues)
- **Community**: [Discord](https://discord.gg/nexpo)

## License

MIT License - see LICENSE file for details
