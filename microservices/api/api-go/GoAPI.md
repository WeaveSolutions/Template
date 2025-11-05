# Go Beego Backend Service

Production-ready Beego implementation for the Nexpo template with Auth0 integration and automatic Swagger documentation.

## Features

- 🚀 **High Performance**: Go with Beego framework
- 🔐 **Auth0 Integration**: JWT validation and CRA support
- 📝 **Automatic API Docs**: Swagger UI at `/swagger`
- 📊 **MindsDB Integration**: Unified database access
- 🔧 **Type Safety**: Go's static typing
- 🏗️ **Production Ready**: Docker support, health checks, structured logging

## Quick Start

### Development

1. **Install Go** (1.21 or later)

2. **Install dependencies**:
   ```bash
   go mod download
   ```

3. **Configure environment**:
   ```bash
   cp .env.example .env
   # Edit .env with your Auth0 credentials
   ```

4. **Generate Swagger docs**:
   ```bash
   go install github.com/swaggo/swag/cmd/swag@latest
   swag init
   ```

5. **Run the server**:
   ```bash
   go run main.go
   ```

6. **Access Swagger UI**:
   Open http://localhost:8040/swagger

### Production

```bash
# Build Docker image
docker build -t nexpo-beego .

# Run container
docker run -d \
  --name nexpo-beego \
  -p 8040:8040 \
  --env-file .env \
  nexpo-beego
```

## API Documentation

### Swagger/OpenAPI

Beego automatically generates Swagger documentation:

- **Swagger UI**: http://localhost:8040/swagger
- **Swagger JSON**: http://localhost:8040/swagger/swagger.json

### Authentication

All protected endpoints require a valid JWT token from Auth0:

```bash
curl -H "Authorization: Bearer YOUR_TOKEN" http://localhost:8040/api/user/profile
```

## Endpoints

| Method | Path | Description | Auth Required |
|--------|------|-------------|---------------|
| GET | `/` | Health check | No |
| GET | `/health` | Detailed health check | No |
| GET | `/api/user/profile` | Get user profile | Yes |
| GET | `/api/user/linked-accounts` | Get linked accounts | Yes |
| POST | `/api/auth/token` | Token exchange | No |
| GET | `/api/data/query` | Query MindsDB | Yes |
| GET | `/swagger` | Swagger UI | No |

## Integration with Kong Gateway

Configure Kong to route to the Beego service:

```yaml
services:
  - name: beego-service
    url: http://nexpo-beego:8040
    
routes:
  - name: beego-routes
    service: beego-service
    paths:
      - /api/go
```

## Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `GO_API_PORT` | Server port | 8040 |
| `NODE_ENV` | Environment | development |
| `AUTH0_DOMAIN` | Auth0 tenant domain | - |
| `AUTH0_AUDIENCE` | Auth0 API audience | - |
| `AUTH0_CLIENT_ID` | Auth0 client ID | - |
| `AUTH0_CLIENT_SECRET` | Auth0 client secret | - |
| `MINDSDB_URL` | MindsDB gateway URL | http://localhost:4040 |
| `CORS_ORIGINS` | Allowed CORS origins | http://localhost:3000 |

## Development

### Project Structure

```
api-go/
├── main.go           # Main application file
├── go.mod           # Go module file
├── go.sum           # Go dependencies
├── docs/            # Generated Swagger docs
├── swagger/         # Swagger UI static files
├── .env.example     # Environment variables example
├── Dockerfile       # Docker configuration
└── README.md       # This file
```

### Adding New Endpoints

1. Add controller method with Swagger annotations:

```go
// GetItems handles GET /api/items
// @Title Get Items
// @Description Get list of items
// @Param Authorization header string true "Bearer token"
// @Param page query int false "Page number" default(1)
// @Param limit query int false "Items per page" default(10)
// @Success 200 {array} Item
// @Failure 401 {object} ErrorResponse
// @router /api/items [get]
func (c *ItemController) GetItems() {
    // Implementation
}
```

2. Register route in main.go:
```go
web.Router("/api/items", &ItemController{}, "get:GetItems")
```

3. Regenerate Swagger docs:
```bash
swag init
```

### Testing

```bash
# Run tests
go test ./...

# Run tests with coverage
go test -cover ./...

# Run benchmarks
go test -bench=. ./...
```

## Performance

- **Compiled language**: Native performance
- **Concurrent request handling**: Goroutines
- **Efficient routing**: Beego's tree-based router
- **Low memory footprint**: Efficient memory usage

## Security

- JWT validation with configurable algorithms
- CORS middleware with configurable origins
- Structured logging without sensitive data
- Non-root Docker user
- Health checks for container orchestration

## Monitoring

- Structured logging with Beego logs
- Health endpoints for monitoring
- Prometheus metrics support (can be added)
- Request ID tracking via headers

## Production Considerations

- Enable HTTPS in production
- Use proper JWT validation with Auth0 JWKS
- Configure rate limiting at Kong Gateway
- Enable distributed tracing
- Set up proper log aggregation
