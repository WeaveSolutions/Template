# Nexpo Julia API with Genie.jl

A high-performance Julia API backend implementing the Nexpo API specification using the Genie.jl web framework.

## Features

- 🔐 **Auth0 JWT Authentication**: Secure API endpoints with JWT token validation
- 📊 **MindsDB Integration**: Query proxy for unified data access
- 🚀 **High Performance**: Julia's speed for compute-intensive operations
- 🔄 **CORS Support**: Configurable cross-origin resource sharing
- 📝 **Structured Logging**: Built-in logging with configurable levels
- 🐳 **Docker Ready**: Multi-stage Dockerfile for optimized containers
- ✅ **Health Checks**: Built-in health monitoring endpoints

## Quick Start

### Prerequisites

- Julia 1.9 or higher
- Docker (for containerized deployment)

### Local Development

1. **Clone and navigate to the project**:
   ```bash
   cd microservices/api/api-julia
   ```

2. **Install dependencies**:
   ```julia
   julia> using Pkg
   julia> Pkg.activate(".")
   julia> Pkg.instantiate()
   ```

3. **Set up environment variables**:
   ```bash
   cp .env.example .env
   # Edit .env with your Auth0 and MindsDB credentials
   ```

4. **Run the API**:
   ```bash
   julia app.jl
   ```

The API will be available at `http://localhost:7070`

### Docker Deployment

1. **Build the Docker image**:
   ```bash
   docker run -p 7070:7070 --env-file .env nexpo-api-julia .
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
| `JULIA_API_PORT` | API server port | `7070` |
| `AUTH0_DOMAIN` | Auth0 domain | Required |
| `AUTH0_AUDIENCE` | Auth0 API audience | Required |
| `AUTH0_CLIENT_ID` | Auth0 client ID | Required |
| `AUTH0_CLIENT_SECRET` | Auth0 client secret | Required |
| `MINDSDB_URL` | MindsDB server URL | `http://localhost:47334` |
| `MINDSDB_TIMEOUT` | Query timeout (seconds) | `30` |
| `CORS_ORIGINS` | Allowed CORS origins | `*` |
| `LOG_LEVEL` | Logging level | `INFO` |
| `GENIE_ENV` | Genie environment | `prod` |

## Authentication

The API uses Auth0 JWT tokens for authentication. Include the token in the Authorization header:

```bash
curl -H "Authorization: Bearer YOUR_JWT_TOKEN" http://localhost:7070/api/user/profile
```

## Examples

### Health Check
```bash
curl http://localhost:8090/health
```

Response:
```json
{
  "status": "healthy",
  "service": "nexpo-api-julia",
  "version": "1.0.0",
  "timestamp": "2024-01-15T10:30:45Z"
}
```

### Token Exchange
```bash
curl -X POST http://localhost:8090/api/auth/token \
  -H "Content-Type: application/json" \
  -d '{
    "grant_type": "authorization_code",
    "code": "AUTH_CODE",
    "redirect_uri": "http://localhost:3000/callback"
  }'
```

### MindsDB Query
```bash
curl -X POST http://localhost:8090/api/data/query \
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
api-julia/
├── src/
│   └── NexpoAPI.jl      # Main API module
├── app.jl               # Application entry point
├── Project.toml         # Julia project file
├── Manifest.toml        # Julia dependencies lock
├── Dockerfile           # Multi-stage Docker build
├── docker-compose.yml   # Docker Compose configuration
├── .env.example        # Environment variable template
├── .gitignore          # Git ignore file
├── package.json        # pnpm scripts for consistency
└── README.md           # This file
```

### Adding New Endpoints

Add new routes in the `NexpoAPI.jl` module:

```julia
route("/api/custom/data", method = GET, before = auth_middleware) do
    cors_headers()
    
    claims = params(:jwt_claims)
    
    Json.json(Dict(
        "data" => "Custom data",
        "user" => claims.sub
    ))
end
```

### Logging

Use Julia's built-in logging:

```julia
@info "Processing request"
@error "Error occurred" exception=e
@debug "Debug information"
```

## Performance Optimization

Julia excels at:
- **Numerical Computing**: Fast mathematical operations
- **Data Processing**: Efficient array and matrix operations
- **Parallel Processing**: Built-in support for parallelism
- **Type Stability**: JIT compilation for optimal performance

### Optimization Tips

1. **Type Annotations**: Use type annotations for better performance
2. **Avoid Global Variables**: Use function parameters or constants
3. **Precompilation**: Use `PackageCompiler.jl` for faster startup
4. **Profiling**: Use `@profile` and `ProfileView.jl`

## Testing

### Manual Testing

1. **Start the API**:
   ```bash
   julia app.jl
   ```

2. **Test endpoints**:
   ```bash
   # Health check
   curl http://localhost:8090/health
   
   # With authentication
   curl -H "Authorization: Bearer TOKEN" \
     http://localhost:8090/api/user/profile
   ```

### Running Tests

```julia
julia> using Pkg
julia> Pkg.test()
```

## Production Deployment

### Security Considerations

1. **Environment Variables**: Never commit `.env` files
2. **HTTPS**: Always use HTTPS in production
3. **Rate Limiting**: Implement rate limiting at the gateway level
4. **Input Validation**: Validate all user inputs

### Performance Tuning

1. **Julia Threads**: Set `JULIA_NUM_THREADS` for parallelism
2. **Connection Pooling**: Use connection pools for database access
3. **Caching**: Implement caching for frequently accessed data
4. **Memory Management**: Monitor and optimize memory usage

### Deployment Options

1. **Docker Swarm/Kubernetes**:
   ```yaml
   apiVersion: apps/v1
   kind: Deployment
   metadata:
     name: nexpo-api-julia
   spec:
     replicas: 3
     selector:
       matchLabels:
         app: nexpo-api-julia
   ```

2. **Cloud Platforms**:
   - AWS ECS/Fargate
   - Google Cloud Run
   - Azure Container Instances
   - Heroku with Julia buildpack

## Troubleshooting

### Common Issues

1. **Package Installation**:
   ```julia
   # Clear package registry
   julia> using Pkg
   julia> Pkg.Registry.update()
   julia> Pkg.resolve()
   ```

2. **JWT Validation Errors**:
   - Check Auth0 configuration
   - Verify token expiration
   - Ensure correct audience/issuer

3. **Performance Issues**:
   ```julia
   # Enable profiling
   julia> @profile your_function()
   julia> using ProfileView
   julia> ProfileView.view()
   ```

### Debug Mode

Enable debug logging:
```bash
export LOG_LEVEL=DEBUG
julia app.jl
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Commit your changes
4. Push to the branch
5. Create a Pull Request

## License

This project is part of the Nexpo template and is licensed under the MIT License.
