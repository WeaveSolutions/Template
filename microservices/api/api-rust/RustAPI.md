# Rust Actix Backend Service

Production-ready Actix-web implementation for the Nexpo template with Auth0 integration and automatic Swagger documentation.

## Features

- 🚀 **High Performance**: Rust with Actix-web framework
- 🔐 **Auth0 Integration**: JWT validation and CRA support
- 📝 **Automatic API Docs**: Swagger UI at `/swagger-ui`
- 📊 **MindsDB Integration**: Unified database access
- 🔧 **Type Safety**: Rust's ownership system and strong typing
- 🏗️ **Production Ready**: Docker support, health checks, structured logging

## Quick Start

### Development

1. **Install Rust** (1.77 or later):
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
   ```

2. **Configure environment**:
   ```bash
   cp .env.example .env
   # Edit .env with your Auth0 credentials
   ```

3. **Run the server**:
   ```bash
   cargo run
   ```

4. **Access Swagger UI**:
   Open http://localhost:7030/swagger-ui

### Production

```bash
# Build Docker image
docker build -t nexpo-actix .

# Run container
docker run -d \
  --name nexpo-actix \
  -p 7030:7030 \
  --env-file .env \
  nexpo-actix
```

## API Documentation

### Swagger/OpenAPI

Actix-web with utoipa provides automatic Swagger documentation:

- **Swagger UI**: http://localhost:7030/swagger-ui
- **OpenAPI JSON**: http://localhost:7030/api-docs/openapi.json

### Authentication

All protected endpoints require a valid JWT token from Auth0:

```bash
curl -H "Authorization: Bearer YOUR_TOKEN" http://localhost:7030/api/user/profile
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
| GET | `/swagger-ui` | Swagger UI | No |

## Integration with Kong Gateway

Configure Kong to route to the Actix service:

```yaml
services:
  - name: actix-service
    url: http://nexpo-actix:7030
    
routes:
  - name: actix-routes
    service: actix-service
    paths:
      - /api/rust
```

## Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `RUST_API_PORT` | Server port | 8050 |
| `HOST` | Server host | 0.0.0.0 |
| `NODE_ENV` | Environment | development |
| `AUTH0_DOMAIN` | Auth0 tenant domain | - |
| `AUTH0_AUDIENCE` | Auth0 API audience | - |
| `AUTH0_CLIENT_ID` | Auth0 client ID | - |
| `AUTH0_CLIENT_SECRET` | Auth0 client secret | - |
| `MINDSDB_URL` | MindsDB gateway URL | http://localhost:4040 |
| `CORS_ORIGINS` | Allowed CORS origins | http://localhost:3000 |
| `RUST_LOG` | Log level | info |

## Development

### Project Structure

```
api-rust/
├── src/
│   └── main.rs      # Main application file
├── Cargo.toml       # Rust dependencies
├── Cargo.lock       # Dependency lock file
├── .env.example     # Environment variables example
├── Dockerfile       # Docker configuration
└── README.md        # This file
```

### Adding New Endpoints

1. Add handler function with utoipa macros:

```rust
#[utoipa::path(
    get,
    path = "/api/items",
    tag = "items",
    params(
        ("page" = Option<i32>, Query, description = "Page number"),
        ("limit" = Option<i32>, Query, description = "Items per page")
    ),
    security(
        ("bearer_auth" = [])
    ),
    responses(
        (status = 200, description = "Items retrieved", body = Vec<Item>),
        (status = 401, description = "Unauthorized", body = ErrorResponse)
    )
)]
async fn get_items(
    req: HttpRequest,
    query: web::Query<ItemQuery>,
) -> Result<HttpResponse> {
    // Implementation
}
```

2. Register route in main:
```rust
.route("/api/items", web::get().to(get_items))
```

3. Add to OpenAPI spec:
```rust
#[derive(OpenApi)]
#[openapi(
    paths(
        // ... existing paths
        get_items
    ),
    components(
        schemas(
            // ... existing schemas
            Item,
            ItemQuery
        )
    )
)]
```

### Testing

```bash
# Run tests
cargo test

# Run tests with output
cargo test -- --nocapture

# Run specific test
cargo test test_name

# Run benchmarks
cargo bench
```

## Performance

- **Zero-cost abstractions**: Rust's compile-time optimizations
- **Async/await**: Non-blocking I/O with Tokio runtime
- **Memory safety**: No garbage collector overhead
- **Efficient routing**: Actix-web's type-safe router

## Security

- JWT validation with configurable algorithms
- CORS middleware with configurable origins
- Structured logging without sensitive data
- Non-root Docker user
- Memory safety guaranteed by Rust

## Monitoring

- Structured logging with tracing
- Health endpoints for monitoring
- Request ID tracking via headers
- Performance metrics with minimal overhead

## Production Considerations

- Enable HTTPS in production
- Use proper JWT validation with Auth0 JWKS
- Configure rate limiting at Kong Gateway
- Enable distributed tracing
- Set up proper log aggregation
- Consider using `cargo-audit` for security scanning
