# FastAPI Backend Service

Production-ready FastAPI implementation for the Nexpo template with Auth0 integration and automatic Swagger documentation.

## Features

- 🚀 **High Performance**: Async Python with Starlette and Pydantic
- 🔐 **Auth0 Integration**: JWT validation and CRA support
- 📝 **Automatic API Docs**: Swagger UI at `/docs`, ReDoc at `/redoc`
- 📊 **MindsDB Integration**: Unified database access
- 🔧 **Type Safety**: Full Pydantic model validation
- 🏗️ **Production Ready**: Docker support, health checks, structured logging

## Quick Start

### Development

1. **Install dependencies**:
   ```bash
   pip install -r requirements.txt
   ```

2. **Configure environment**:
   ```bash
   cp .env.example .env
   # Edit .env with your Auth0 credentials
   ```

3. **Run the server**:
   ```bash
   uvicorn main:app --reload --port 8030
   ```

4. **Access Swagger UI**:
   Open http://localhost:8030/docs

### Production

```bash
# Build Docker image
docker build -t nexpo-fastapi .

# Run container
docker run -d \
  --name nexpo-fastapi \
  -p 8030:8030 \
  --env-file .env \
  nexpo-fastapi
```

## API Documentation

### Swagger/OpenAPI

FastAPI automatically generates OpenAPI (Swagger) documentation:

- **Swagger UI**: http://localhost:8030/docs
- **ReDoc**: http://localhost:8030/redoc
- **OpenAPI JSON**: http://localhost:8030/openapi.json

### Authentication

All protected endpoints require a valid JWT token from Auth0:

```bash
curl -H "Authorization: Bearer YOUR_TOKEN" http://localhost:8030/api/user/profile
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

## Integration with Kong Gateway

Configure Kong to route to the FastAPI service:

```yaml
services:
  - name: fastapi-service
    url: http://nexpo-fastapi:8030
    
routes:
  - name: fastapi-routes
    service: fastapi-service
    paths:
      - /api/python
```

## Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `PYTHON_API_PORT` | Server port | 8030 |
| `NODE_ENV` | Environment | development |
| `AUTH0_DOMAIN` | Auth0 tenant domain | - |
| `AUTH0_AUDIENCE` | Auth0 API audience | - |
| `AUTH0_CLIENT_ID` | Auth0 client ID | - |
| `AUTH0_CLIENT_SECRET` | Auth0 client secret | - |
| `MINDSDB_URL` | MindsDB gateway URL | http://localhost:4040 |
| `CORS_ORIGINS` | Allowed CORS origins | http://localhost:3000 |

## Development

### Code Quality

```bash
# Format code
black .

# Sort imports
isort .

# Type checking
mypy .

# Linting
pylint main.py

# Run tests
pytest
```

### Adding New Endpoints

1. Define Pydantic models for request/response
2. Add route with proper tags and response models
3. Include authentication dependencies where needed
4. Document with docstrings (shown in Swagger UI)

Example:

```python
class ItemCreate(BaseModel):
    name: str = Field(description="Item name")
    price: float = Field(gt=0, description="Item price")

@app.post("/api/items", 
          response_model=ItemResponse,
          tags=["Items"])
async def create_item(
    item: ItemCreate,
    current_user: Dict = Depends(verify_token)
) -> ItemResponse:
    """Create a new item"""
    # Implementation
    pass
```

## Performance

- **Async/await**: Full async support for I/O operations
- **Connection pooling**: httpx with connection reuse
- **Response caching**: Can be added with Redis
- **Rate limiting**: Can be configured at Kong Gateway level

## Security

- JWT validation with RS256 algorithm
- CORS middleware with configurable origins
- Structured logging without sensitive data
- Non-root Docker user
- Health checks for container orchestration

## Monitoring

- Structured logging with structlog
- Health endpoints for monitoring
- Prometheus metrics (can be added)
- Request ID tracking via headers
