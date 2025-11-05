# Nexpo Haskell API

Basic configurable backend API for Nexpo - Haskell + Servant Implementation

## Features

- **Environment Configuration**: Configurable via environment variables
- **JWT Authentication**: Auth0 integration with JWT bearer tokens
- **Swagger/OpenAPI**: Auto-generated API documentation
- **CORS Support**: Configurable cross-origin resource sharing
- **Health Checks**: Basic health monitoring endpoints
- **User Management**: User profile and account linking endpoints
- **Token Exchange**: OAuth2 token management
- **Query Interface**: MindsDB integration for AI queries

## Quick Start

### Prerequisites

- GHC 9.4+ and Cabal 3.8+ (recommended via [GHCup](https://www.haskell.org/ghcup/))
- Auth0 account (for authentication)

### Installation

```bash
# Clone and navigate to the directory
cd microservices/api/api-haskell

# Update package database
cabal update

# Install dependencies and build
cabal build
```

### Configuration

Copy `.env.example` to `.env` and configure your settings:

```bash
cp .env.example .env
```

Key environment variables:
- `HASKELL_API_PORT`: Server port (default: 7030)
- `AUTH0_DOMAIN`: Your Auth0 domain
- `AUTH0_CLIENT_ID`: Auth0 application client ID
- `AUTH0_CLIENT_SECRET`: Auth0 application client secret
- `AUTH0_AUDIENCE`: Auth0 API audience

### Running

```bash
# Development mode
cabal run nexpo-api

# Or specify custom port
HASKELL_API_PORT=8030 cabal run nexpo-api
```

The API will be available at:
- **API Base**: `http://localhost:7030`
- **Swagger UI**: `http://localhost:7030/docs`
- **Health Check**: `http://localhost:7030/health`

## API Endpoints

### Health
- `GET /` - Basic health check
- `GET /health` - Detailed health information

### Authentication
- `POST /api/token` - Exchange credentials for access token

### User Management
- `GET /api/user/profile` - Get current user profile (requires auth)
- `GET /api/user/accounts` - Get linked social accounts (requires auth)

### Query Interface
- `POST /api/query` - Execute MindsDB queries (requires auth)

## Development

### Project Structure

```
├── src/
│   ├── Lib/
│   │   ├── Config.hs     # Configuration management
│   │   ├── Types.hs      # Data types and models
│   │   ├── Auth.hs       # Authentication logic
│   │   ├── Handlers.hs   # API endpoint handlers
│   │   └── Server.hs     # Server setup and routing
│   └── Main.hs           # Application entry point
├── nexpo-api.cabal       # Project configuration
└── README.md             # This file
```

### Authentication

The API uses JWT Bearer tokens for authentication. Include the token in the Authorization header:

```
Authorization: Bearer <your-jwt-token>
```

### Error Handling

All errors return JSON responses with appropriate HTTP status codes. The API follows RESTful conventions for error responses.

### Type Safety

This implementation leverages Haskell's strong type system to ensure:
- Compile-time guarantees about API contracts
- Safe JSON serialization/deserialization
- Proper error handling with Maybe and Either types

## Building and Testing

### Build

```bash
# Clean build
cabal clean && cabal build

# Build with optimizations
cabal build --enable-optimization
```

### Development REPL

```bash
# Start GHCi with project loaded
cabal repl
```

### Generating Documentation

```bash
# Generate Haddock documentation
cabal haddock
```

## Deployment

### Production Build

```bash
# Build optimized executable
cabal build --enable-optimization

# Copy executable for deployment
cp dist-newstyle/build/x86_64-linux/ghc-*/nexpo-api-1.0.0/x/nexpo-api/build/nexpo-api/nexpo-api ./nexpo-api-prod
```

### Production Considerations

- Set `NODE_ENV=production` for production deployments
- Use proper SSL certificates (HTTPS)
- Configure proper CORS origins
- Use secure Auth0 credentials
- Monitor logs and health endpoints
- Consider using Docker for containerized deployments

## Libraries Used

- **servant**: Type-safe web API framework
- **servant-swagger**: OpenAPI/Swagger documentation generation
- **aeson**: JSON parsing and encoding
- **warp**: High-performance HTTP server
- **jose**: JWT handling (placeholder for proper implementation)
- **http-client**: HTTP client for external API calls

## Notes

- This implementation uses simplified JWT validation for demonstration
- In production, implement proper JWT verification using the `jose` library
- Auth0 integration is mocked - replace with actual Auth0 Management API calls
- MindsDB queries return mock responses - integrate with actual MindsDB instance

## Contributing

1. Follow Haskell style conventions
2. Add Haddock documentation for public functions
3. Use hlint for code quality checks
4. Include property-based tests with QuickCheck
5. Update this README for significant changes
