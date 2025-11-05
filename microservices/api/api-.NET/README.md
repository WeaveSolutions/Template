# Nexpo .NET API

Basic configurable backend API for Nexpo - .NET + ASP.NET Core Implementation

## Features

- **Environment Configuration**: Configurable via environment variables
- **JWT Authentication**: Auth0 integration with JWT bearer tokens
- **OpenAPI/Swagger**: Auto-generated API documentation
- **CORS Support**: Configurable cross-origin resource sharing
- **Health Checks**: Basic health monitoring endpoints
- **User Management**: User profile and account linking endpoints
- **Token Exchange**: OAuth2 token management
- **Query Interface**: MindsDB integration for AI queries

## Quick Start

### Prerequisites

- .NET 8.0 SDK or later
- Auth0 account (for authentication)

### Installation

```bash
# Clone and navigate to the directory
cd microservices/api/api-.NET

# Restore dependencies
dotnet restore

# Build the project
dotnet build
```

### Configuration

Copy `.env.example` to `.env` and configure your settings:

```bash
cp .env.example .env
```

Key environment variables:
- `DOTNET_API_PORT`: Server port (default: 7020)
- `AUTH0_DOMAIN`: Your Auth0 domain
- `AUTH0_CLIENT_ID`: Auth0 application client ID
- `AUTH0_CLIENT_SECRET`: Auth0 application client secret
- `AUTH0_AUDIENCE`: Auth0 API audience

### Running

```bash
# Development mode
dotnet run

# Or specify custom port
DOTNET_API_PORT=8020 dotnet run
```

The API will be available at:
- **API Base**: `http://localhost:7020`
- **Swagger UI**: `http://localhost:7020/docs`
- **Health Check**: `http://localhost:7020/health`

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
├── Models/
│   ├── Settings.cs          # Configuration model
│   └── ResponseModels.cs    # API response models
├── Services/
│   └── JwtService.cs        # JWT token verification
├── Program.cs               # Main application entry point
├── NexpoApi.csproj         # Project file
└── README.md               # This file
```

### Authentication

The API uses JWT Bearer tokens for authentication. Include the token in the Authorization header:

```
Authorization: Bearer <your-jwt-token>
```

### Error Handling

All errors return JSON responses with the following structure:

```json
{
  "error": "error_code",
  "message": "Human readable error message",
  "details": {}
}
```

## Deployment

### Docker

```bash
# Build Docker image
docker build -t nexpo-dotnet-api .

# Run container
docker run -p 7020:7020 --env-file .env nexpo-dotnet-api
```

### Production Considerations

- Set `NODE_ENV=production` for production deployments
- Use proper SSL certificates (HTTPS)
- Configure proper CORS origins
- Use secure Auth0 credentials
- Monitor logs and health endpoints

## Contributing

1. Follow C# coding conventions
2. Add XML documentation for public APIs
3. Include unit tests for new features
4. Update this README for significant changes
