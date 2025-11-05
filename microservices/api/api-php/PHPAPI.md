# Nexpo PHP Laravel API

A comprehensive PHP Laravel backend for the Nexpo polyglot microservices platform, providing Auth0 JWT authentication, MindsDB integration, and RESTful API endpoints.

## 🚀 Features

- **Auth0 Integration**: JWT token validation with JWKS support
- **MindsDB Integration**: Query proxy for machine learning operations
- **RESTful API**: User management and data access endpoints
- **Swagger Documentation**: Auto-generated OpenAPI documentation
- **Security**: CORS, rate limiting, and security headers
- **Docker Support**: Multi-stage builds for development and production
- **Testing**: Comprehensive test suite with PHPUnit
- **Logging**: Structured logging with Laravel's logging system

## 🛠 Tech Stack

- **Framework**: Laravel 10.x
- **Language**: PHP 8.2+
- **Authentication**: Auth0 JWT with Firebase JWT library
- **API Docs**: L5-Swagger (OpenAPI 3.0)
- **HTTP Client**: Guzzle HTTP
- **Testing**: PHPUnit
- **Server**: Nginx + PHP-FPM
- **Process Manager**: Supervisor

## 📦 Installation

### Prerequisites

- PHP 8.2 or higher
- Composer
- Docker (optional)

### Local Development

1. **Clone and navigate to the directory**:
   ```bash
   cd microservices/api/api-php
   ```

2. **Install dependencies**:
   ```bash
   composer install
   ```

3. **Environment setup**:
   ```bash
   cp .env.example .env
   php artisan key:generate
   ```

4. **Configure environment variables** in `.env`:
   ```env
   # Auth0 Configuration
   AUTH0_DOMAIN=your-auth0-domain.auth0.com
   AUTH0_CLIENT_ID=your-client-id
   AUTH0_CLIENT_SECRET=your-client-secret
   AUTH0_AUDIENCE=your-api-audience
   
   # MindsDB Configuration
   MINDSDB_URL=https://cloud.mindsdb.com
   MINDSDB_USERNAME=your-username
   MINDSDB_PASSWORD=your-password
   ```

5. **Start the development server**:
   ```bash
   php artisan serve --host=0.0.0.0 --port=8100
   ```

### Docker Development

1. **Build and run with Docker**:
   ```bash
   docker build --target development -t nexpo-php-api:dev .
   docker run -p 8100:8100 --env-file .env nexpo-php-api:dev
   ```

## 🔗 API Endpoints

### Health Check
- `GET /` - API information and status
- `GET /health` - Detailed health check

### Authentication
- `POST /api/auth/token` - Exchange authorization code for JWT token

### User Management (Auth Required)
- `GET /api/user/profile` - Get authenticated user profile
- `GET /api/user/linked-accounts` - Get user's linked social accounts

### Data Access (Auth Required)
- `GET /api/data/query` - Execute MindsDB queries

## 📖 API Documentation

Interactive Swagger documentation is available at:
- **Development**: http://localhost:8100/api/documentation
- **Production**: https://your-domain.com/api/documentation

## 🔐 Authentication

This API uses Auth0 JWT tokens for authentication. Include the token in the Authorization header:

```bash
curl -H "Authorization: Bearer YOUR_JWT_TOKEN" \
     http://localhost:8100/api/user/profile
```

### Token Exchange Flow

1. Get authorization code from Auth0
2. Exchange code for JWT token:
   ```bash
   curl -X POST http://localhost:8100/api/auth/token \
        -H "Content-Type: application/json" \
        -d '{"authorization_code": "YOUR_AUTH_CODE"}'
   ```

## 🧪 Testing

Run the test suite:

```bash
# Run all tests
php artisan test

# Run specific test suite
php artisan test --testsuite=Feature

# Run with coverage
php artisan test --coverage
```

## 🐳 Docker

### Development Build
```bash
docker build --target development -t nexpo-php-api:dev .
```

### Production Build
```bash
docker build --target production -t nexpo-php-api:prod .
```

### Docker Compose
```yaml
version: '3.8'
services:
  api-php:
    build:
      context: .
      target: development
    ports:
      - "8100:8100"
    environment:
      - APP_ENV=local
      - AUTH0_DOMAIN=${AUTH0_DOMAIN}
      - AUTH0_CLIENT_ID=${AUTH0_CLIENT_ID}
      - AUTH0_CLIENT_SECRET=${AUTH0_CLIENT_SECRET}
    volumes:
      - .:/var/www/html
```

## 🔧 Configuration

### Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `APP_PORT` | Server port | `8100` |
| `AUTH0_DOMAIN` | Auth0 domain | Required |
| `AUTH0_CLIENT_ID` | Auth0 client ID | Required |
| `AUTH0_CLIENT_SECRET` | Auth0 client secret | Required |
| `AUTH0_AUDIENCE` | API audience | Required |
| `MINDSDB_URL` | MindsDB endpoint | `https://cloud.mindsdb.com` |
| `MINDSDB_USERNAME` | MindsDB username | Required |
| `MINDSDB_PASSWORD` | MindsDB password | Required |
| `CORS_ALLOWED_ORIGINS` | Allowed CORS origins | `*` |

### Laravel Configuration

Key configuration files:
- `config/cors.php` - CORS settings
- `config/l5-swagger.php` - Swagger documentation
- `bootstrap/app.php` - Application bootstrap and middleware

## 📊 Monitoring & Logging

### Health Checks
The API provides comprehensive health checks including:
- Database connectivity
- Auth0 service status  
- MindsDB connectivity
- System uptime and resources

### Logging
Structured logging is implemented with Laravel's logging system:
```php
Log::info('User authenticated', ['user_id' => $userId]);
Log::error('Auth failed', ['error' => $exception->getMessage()]);
```

## 🚀 Deployment

### Production Deployment

1. **Build production image**:
   ```bash
   docker build --target production -t nexpo-php-api:latest .
   ```

2. **Deploy with environment variables**:
   ```bash
   docker run -d \
     --name nexpo-php-api \
     -p 8100:8100 \
     -e APP_ENV=production \
     -e AUTH0_DOMAIN=your-domain.auth0.com \
     nexpo-php-api:latest
   ```

### AWS ECS Deployment

The API is configured for AWS ECS deployment with:
- Health check endpoint: `/health`
- Graceful shutdown handling
- Container resource optimization
- CloudWatch logging integration

## 🔄 Integration with Other Services

### Kong API Gateway
Configure Kong upstream and service:
```bash
# Create upstream
curl -X POST http://kong:8001/upstreams \
  --data name=php-api \
  --data algorithm=round-robin

# Add target
curl -X POST http://kong:8001/upstreams/php-api/targets \
  --data target=api-php:8100

# Create service
curl -X POST http://kong:8001/services \
  --data name=php-api-service \
  --data host=php-api
```

### Auth0 Configuration
Ensure your Auth0 application has:
- JWT algorithm: RS256
- Token endpoint authentication: POST
- Allowed callback URLs configured
- API audience set correctly

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add some amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## 📄 License

This project is part of the Nexpo platform and follows the same licensing terms.

## 🆘 Support

For support, please refer to:
- API documentation at `/api/documentation`
- Health check endpoint at `/health`
- Laravel documentation: https://laravel.com/docs
- Auth0 documentation: https://auth0.com/docs
