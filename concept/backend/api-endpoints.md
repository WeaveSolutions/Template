# API Endpoints Template

## REST API Structure

API endpoints should follow RESTful conventions:

**Standard CRUD Operations:**
- `GET /api/resource` - List all items
- `POST /api/resource` - Create new item
- `GET /api/resource/:id` - Get single item
- `PUT /api/resource/:id` - Update item
- `DELETE /api/resource/:id` - Delete item

**Best Practices:**
- Use proper HTTP methods (GET, POST, PUT, DELETE)
- Return appropriate status codes (200, 201, 400, 404, 500)
- Wrap responses in consistent format
- Handle errors gracefully
- Validate input data

## Response Format

All API responses should have a consistent structure:
- `success` (boolean): Indicates if request succeeded
- `data` (optional): The requested data or created/updated resource
- `error` (optional): Error message if request failed
- `pagination` (optional): Page info for list endpoints (page, limit, total)

## Error Handling

Implement centralized error handling:
- Create custom error class with status codes
- Use error middleware to catch all errors
- Return consistent error response format
- Log errors for debugging
- Don't expose sensitive information in error messages

## Authentication

Secure your API endpoints with authentication:
- Extract JWT token from Authorization header
- Verify token validity
- Attach user info to request object
- Return 401 for unauthorized requests
- Apply authentication middleware to protected routes
- Use different middleware for public vs protected endpoints

## Best Practices
1. Use versioning: `/api/v1/`
2. Implement rate limiting
3. Validate input
4. Log requests
5. Use CORS properly
6. Return consistent responses
7. Document with OpenAPI/Swagger
