// Test script to verify Swagger documentation generation
const swaggerJSDoc = require('swagger-jsdoc');

const swaggerDefinition = {
  openapi: '3.0.0',
  info: {
    title: 'Auth Microservice API',
    version: '1.0.0',
    description: 'Authentication and user management API with Auth0 integration',
    contact: {
      name: 'API Support',
      email: 'support@nexpo.com'
    },
    license: {
      name: 'MIT',
      url: 'https://opensource.org/licenses/MIT'
    }
  },
  servers: [
    {
      url: 'http://localhost:7000',
      description: 'Development server'
    }
  ],
  components: {
    securitySchemes: {
      bearerAuth: {
        type: 'http',
        scheme: 'bearer',
        bearerFormat: 'JWT',
        description: 'Auth0 JWT token'
      },
      cookieAuth: {
        type: 'apiKey',
        in: 'cookie',
        name: 'appSession',
        description: 'Session cookie from Auth0'
      }
    },
    schemas: {
      ErrorResponse: {
        type: 'object',
        properties: {
          error: {
            type: 'string',
            example: 'Invalid request'
          },
          message: {
            type: 'string',
            example: 'The request could not be validated'
          },
          code: {
            type: 'string',
            example: 'VALIDATION_ERROR'
          }
        }
      },
      HealthResponse: {
        type: 'object',
        properties: {
          status: {
            type: 'string',
            example: 'healthy'
          },
          service: {
            type: 'string',
            example: 'auth-service'
          },
          version: {
            type: 'string',
            example: '1.0.0'
          },
          timestamp: {
            type: 'string',
            format: 'date-time',
            example: '2024-01-15T10:30:00Z'
          }
        }
      }
    }
  }
};

const options = {
  definition: swaggerDefinition,
  apis: ['./routes/*.ts', './cra-server.ts'] // Path to the API routes
};

try {
  const swaggerSpec = swaggerJSDoc(options);
  console.log('✅ Swagger specification generated successfully!');
  console.log('\nNumber of paths:', Object.keys(swaggerSpec.paths || {}).length);
  console.log('Paths found:', Object.keys(swaggerSpec.paths || {}));
  
  // Display the spec
  console.log('\nGenerated Swagger spec preview:');
  console.log(JSON.stringify(swaggerSpec, null, 2).substring(0, 500) + '...');
} catch (error) {
  console.error('❌ Error generating Swagger spec:', error);
}
