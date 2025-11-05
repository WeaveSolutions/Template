import swaggerJsdoc from 'swagger-jsdoc';
import type { SwaggerUiOptions } from 'swagger-ui-express';

// Swagger definition
const swaggerDefinition = {
  openapi: '3.0.0',
  info: {
    title: 'Nexpo Auth Service API',
    version: '1.0.0',
    description: 'Production-ready Express.js authentication service with Auth0 integration',
    contact: {
      name: 'API Support',
      email: 'support@nexpo.dev'
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
    },
    {
      url: 'http://localhost:8000/auth',
      description: 'Behind Kong Gateway'
    }
  ],
  components: {
    securitySchemes: {
      bearerAuth: {
        type: 'http',
        scheme: 'bearer',
        bearerFormat: 'JWT',
        description: 'Enter JWT token from Auth0'
      },
      cookieAuth: {
        type: 'apiKey',
        in: 'cookie',
        name: 'appSession',
        description: 'Auth0 session cookie'
      }
    },
    schemas: {
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
            example: '2024-01-01T00:00:00Z'
          }
        }
      },
      ReadyResponse: {
        type: 'object',
        properties: {
          status: {
            type: 'string',
            example: 'ready'
          },
          checks: {
            type: 'object',
            properties: {
              auth0: {
                type: 'string',
                example: 'connected'
              },
              database: {
                type: 'string',
                example: 'not_required'
              }
            }
          }
        }
      },
      VersionResponse: {
        type: 'object',
        properties: {
          version: {
            type: 'string',
            example: '1.0.0'
          },
          api: {
            type: 'string',
            example: 'v1'
          },
          features: {
            type: 'object',
            properties: {
              oauth: {
                type: 'boolean',
                example: true
              },
              passkeys: {
                type: 'boolean',
                example: false
              },
              linking: {
                type: 'boolean',
                example: true
              },
              audit: {
                type: 'boolean',
                example: true
              }
            }
          }
        }
      },
      UserProfile: {
        type: 'object',
        properties: {
          sub: {
            type: 'string',
            example: 'auth0|123456'
          },
          email: {
            type: 'string',
            example: 'user@example.com'
          },
          name: {
            type: 'string',
            example: 'John Doe'
          },
          picture: {
            type: 'string',
            example: 'https://example.com/avatar.jpg'
          },
          email_verified: {
            type: 'boolean',
            example: true
          },
          updated_at: {
            type: 'string',
            format: 'date-time'
          },
          roles: {
            type: 'array',
            items: {
              type: 'string'
            },
            example: ['user']
          },
          permissions: {
            type: 'array',
            items: {
              type: 'string'
            },
            example: ['read:profile']
          }
        }
      },
      LinkedAccount: {
        type: 'object',
        properties: {
          provider: {
            type: 'string',
            example: 'google'
          },
          user_id: {
            type: 'string',
            example: 'google-oauth2|123456'
          },
          connection: {
            type: 'string',
            example: 'google-oauth2'
          },
          is_social: {
            type: 'boolean',
            example: true
          },
          linked_at: {
            type: 'string',
            format: 'date-time'
          }
        }
      },
      ErrorResponse: {
        type: 'object',
        properties: {
          error: {
            type: 'string',
            example: 'UNAUTHORIZED'
          },
          message: {
            type: 'string',
            example: 'Authentication required'
          },
          details: {
            type: 'object',
            additionalProperties: true
          }
        }
      },
      TokenRequest: {
        type: 'object',
        properties: {
          grant_type: {
            type: 'string',
            example: 'client_credentials',
            enum: ['client_credentials', 'authorization_code', 'refresh_token']
          },
          client_id: {
            type: 'string',
            example: 'your-client-id'
          },
          client_secret: {
            type: 'string',
            example: 'your-client-secret'
          },
          scope: {
            type: 'string',
            example: 'read:user'
          },
          code: {
            type: 'string',
            description: 'Required for authorization_code grant'
          },
          refresh_token: {
            type: 'string',
            description: 'Required for refresh_token grant'
          }
        }
      },
      TokenResponse: {
        type: 'object',
        properties: {
          access_token: {
            type: 'string',
            example: 'eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9...'
          },
          token_type: {
            type: 'string',
            example: 'Bearer'
          },
          expires_in: {
            type: 'integer',
            example: 3600
          },
          scope: {
            type: 'string',
            example: 'read:user'
          }
        }
      },
      SessionResponse: {
        type: 'object',
        properties: {
          authenticated: {
            type: 'boolean',
            example: true
          },
          user: {
            $ref: '#/components/schemas/UserProfile'
          },
          session_id: {
            type: 'string',
            example: 'sess_123456'
          },
          expires_at: {
            type: 'string',
            format: 'date-time'
          }
        }
      },
      MetricsResponse: {
        type: 'object',
        properties: {
          auth_attempts: {
            type: 'integer',
            example: 1234
          },
          successful_logins: {
            type: 'integer',
            example: 1000
          },
          failed_logins: {
            type: 'integer',
            example: 234
          },
          active_sessions: {
            type: 'integer',
            example: 150
          },
          linked_accounts: {
            type: 'object',
            properties: {
              google: {
                type: 'integer',
                example: 100
              },
              github: {
                type: 'integer',
                example: 50
              }
            }
          }
        }
      }
    }
  },
  tags: [
    {
      name: 'health',
      description: 'Health check endpoints'
    },
    {
      name: 'auth',
      description: 'Authentication endpoints'
    },
    {
      name: 'user',
      description: 'User management endpoints'
    },
    {
      name: 'linking',
      description: 'Account linking endpoints'
    },
    {
      name: 'metrics',
      description: 'Metrics and monitoring endpoints'
    }
  ]
};

// Options for swagger-jsdoc
export const swaggerOptions = {
  definition: swaggerDefinition,
  apis: [
    './cra-server.ts',
    './routes/*.ts',
    './dist/cra-server.js',
    './dist/routes/*.js'
  ]
};

// Generate swagger specification
export const swaggerSpec = swaggerJsdoc(swaggerOptions);

// Swagger UI options
export const swaggerUiOptions: SwaggerUiOptions = {
  explorer: true,
  customSiteTitle: 'Nexpo Auth Service API Docs',
  customfavIcon: '/favicon.ico',
  customCss: `
    .swagger-ui .topbar { display: none }
    .swagger-ui .info { margin-bottom: 50px }
    .swagger-ui .scheme-container { margin-bottom: 30px }
  `
};
