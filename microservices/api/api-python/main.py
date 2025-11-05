"""
FastAPI Backend Service - Nexpo Template
Auth0 + CRA Integration with Swagger/OpenAPI Documentation
"""

import os
from contextlib import asynccontextmanager
from datetime import datetime
from typing import Optional, Dict, Any, List

from fastapi import FastAPI, HTTPException, Depends, Security, Request, status
from fastapi.middleware.cors import CORSMiddleware
from fastapi.security import HTTPBearer, HTTPAuthorizationCredentials
from fastapi.responses import JSONResponse
from pydantic import BaseModel, Field
from dotenv import load_dotenv
import httpx
from jose import jwt, JWTError
import structlog

# Load environment variables
load_dotenv()

# Configure structured logging
logger = structlog.get_logger()

# Configuration
class Settings:
    """Application settings"""
    # Server
    port: int = int(os.getenv("PYTHON_API_PORT", "7010"))
    host: str = os.getenv("HOST", "0.0.0.0")
    environment: str = os.getenv("NODE_ENV", "development")

    # Auth0
    auth0_domain: str = os.getenv("AUTH0_DOMAIN", "")
    auth0_audience: str = os.getenv("AUTH0_AUDIENCE", "https://api.nexpo.dev")
    auth0_client_id: str = os.getenv("AUTH0_CLIENT_ID", "")
    auth0_client_secret: str = os.getenv("AUTH0_CLIENT_SECRET", "")

    # MindsDB
    mindsdb_url: str = os.getenv("MINDSDB_URL", "http://localhost:4040")

    # CORS
    cors_origins: List[str] = os.getenv("CORS_ORIGINS", "http://localhost:3000").split(",")

settings = Settings()

# Security
security = HTTPBearer()

# Pydantic Models
class HealthResponse(BaseModel):
    """Health check response model"""
    status: str = Field(description="Service status")
    service: str = Field(description="Service name")
    timestamp: str = Field(description="Current timestamp")
    version: str = Field(description="API version")
    environment: str = Field(description="Environment name")

class UserProfile(BaseModel):
    """User profile model"""
    sub: str = Field(description="User ID from Auth0")
    email: Optional[str] = Field(description="User email")
    name: Optional[str] = Field(description="User display name")
    picture: Optional[str] = Field(description="User avatar URL")
    email_verified: bool = Field(default=False, description="Email verification status")
    metadata: Dict[str, Any] = Field(default_factory=dict, description="User metadata")

class ErrorResponse(BaseModel):
    """Error response model"""
    error: str = Field(description="Error code")
    message: str = Field(description="Error message")
    details: Optional[Dict[str, Any]] = Field(default=None, description="Additional error details")

class TokenResponse(BaseModel):
    """Token exchange response"""
    access_token: str = Field(description="JWT access token")
    token_type: str = Field(default="Bearer", description="Token type")
    expires_in: int = Field(description="Token expiration time in seconds")
    scope: Optional[str] = Field(description="Token scopes")

class LinkedAccount(BaseModel):
    """Linked social account"""
    provider: str = Field(description="Provider name (google, github, etc)")
    user_id: str = Field(description="User ID on the provider")
    connection: str = Field(description="Auth0 connection name")
    is_social: bool = Field(default=True, description="Is social connection")
    linked_at: str = Field(description="Link timestamp")

# Lifespan context manager
@asynccontextmanager
async def lifespan(_app: FastAPI):
    """Application lifespan events"""
    # Startup
    logger.info("FastAPI service starting",
                port=settings.port,
                environment=settings.environment)
    yield
    # Shutdown
    logger.info("FastAPI service shutting down")

# Create FastAPI app with OpenAPI documentation
app = FastAPI(
    title="Nexpo FastAPI Backend",
    description="""
    ## Nexpo FastAPI Backend Service
    
    This is a production-ready FastAPI backend implementation for the Nexpo template.
    
    ### Features:
    - 🔐 **Auth0 Integration**: Secure authentication with JWT validation
    - 🌐 **CRA Support**: Central Rank Authority for federated identity
    - 📊 **MindsDB Integration**: Unified database access
    - 📝 **Swagger/OpenAPI**: Auto-generated API documentation
    - 🚀 **High Performance**: Async Python with uvicorn
    - 🔧 **Type Safety**: Full Pydantic model validation
    
    ### Authentication:
    All protected endpoints require a valid JWT token from Auth0.
    Include the token in the Authorization header: `Bearer <token>`
    """,
    version="1.0.0",
    docs_url="/docs",
    redoc_url="/redoc",
    openapi_url="/openapi.json",
    lifespan=lifespan,
    servers=[
        {"url": "http://localhost:7010", "description": "Development server"},
        {"url": "http://localhost:8000/api/python", "description": "Behind Kong Gateway"},
    ],
)

# Configure CORS
app.add_middleware(
    CORSMiddleware,
    allow_origins=settings.cors_origins,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
    expose_headers=["X-Request-ID", "X-Kong-Request-ID"],
)

# JWT Validation
async def get_jwks():
    """Get JWKS from Auth0"""
    async with httpx.AsyncClient() as client:
        response = await client.get(f"https://{settings.auth0_domain}/.well-known/jwks.json")
        return response.json()

async def verify_token(credentials: HTTPAuthorizationCredentials = Security(security)) -> Dict[str, Any]:
    """Verify JWT token from Auth0"""
    token = credentials.credentials
    
    try:
        # Get JWKS
        jwks = await get_jwks()
    # Decode and verify token
        unverified_header = jwt.get_unverified_header(token)
        rsa_key = {}
    for key in jwks["keys"]:
            if key["kid"] == unverified_header["kid"]:
                rsa_key = {
                    "kty": key["kty"],
                    "kid": key["kid"],
                    "use": key["use"],
                    "n": key["n"],
                    "e": key["e"]
                }
    if rsa_key:
            payload = jwt.decode(
                token,
                rsa_key,
                algorithms=["RS256"],
                audience=settings.auth0_audience,
                issuer=f"https://{settings.auth0_domain}/"
            )
            return payload
        else:
            raise HTTPException(
                status_code=status.HTTP_401_UNAUTHORIZED,
                detail="Unable to find appropriate key"
            )
    except JWTError as e:
        logger.error("JWT validation failed", error=str(e))
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Invalid authentication credentials"
        ) from e

# Routes
@app.get("/", response_model=HealthResponse, tags=["Health"])
async def root():
    """Root endpoint - health check"""
    return HealthResponse(
        status="healthy",
        service="nexpo-fastapi",
        timestamp=datetime.utcnow().isoformat(),
        version="1.0.0",
        environment=settings.environment
    )

@app.get("/health", response_model=HealthResponse, tags=["Health"])
async def health_check():
    """Detailed health check endpoint"""
    return HealthResponse(
        status="healthy",
        service="nexpo-fastapi",
        timestamp=datetime.utcnow().isoformat(),
        version="1.0.0",
        environment=settings.environment
    )

@app.get("/api/user/profile", 
         response_model=UserProfile,
         responses={
             401: {"model": ErrorResponse, "description": "Unauthorized"},
             500: {"model": ErrorResponse, "description": "Internal server error"}
         },
         tags=["User"])
async def get_user_profile(
    current_user: Dict[str, Any] = Depends(verify_token)
) -> UserProfile:
    """
    Get current user profile
    
    Requires valid Auth0 JWT token.
    Returns user profile information from the token claims.
    """
    return UserProfile(
        sub=current_user.get("sub", ""),
        email=current_user.get("email"),
        name=current_user.get("name"),
        picture=current_user.get("picture"),
        email_verified=current_user.get("email_verified", False),
        metadata=current_user.get("metadata", {})
    )

@app.get("/api/user/linked-accounts",
         response_model=List[LinkedAccount],
         responses={
             401: {"model": ErrorResponse, "description": "Unauthorized"},
             500: {"model": ErrorResponse, "description": "Internal server error"}
         },
         tags=["User"])
async def get_linked_accounts(
    _current_user: Dict[str, Any] = Depends(verify_token)
) -> List[LinkedAccount]:
    """
    Get user's linked social accounts
    
    Returns a list of all social accounts linked to this user's CRA identity.
    """
    # In production, this would query Auth0 Management API or user metadata
    # For demo, return mock data
    return [
        LinkedAccount(
            provider="google",
            user_id="google-oauth2|123456789",
            connection="google-oauth2",
            is_social=True,
            linked_at=datetime.utcnow().isoformat()
        )
    ]

@app.post("/api/auth/token",
          response_model=TokenResponse,
          responses={
              400: {"model": ErrorResponse, "description": "Bad request"},
              401: {"model": ErrorResponse, "description": "Invalid credentials"}
          },
          tags=["Auth"])
async def exchange_token(
    _request: Request,
    _grant_type: str = "client_credentials",
    scope: Optional[str] = None
) -> TokenResponse:
    """
    Exchange credentials for access token
    
    This endpoint demonstrates token exchange for service-to-service auth.
    In production, implement proper OAuth2 flows.
    """
    # This is a simplified example
    # In production, validate credentials and generate proper tokens
    return TokenResponse(
        access_token="mock_access_token",
        token_type="Bearer",
        expires_in=3600,
        scope=scope or "read:user"
    )

@app.get("/api/data/query",
         responses={
             401: {"model": ErrorResponse, "description": "Unauthorized"},
             500: {"model": ErrorResponse, "description": "MindsDB error"}
         },
         tags=["Data"])
async def query_mindsdb(
    query: str,
    current_user: Dict[str, Any] = Depends(verify_token)
) -> Dict[str, Any]:
    """
    Execute query through MindsDB gateway
    
    Allows querying any connected database through MindsDB's unified interface.
    Requires valid authentication.
    """
    try:
        async with httpx.AsyncClient() as client:
            response = await client.post(
                f"{settings.mindsdb_url}/api/sql/query",
                json={"query": query},
                headers={
                    "Authorization": f"Bearer {current_user.get('sub')}",
                    "X-User-ID": current_user.get("sub", "")
                }
            )
            return response.json()
    except Exception as e:
        logger.error("MindsDB query failed", error=str(e))
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail="Failed to execute query"
        ) from e

# Exception handlers
@app.exception_handler(HTTPException)
async def http_exception_handler(request: Request, exc: HTTPException):
    """Handle HTTP exceptions"""
    return JSONResponse(
        status_code=exc.status_code,
        content={
            "error": f"HTTP_{exc.status_code}",
            "message": exc.detail,
            "path": str(request.url.path)
        }
    )

@app.exception_handler(Exception)
async def general_exception_handler(request: Request, exc: Exception):
    """Handle general exceptions"""
    logger.error("Unhandled exception", error=str(exc), path=str(request.url.path))
    return JSONResponse(
        status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
        content={
            "error": "INTERNAL_SERVER_ERROR",
            "message": "An unexpected error occurred",
            "path": str(request.url.path)
        }
    )

# Run with: uvicorn main:app --reload --port 7010
if __name__ == "__main__":
    import uvicorn
    uvicorn.run(
        "main:app",
        host=settings.host,
        port=settings.port,
        reload=settings.environment == "development",
        log_level="info"
    )
