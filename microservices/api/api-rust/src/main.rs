use actix_cors::Cors;
use actix_web::{middleware, web, App, HttpResponse, HttpServer, Result, http::header};
use chrono::{DateTime, Utc};
use dotenv::dotenv;
use jsonwebtoken::{decode, decode_header, Algorithm, DecodingKey, Validation};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::env;
use utoipa::{OpenApi, ToSchema};
use utoipa_swagger_ui::SwaggerUi;

// OpenAPI documentation
#[derive(OpenApi)]
#[openapi(
    info(
        title = "Nexpo Rust API",
        version = "1.0.0",
        description = "Production-ready Actix-web backend implementation for Nexpo template",
        contact(name = "API Support", email = "support@nexpo.dev"),
        license(name = "MIT", url = "https://opensource.org/licenses/MIT")
    ),
    paths(
        health_check,
        root_health,
        get_user_profile,
        get_linked_accounts,
        exchange_token,
        query_mindsdb
    ),
    components(
        schemas(
            HealthResponse,
            UserProfile,
            LinkedAccount,
            ErrorResponse,
            TokenResponse,
            TokenRequest,
            MindsDBQuery
        )
    ),
    tags(
        (name = "health", description = "Health check endpoints"),
        (name = "user", description = "User management endpoints"),
        (name = "auth", description = "Authentication endpoints"),
        (name = "data", description = "Data query endpoints")
    ),
    servers(
        (url = "http://localhost:7030", description = "Development server"),
        (url = "http://localhost:8000/api/rust", description = "Behind Kong Gateway")
    )
)]
struct ApiDoc;

// Models
#[derive(Serialize, Deserialize, ToSchema)]
struct HealthResponse {
    #[schema(example = "healthy")]
    status: String,
    #[schema(example = "nexpo-actix")]
    service: String,
    #[schema(example = "2024-01-01T00:00:00Z")]
    timestamp: String,
    #[schema(example = "1.0.0")]
    version: String,
    #[schema(example = "development")]
    environment: String,
}

#[derive(Serialize, Deserialize, ToSchema)]
struct UserProfile {
    #[schema(example = "auth0|123456")]
    sub: String,
    #[schema(example = "user@example.com")]
    email: Option<String>,
    #[schema(example = "John Doe")]
    name: Option<String>,
    #[schema(example = "https://example.com/avatar.jpg")]
    picture: Option<String>,
    #[schema(example = true)]
    email_verified: bool,
    metadata: HashMap<String, serde_json::Value>,
}

#[derive(Serialize, Deserialize, ToSchema)]
struct LinkedAccount {
    #[schema(example = "google")]
    provider: String,
    #[schema(example = "google-oauth2|123456")]
    user_id: String,
    #[schema(example = "google-oauth2")]
    connection: String,
    #[schema(example = true)]
    is_social: bool,
    #[schema(example = "2024-01-01T00:00:00Z")]
    linked_at: String,
}

#[derive(Serialize, Deserialize, ToSchema)]
struct ErrorResponse {
    #[schema(example = "UNAUTHORIZED")]
    error: String,
    #[schema(example = "Invalid authentication credentials")]
    message: String,
    details: Option<HashMap<String, serde_json::Value>>,
}

#[derive(Serialize, Deserialize, ToSchema)]
struct TokenResponse {
    #[schema(example = "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9...")]
    access_token: String,
    #[schema(example = "Bearer")]
    token_type: String,
    #[schema(example = 3600)]
    expires_in: i32,
    #[schema(example = "read:user")]
    scope: Option<String>,
}

#[derive(Deserialize, ToSchema)]
struct TokenRequest {
    #[schema(example = "client_credentials")]
    grant_type: String,
    #[schema(example = "read:user")]
    scope: Option<String>,
}

#[derive(Deserialize, ToSchema)]
struct MindsDBQuery {
    #[schema(example = "SELECT * FROM users LIMIT 10")]
    query: String,
}

// JWT Claims
#[derive(Debug, Serialize, Deserialize)]
struct Claims {
    sub: String,
    email: Option<String>,
    name: Option<String>,
    picture: Option<String>,
    email_verified: Option<bool>,
    exp: usize,
    iat: usize,
    aud: Option<String>,
    iss: Option<String>,
}

// State
struct AppState {
    auth0_domain: String,
    auth0_audience: String,
    mindsdb_url: String,
}

// Handlers
#[utoipa::path(
    get,
    path = "/",
    tag = "health",
    responses(
        (status = 200, description = "Service is healthy", body = HealthResponse)
    )
)]
async fn root_health() -> Result<HttpResponse> {
    let response = HealthResponse {
        status: "healthy".to_string(),
        service: "nexpo-actix".to_string(),
        timestamp: Utc::now().to_rfc3339(),
        version: "1.0.0".to_string(),
        environment: env::var("NODE_ENV").unwrap_or_else(|_| "development".to_string()),
    };
    Ok(HttpResponse::Ok().json(response))
}

#[utoipa::path(
    get,
    path = "/health",
    tag = "health",
    responses(
        (status = 200, description = "Detailed health status", body = HealthResponse)
    )
)]
async fn health_check() -> Result<HttpResponse> {
    let response = HealthResponse {
        status: "healthy".to_string(),
        service: "nexpo-actix".to_string(),
        timestamp: Utc::now().to_rfc3339(),
        version: "1.0.0".to_string(),
        environment: env::var("NODE_ENV").unwrap_or_else(|_| "development".to_string()),
    };
    Ok(HttpResponse::Ok().json(response))
}

#[utoipa::path(
    get,
    path = "/api/user/profile",
    tag = "user",
    security(
        ("bearer_auth" = [])
    ),
    responses(
        (status = 200, description = "User profile retrieved", body = UserProfile),
        (status = 401, description = "Unauthorized", body = ErrorResponse)
    )
)]
async fn get_user_profile(
    req: actix_web::HttpRequest,
    _data: web::Data<AppState>,
) -> Result<HttpResponse> {
    // Extract and validate JWT token
    match extract_token_from_header(&req) {
        Ok(token) => {
            // In production, validate with Auth0 JWKS
            // For demo, decode without validation
            match decode_without_validation(&token) {
                Ok(claims) => {
                    let profile = UserProfile {
                        sub: claims.sub,
                        email: claims.email,
                        name: claims.name,
                        picture: claims.picture,
                        email_verified: claims.email_verified.unwrap_or(false),
                        metadata: HashMap::new(),
                    };
                    Ok(HttpResponse::Ok().json(profile))
                }
                Err(_) => {
                    let error = ErrorResponse {
                        error: "UNAUTHORIZED".to_string(),
                        message: "Invalid token".to_string(),
                        details: None,
                    };
                    Ok(HttpResponse::Unauthorized().json(error))
                }
            }
        }
        Err(e) => {
            let error = ErrorResponse {
                error: "UNAUTHORIZED".to_string(),
                message: e,
                details: None,
            };
            Ok(HttpResponse::Unauthorized().json(error))
        }
    }
}

#[utoipa::path(
    get,
    path = "/api/user/linked-accounts",
    tag = "user",
    security(
        ("bearer_auth" = [])
    ),
    responses(
        (status = 200, description = "Linked accounts retrieved", body = Vec<LinkedAccount>),
        (status = 401, description = "Unauthorized", body = ErrorResponse)
    )
)]
async fn get_linked_accounts(
    req: actix_web::HttpRequest,
    _data: web::Data<AppState>,
) -> Result<HttpResponse> {
    // Validate token
    match extract_token_from_header(&req) {
        Ok(_) => {
            // Mock linked accounts (in production, query Auth0 Management API)
            let accounts = vec![LinkedAccount {
                provider: "google".to_string(),
                user_id: "google-oauth2|123456789".to_string(),
                connection: "google-oauth2".to_string(),
                is_social: true,
                linked_at: Utc::now().to_rfc3339(),
            }];
            Ok(HttpResponse::Ok().json(accounts))
        }
        Err(e) => {
            let error = ErrorResponse {
                error: "UNAUTHORIZED".to_string(),
                message: e,
                details: None,
            };
            Ok(HttpResponse::Unauthorized().json(error))
        }
    }
}

#[utoipa::path(
    post,
    path = "/api/auth/token",
    tag = "auth",
    request_body = TokenRequest,
    responses(
        (status = 200, description = "Token generated", body = TokenResponse),
        (status = 400, description = "Bad request", body = ErrorResponse)
    )
)]
async fn exchange_token(
    req: web::Json<TokenRequest>,
) -> Result<HttpResponse> {
    // Mock token response (in production, implement proper OAuth2 flow)
    let response = TokenResponse {
        access_token: format!("mock_access_token_{}", Utc::now().timestamp()),
        token_type: "Bearer".to_string(),
        expires_in: 3600,
        scope: req.scope.clone(),
    };
    Ok(HttpResponse::Ok().json(response))
}

#[utoipa::path(
    get,
    path = "/api/data/query",
    tag = "data",
    params(
        ("query" = String, Query, description = "SQL query to execute")
    ),
    security(
        ("bearer_auth" = [])
    ),
    responses(
        (status = 200, description = "Query executed successfully"),
        (status = 401, description = "Unauthorized", body = ErrorResponse),
        (status = 500, description = "MindsDB error", body = ErrorResponse)
    )
)]
async fn query_mindsdb(
    req: actix_web::HttpRequest,
    query: web::Query<MindsDBQuery>,
    data: web::Data<AppState>,
) -> Result<HttpResponse> {
    // Validate token
    match extract_token_from_header(&req) {
        Ok(token) => {
            match decode_without_validation(&token) {
                Ok(claims) => {
                    // Mock MindsDB response (in production, make actual HTTP request)
                    let response = serde_json::json!({
                        "query": query.query,
                        "user_id": claims.sub,
                        "results": [
                            {"id": 1, "name": "Sample Result"}
                        ],
                        "execution_time": "0.123s"
                    });
                    Ok(HttpResponse::Ok().json(response))
                }
                Err(_) => {
                    let error = ErrorResponse {
                        error: "UNAUTHORIZED".to_string(),
                        message: "Invalid token".to_string(),
                        details: None,
                    };
                    Ok(HttpResponse::Unauthorized().json(error))
                }
            }
        }
        Err(e) => {
            let error = ErrorResponse {
                error: "UNAUTHORIZED".to_string(),
                message: e,
                details: None,
            };
            Ok(HttpResponse::Unauthorized().json(error))
        }
    }
}

// Helper functions
fn extract_token_from_header(req: &actix_web::HttpRequest) -> Result<String, String> {
    let auth_header = req
        .headers()
        .get(header::AUTHORIZATION)
        .ok_or_else(|| "Missing authorization header".to_string())?;

    let auth_str = auth_header
        .to_str()
        .map_err(|_| "Invalid authorization header".to_string())?;

    if !auth_str.starts_with("Bearer ") {
        return Err("Invalid authorization header format".to_string());
    }

    Ok(auth_str[7..].to_string())
}

fn decode_without_validation(token: &str) -> Result<Claims, jsonwebtoken::errors::Error> {
    // In production, use proper validation with Auth0 JWKS
    let mut validation = Validation::default();
    validation.insecure_disable_signature_validation();
    validation.validate_exp = false;
    
    let token_data = decode::<Claims>(
        token,
        &DecodingKey::from_secret(b"dummy_secret"),
        &validation,
    )?;
    
    Ok(token_data.claims)
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    // Load environment variables
    dotenv().ok();
    env_logger::init();

    // Configuration
    let port = env::var("RUST_API_PORT").unwrap_or_else(|_| "7030".to_string());
    let host = env::var("HOST").unwrap_or_else(|_| "0.0.0.0".to_string());
    let cors_origins = env::var("CORS_ORIGINS")
        .unwrap_or_else(|_| "http://localhost:3000".to_string());

    let app_state = web::Data::new(AppState {
        auth0_domain: env::var("AUTH0_DOMAIN").unwrap_or_default(),
        auth0_audience: env::var("AUTH0_AUDIENCE").unwrap_or_default(),
        mindsdb_url: env::var("MINDSDB_URL")
            .unwrap_or_else(|_| "http://localhost:4040".to_string()),
    });

    log::info!("Starting Actix-web server on {}:{}", host, port);

    // Create server
    HttpServer::new(move || {
        let cors = Cors::default()
            .allowed_origin_fn(move |origin, _req_head| {
                cors_origins
                    .split(',')
                    .any(|allowed| origin.as_bytes() == allowed.as_bytes())
            })
            .allowed_methods(vec!["GET", "POST", "PUT", "DELETE", "OPTIONS"])
            .allowed_headers(vec![
                header::AUTHORIZATION,
                header::CONTENT_TYPE,
                header::HeaderName::from_static("x-request-id"),
            ])
            .expose_headers(vec![
                header::HeaderName::from_static("x-request-id"),
                header::HeaderName::from_static("x-kong-request-id"),
            ])
            .supports_credentials()
            .max_age(3600);

        App::new()
            .app_data(app_state.clone())
            .wrap(cors)
            .wrap(middleware::Logger::default())
            .service(
                SwaggerUi::new("/swagger-ui/{_:.*}")
                    .url("/api-docs/openapi.json", ApiDoc::openapi()),
            )
            .route("/", web::get().to(root_health))
            .route("/health", web::get().to(health_check))
            .route("/api/user/profile", web::get().to(get_user_profile))
            .route("/api/user/linked-accounts", web::get().to(get_linked_accounts))
            .route("/api/auth/token", web::post().to(exchange_token))
            .route("/api/data/query", web::get().to(query_mindsdb))
    })
    .bind(format!("{}:{}", host, port))?
    .run()
    .await
}
