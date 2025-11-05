# Nexpo R API with Plumber
# API backend implementing the standard Nexpo API specification

#* @apiTitle Nexpo R API
#* @apiDescription R API backend with Auth0 JWT validation and MindsDB integration
#* @apiVersion 1.0.0
#* @apiBasePath /
#* @apiTag Health Health check endpoints
#* @apiTag Auth Authentication endpoints
#* @apiTag User User management endpoints
#* @apiTag Data Data query endpoints

library(plumber)
library(jose)
library(httr)
library(jsonlite)
library(logger)

# Load environment variables
if (file.exists(".env")) {
  readRenviron(".env")
}

# Configuration
config <- list(
  port = as.integer(Sys.getenv("R_API_PORT", "7060")),
  auth0_domain = Sys.getenv("AUTH0_DOMAIN"),
  auth0_audience = Sys.getenv("AUTH0_AUDIENCE"),
  auth0_client_id = Sys.getenv("AUTH0_CLIENT_ID"),
  auth0_client_secret = Sys.getenv("AUTH0_CLIENT_SECRET"),
  mindsdb_url = Sys.getenv("MINDSDB_URL", "http://localhost:47334"),
  mindsdb_timeout = as.integer(Sys.getenv("MINDSDB_TIMEOUT", "30")),
  cors_origins = Sys.getenv("CORS_ORIGINS", "*"),
  log_level = Sys.getenv("LOG_LEVEL", "INFO")
)

# Set up logging
log_appender(appender_console)
log_threshold(config$log_level)

# JWT validation function
validate_jwt <- function(token) {
  tryCatch({
    # Remove "Bearer " prefix if present
    if (startsWith(token, "Bearer ")) {
      token <- substring(token, 8)
    }
    
    # Get JWKS from Auth0
    jwks_url <- paste0("https://", config$auth0_domain, "/.well-known/jwks.json")
    jwks_response <- GET(jwks_url)
    jwks <- content(jwks_response, "parsed")
    
    # Decode and verify JWT
    jwt_claims <- jwt_decode_sig(token, jwks)
    
    # Verify audience and issuer
    if (jwt_claims$aud != config$auth0_audience) {
      stop("Invalid audience")
    }
    
    if (jwt_claims$iss != paste0("https://", config$auth0_domain, "/")) {
      stop("Invalid issuer")
    }
    
    # Check expiration
    if (jwt_claims$exp < as.numeric(Sys.time())) {
      stop("Token expired")
    }
    
    return(jwt_claims)
  }, error = function(e) {
    log_error("JWT validation failed: {e$message}")
    return(NULL)
  })
}

# Auth filter
auth_filter <- function(req, res) {
  # Skip auth for health and docs endpoints
  if (grepl("^/(health|swagger|docs|api/auth/token)", req$PATH_INFO)) {
    plumber::forward()
  }
  
  # Check for Authorization header
  auth_header <- req$HTTP_AUTHORIZATION
  if (is.null(auth_header)) {
    res$status <- 401
    return(list(
      error = "Unauthorized",
      message = "Missing authorization header"
    ))
  }
  
  # Validate JWT
  claims <- validate_jwt(auth_header)
  if (is.null(claims)) {
    res$status <- 401
    return(list(
      error = "Unauthorized",
      message = "Invalid token"
    ))
  }
  
  # Attach claims to request
  req$jwt_claims <- claims
  plumber::forward()
}

#* Enable CORS
#* @filter cors
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", config$cors_origins)
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }
  
  plumber::forward()
}

#* Health check endpoint
#* @get /
#* @get /health
#* @tag Health
#* @response 200 A JSON object with service information
#* @serializer json
function() {
  log_info("Health check requested")
  list(
    status = "healthy",
    service = "nexpo-api-r",
    version = "1.0.0",
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
}

#* Exchange authorization code for tokens
#* @post /api/auth/token
#* @tag Auth
#* @param grant_type:str The grant type (authorization_code or refresh_token)
#* @param code:str The authorization code (for authorization_code grant)
#* @param redirect_uri:str The redirect URI (for authorization_code grant)
#* @param refresh_token:str The refresh token (for refresh_token grant)
#* @response 200 Token response
#* @response 400 Bad request
#* @serializer json
function(req, grant_type = NULL, code = NULL, redirect_uri = NULL, refresh_token = NULL) {
  log_info("Token exchange requested")
  
  # Parse JSON body if present
  if (!is.null(req$postBody)) {
    body <- fromJSON(req$postBody)
    grant_type <- body$grant_type %||% grant_type
    code <- body$code %||% code
    redirect_uri <- body$redirect_uri %||% redirect_uri
    refresh_token <- body$refresh_token %||% refresh_token
  }
  
  # Validate grant type
  if (is.null(grant_type) || !(grant_type %in% c("authorization_code", "refresh_token"))) {
    res$status <- 400
    return(list(
      error = "invalid_request",
      error_description = "Invalid or missing grant_type"
    ))
  }
  
  # Prepare Auth0 request
  token_url <- paste0("https://", config$auth0_domain, "/oauth/token")
  
  if (grant_type == "authorization_code") {
    body <- list(
      grant_type = "authorization_code",
      client_id = config$auth0_client_id,
      client_secret = config$auth0_client_secret,
      code = code,
      redirect_uri = redirect_uri
    )
  } else {
    body <- list(
      grant_type = "refresh_token",
      client_id = config$auth0_client_id,
      client_secret = config$auth0_client_secret,
      refresh_token = refresh_token
    )
  }
  
  # Make request to Auth0
  response <- POST(
    token_url,
    body = body,
    encode = "json",
    add_headers("Content-Type" = "application/json")
  )
  
  if (status_code(response) != 200) {
    res$status <- status_code(response)
    return(content(response, "parsed"))
  }
  
  return(content(response, "parsed"))
}

#* Get user profile
#* @get /api/user/profile
#* @tag User
#* @preempt auth_filter
#* @response 200 User profile
#* @response 401 Unauthorized
#* @serializer json
function(req) {
  log_info("User profile requested")
  
  claims <- req$jwt_claims
  
  list(
    sub = claims$sub,
    name = claims$name %||% "Unknown User",
    email = claims$email %||% NULL,
    picture = claims$picture %||% NULL,
    metadata = list(
      created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      last_login = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      login_count = 1
    )
  )
}

#* Get linked accounts
#* @get /api/user/linked-accounts
#* @tag User
#* @preempt auth_filter
#* @response 200 List of linked accounts
#* @response 401 Unauthorized
#* @serializer json
function(req) {
  log_info("Linked accounts requested")
  
  list(
    accounts = list(
      list(
        provider = "google",
        user_id = "google|123456789",
        linked = TRUE,
        linked_at = format(Sys.time() - 86400 * 30, "%Y-%m-%dT%H:%M:%S%z")
      ),
      list(
        provider = "github",
        user_id = "github|987654321",
        linked = TRUE,
        linked_at = format(Sys.time() - 86400 * 15, "%Y-%m-%dT%H:%M:%S%z")
      )
    ),
    total = 2
  )
}

#* Execute MindsDB query
#* @post /api/data/query
#* @tag Data
#* @preempt auth_filter
#* @param query:str The SQL query to execute
#* @param parameters:object Optional query parameters
#* @param timeout:int Query timeout in seconds
#* @response 200 Query results
#* @response 400 Bad request
#* @response 500 Server error
#* @serializer json
function(req, res, query = NULL, parameters = NULL, timeout = NULL) {
  log_info("MindsDB query requested")
  
  # Parse JSON body
  if (!is.null(req$postBody)) {
    body <- fromJSON(req$postBody)
    query <- body$query %||% query
    parameters <- body$parameters %||% parameters
    timeout <- body$timeout %||% timeout
  }
  
  if (is.null(query) || query == "") {
    res$status <- 400
    return(list(
      error = "bad_request",
      message = "Query is required"
    ))
  }
  
  # Use configured timeout if not specified
  if (is.null(timeout)) {
    timeout <- config$mindsdb_timeout
  }
  
  log_info("Executing MindsDB query: {query}")
  
  tryCatch({
    start_time <- Sys.time()
    
    # Prepare MindsDB request
    mindsdb_request <- list(
      query = query
    )
    
    if (!is.null(parameters)) {
      mindsdb_request$parameters <- parameters
    }
    
    # Execute query
    response <- POST(
      paste0(config$mindsdb_url, "/api/sql/query"),
      body = mindsdb_request,
      encode = "json",
      timeout(timeout),
      add_headers("Content-Type" = "application/json")
    )
    
    execution_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    
    if (status_code(response) != 200) {
      res$status <- status_code(response)
      return(list(
        error = "mindsdb_error",
        message = "Query execution failed",
        details = content(response, "text")
      ))
    }
    
    result <- content(response, "parsed")
    
    # Format response
    list(
      data = result$data %||% list(),
      count = length(result$data %||% list()),
      execution_time = round(execution_time * 1000), # Convert to milliseconds
      metadata = list(
        query = query,
        timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
      )
    )
    
  }, error = function(e) {
    log_error("MindsDB query error: {e$message}")
    res$status <- 500
    list(
      error = "server_error",
      message = "Query execution failed",
      details = e$message
    )
  })
}

# Create and configure the API
pr <- plumber$new()

# Add CORS filter
pr$filter("cors", cors)

# Add auth filter  
pr$handle(auth_filter)

# Mount the API endpoints defined above
pr$mount("/", plumb("api.R"))

# Custom 404 handler
pr$set404Handler(function(req, res) {
  res$status <- 404
  list(
    error = "not_found",
    message = "Endpoint not found",
    path = req$PATH_INFO
  )
})

# Custom error handler
pr$setErrorHandler(function(req, res, err) {
  log_error("Unhandled error: {err$message}")
  res$status <- 500
  list(
    error = "internal_server_error",
    message = "An unexpected error occurred"
  )
})

# Run the API
log_info("Starting Nexpo R API on port {config$port}")
pr$run(host = "0.0.0.0", port = config$port)
