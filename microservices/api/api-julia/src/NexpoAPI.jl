module NexpoAPI

using Genie, Genie.Router, Genie.Renderer.Json, Genie.Requests
using HTTP, JSON3, JSONWebTokens, Dates, Base64
using Logging

# Load environment variables
using DotEnv
DotEnv.config()

# Configuration
const CONFIG = Dict(
    :port => parse(Int, get(ENV, "JULIA_API_PORT", "7070")),
    :auth0_domain => get(ENV, "AUTH0_DOMAIN", ""),
    :auth0_audience => get(ENV, "AUTH0_AUDIENCE", ""),
    :auth0_client_id => get(ENV, "AUTH0_CLIENT_ID", ""),
    :auth0_client_secret => get(ENV, "AUTH0_CLIENT_SECRET", ""),
    :mindsdb_url => get(ENV, "MINDSDB_URL", "http://localhost:47334"),
    :mindsdb_timeout => parse(Int, get(ENV, "MINDSDB_TIMEOUT", "30")),
    :cors_origins => get(ENV, "CORS_ORIGINS", "*"),
    :log_level => get(ENV, "LOG_LEVEL", "INFO")
)

# Set up logging
logger = ConsoleLogger(stdout, Logging.Info)
global_logger(logger)

# JWT validation
mutable struct JWTClaims
    sub::String
    name::Union{String, Nothing}
    email::Union{String, Nothing}
    picture::Union{String, Nothing}
    aud::String
    iss::String
    exp::Int64
    iat::Int64
end

function get_jwks()
    url = "https://$(CONFIG[:auth0_domain])/.well-known/jwks.json"
    response = HTTP.get(url)
    return JSON3.read(response.body)
end

function validate_jwt(token::String)
    try
        # Remove "Bearer " prefix if present
        if startswith(token, "Bearer ")
            token = token[8:end]
        end
        
        # Get JWKS
        jwks = get_jwks()
        
        # Decode and verify JWT
        # Note: In production, use proper JWT validation with RS256
        # This is a simplified version for demonstration
        parts = split(token, ".")
        if length(parts) != 3
            return nothing
        end
        
        # Decode claims
        claims_json = String(base64decode(parts[2] * "=="))
        claims = JSON3.read(claims_json)
        
        # Verify audience and issuer
        if claims.aud != CONFIG[:auth0_audience]
            @error "Invalid audience"
            return nothing
        end
        
        if claims.iss != "https://$(CONFIG[:auth0_domain])/"
            @error "Invalid issuer"
            return nothing
        end
        
        # Check expiration
        if claims.exp < time()
            @error "Token expired"
            return nothing
        end
        
        return claims
    catch e
        @error "JWT validation failed" exception=e
        return nothing
    end
end

# Auth middleware
function auth_middleware()
    # Skip auth for public endpoints
    if in(Genie.Router.params(:REQUEST).target, ["/", "/health", "/api/auth/token"])
        return
    end
    
    # Check for Authorization header
    auth_header = Genie.Requests.getheader("Authorization")
    if isempty(auth_header)
        return Json.json(Dict(
            "error" => "Unauthorized",
            "message" => "Missing authorization header"
        )), 401
    end
    
    # Validate JWT
    claims = validate_jwt(auth_header)
    if isnothing(claims)
        return Json.json(Dict(
            "error" => "Unauthorized",
            "message" => "Invalid token"
        )), 401
    end
    
    # Attach claims to request
    Genie.Router.params(:jwt_claims, claims)
end

# CORS headers
function cors_headers()
    headers = Dict(
        "Access-Control-Allow-Origin" => CONFIG[:cors_origins],
        "Access-Control-Allow-Methods" => "GET, POST, PUT, DELETE, OPTIONS",
        "Access-Control-Allow-Headers" => "Content-Type, Authorization"
    )
    
    for (key, value) in headers
        Genie.Renderer.setheader(key, value)
    end
end

# Routes
route("/", method = GET) do
    cors_headers()
    @info "Health check requested"
    Json.json(Dict(
        "status" => "healthy",
        "service" => "nexpo-api-julia",
        "version" => "1.0.0",
        "timestamp" => Dates.format(now(), "yyyy-mm-ddTHH:MM:SS") * "Z"
    ))
end

route("/health", method = GET) do
    cors_headers()
    @info "Health check requested"
    Json.json(Dict(
        "status" => "healthy",
        "service" => "nexpo-api-julia",
        "version" => "1.0.0",
        "timestamp" => Dates.format(now(), "yyyy-mm-ddTHH:MM:SS") * "Z"
    ))
end

# Handle OPTIONS requests for CORS
route("/*", method = OPTIONS) do
    cors_headers()
    return "", 200
end

route("/api/auth/token", method = POST) do
    cors_headers()
    @info "Token exchange requested"
    
    try
        payload = jsonpayload()
        grant_type = get(payload, "grant_type", nothing)
        
        if isnothing(grant_type) || !(grant_type in ["authorization_code", "refresh_token"])
            return Json.json(Dict(
                "error" => "invalid_request",
                "error_description" => "Invalid or missing grant_type"
            )), 400
        end
        
        # Prepare Auth0 request
        token_url = "https://$(CONFIG[:auth0_domain])/oauth/token"
        
        if grant_type == "authorization_code"
            body = Dict(
                "grant_type" => "authorization_code",
                "client_id" => CONFIG[:auth0_client_id],
                "client_secret" => CONFIG[:auth0_client_secret],
                "code" => get(payload, "code", ""),
                "redirect_uri" => get(payload, "redirect_uri", "")
            )
        else
            body = Dict(
                "grant_type" => "refresh_token",
                "client_id" => CONFIG[:auth0_client_id],
                "client_secret" => CONFIG[:auth0_client_secret],
                "refresh_token" => get(payload, "refresh_token", "")
            )
        end
        
        # Make request to Auth0
        response = HTTP.post(
            token_url,
            ["Content-Type" => "application/json"],
            JSON3.write(body)
        )
        
        return Json.json(JSON3.read(response.body))
        
    catch e
        @error "Token exchange error" exception=e
        if isa(e, HTTP.ExceptionRequest.StatusError)
            return Json.json(JSON3.read(e.response.body)), e.response.status
        end
        return Json.json(Dict(
            "error" => "server_error",
            "message" => "Token exchange failed"
        )), 500
    end
end

route("/api/user/profile", method = GET, before = auth_middleware) do
    cors_headers()
    @info "User profile requested"
    
    claims = params(:jwt_claims)
    
    Json.json(Dict(
        "sub" => claims.sub,
        "name" => something(claims.name, "Unknown User"),
        "email" => claims.email,
        "picture" => claims.picture,
        "metadata" => Dict(
            "created_at" => Dates.format(now(), "yyyy-mm-ddTHH:MM:SS") * "Z",
            "last_login" => Dates.format(now(), "yyyy-mm-ddTHH:MM:SS") * "Z",
            "login_count" => 1
        )
    ))
end

route("/api/user/linked-accounts", method = GET, before = auth_middleware) do
    cors_headers()
    @info "Linked accounts requested"
    
    Json.json(Dict(
        "accounts" => [
            Dict(
                "provider" => "google",
                "user_id" => "google|123456789",
                "linked" => true,
                "linked_at" => Dates.format(now() - Day(30), "yyyy-mm-ddTHH:MM:SS") * "Z"
            ),
            Dict(
                "provider" => "github",
                "user_id" => "github|987654321",
                "linked" => true,
                "linked_at" => Dates.format(now() - Day(15), "yyyy-mm-ddTHH:MM:SS") * "Z"
            )
        ],
        "total" => 2
    ))
end

route("/api/data/query", method = POST, before = auth_middleware) do
    cors_headers()
    @info "MindsDB query requested"
    
    try
        payload = jsonpayload()
        query = get(payload, "query", nothing)
        
        if isnothing(query) || isempty(query)
            return Json.json(Dict(
                "error" => "bad_request",
                "message" => "Query is required"
            )), 400
        end
        
        parameters = get(payload, "parameters", Dict())
        timeout = get(payload, "timeout", CONFIG[:mindsdb_timeout])
        
        @info "Executing MindsDB query: $query"
        
        start_time = time()
        
        # Prepare MindsDB request
        mindsdb_request = Dict("query" => query)
        if !isempty(parameters)
            mindsdb_request["parameters"] = parameters
        end
        
        # Execute query
        response = HTTP.post(
            "$(CONFIG[:mindsdb_url])/api/sql/query",
            ["Content-Type" => "application/json"],
            JSON3.write(mindsdb_request),
            readtimeout = timeout
        )
        
        execution_time = round(Int, (time() - start_time) * 1000)  # Convert to milliseconds
        
        if response.status != 200
            return Json.json(Dict(
                "error" => "mindsdb_error",
                "message" => "Query execution failed",
                "details" => String(response.body)
            )), response.status
        end
        
        result = JSON3.read(response.body)
        
        # Format response
        return Json.json(Dict(
            "data" => get(result, "data", []),
            "count" => length(get(result, "data", [])),
            "execution_time" => execution_time,
            "metadata" => Dict(
                "query" => query,
                "timestamp" => Dates.format(now(), "yyyy-mm-ddTHH:MM:SS") * "Z"
            )
        ))
        
    catch e
        @error "MindsDB query error" exception=e
        return Json.json(Dict(
            "error" => "server_error",
            "message" => "Query execution failed",
            "details" => string(e)
        )), 500
    end
end

# 404 handler
Genie.Router.route("/.*", method = [GET, POST, PUT, DELETE]) do
    cors_headers()
    Json.json(Dict(
        "error" => "not_found",
        "message" => "Endpoint not found",
        "path" => params(:REQUEST).target
    )), 404
end

# Start the server
function start_server()
    @info "Starting Nexpo Julia API on port $(CONFIG[:port])"
    
    Genie.config.run_as_server = true
    Genie.config.cors_allowed_origins = [CONFIG[:cors_origins]]
    
    up(CONFIG[:port], "0.0.0.0")
end

end # module
