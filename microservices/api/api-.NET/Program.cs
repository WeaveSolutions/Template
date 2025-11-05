using Microsoft.AspNetCore.Authentication.JwtBearer;
using Microsoft.OpenApi.Models;
using NexpoApi.Models;
using NexpoApi.Services;
using System.Security.Claims;
using System.Diagnostics;

var builder = WebApplication.CreateBuilder(args);

// Load settings from environment
var settings = Settings.LoadFromEnvironment();

// Add services to the container
builder.Services.AddSingleton(settings);
builder.Services.AddHttpClient();
builder.Services.AddScoped<JwtService>();

// Add CORS
builder.Services.AddCors(options =>
{
    options.AddPolicy("AllowedOrigins", policy =>
    {
        policy.WithOrigins(settings.CorsOrigins)
              .AllowAnyHeader()
              .AllowAnyMethod()
              .AllowCredentials();
    });
});

// Add JWT Authentication
builder.Services.AddAuthentication(JwtBearerDefaults.AuthenticationScheme)
    .AddJwtBearer(options =>
    {
        options.Authority = $"https://{settings.Auth0Domain}/";
        options.Audience = settings.Auth0Audience;
        options.RequireHttpsMetadata = settings.Environment == "production";
    });

builder.Services.AddAuthorization();

// Add API exploration
builder.Services.AddEndpointsApiExplorer();
builder.Services.AddSwaggerGen(c =>
{
    c.SwaggerDoc("v1", new OpenApiInfo 
    { 
        Title = "Nexpo .NET API", 
        Version = "v1.0",
        Description = "Basic configurable backend API for Nexpo - .NET Implementation"
    });
    
    c.AddSecurityDefinition("Bearer", new OpenApiSecurityScheme
    {
        Description = "JWT Authorization header using the Bearer scheme",
        Name = "Authorization",
        In = ParameterLocation.Header,
        Type = SecuritySchemeType.ApiKey,
        Scheme = "Bearer"
    });
    
    c.AddSecurityRequirement(new OpenApiSecurityRequirement
    {
        {
            new OpenApiSecurityScheme
            {
                Reference = new OpenApiReference
                {
                    Type = ReferenceType.SecurityScheme,
                    Id = "Bearer"
                }
            },
            Array.Empty<string>()
        }
    });
});

var app = builder.Build();

// Configure the HTTP request pipeline
if (app.Environment.IsDevelopment())
{
    app.UseSwagger();
    app.UseSwaggerUI(c =>
    {
        c.SwaggerEndpoint("/swagger/v1/swagger.json", "Nexpo .NET API v1");
        c.RoutePrefix = "docs";
    });
}

app.UseCors("AllowedOrigins");
app.UseAuthentication();
app.UseAuthorization();

// Root endpoint - health check
app.MapGet("/", () =>
{
    return Results.Ok(new HealthResponse
    {
        Status = "ok",
        Service = "nexpo-dotnet-api",
        Timestamp = DateTime.UtcNow.ToString("O"),
        Version = "1.0.0",
        Environment = settings.Environment
    });
}).WithTags("Health");

// Detailed health check endpoint
app.MapGet("/health", () =>
{
    return Results.Ok(new HealthResponse
    {
        Status = "healthy",
        Service = "nexpo-dotnet-api",
        Timestamp = DateTime.UtcNow.ToString("O"),
        Version = "1.0.0",
        Environment = settings.Environment
    });
}).WithTags("Health");

// Get current user profile
app.MapGet("/api/user/profile", (ClaimsPrincipal user, JwtService jwtService) =>
{
    if (!user.Identity?.IsAuthenticated == true)
    {
        return Results.Unauthorized();
    }

    var profile = new UserProfile
    {
        Sub = user.FindFirst(ClaimTypes.NameIdentifier)?.Value ?? "",
        Email = user.FindFirst(ClaimTypes.Email)?.Value,
        Name = user.FindFirst(ClaimTypes.Name)?.Value,
        Picture = user.FindFirst("picture")?.Value,
        EmailVerified = bool.Parse(user.FindFirst("email_verified")?.Value ?? "false")
    };

    return Results.Ok(profile);
})
.RequireAuthorization()
.WithTags("User")
.WithOpenApi();

// Get user's linked social accounts
app.MapGet("/api/user/accounts", (ClaimsPrincipal user) =>
{
    if (!user.Identity?.IsAuthenticated == true)
    {
        return Results.Unauthorized();
    }

    // Mock linked accounts - in real implementation, fetch from Auth0 Management API
    var accounts = new[]
    {
        new LinkedAccount
        {
            Provider = "google-oauth2",
            UserId = "google-oauth2|123456789",
            Connection = "google-oauth2",
            IsSocial = true,
            LinkedAt = DateTime.UtcNow.AddDays(-30).ToString("O")
        }
    };

    return Results.Ok(accounts);
})
.RequireAuthorization()
.WithTags("User")
.WithOpenApi();

// Exchange credentials for access token
app.MapPost("/api/token", (HttpContext context) =>
{
    var grantType = context.Request.Form["grant_type"].FirstOrDefault();
    var scope = context.Request.Form["scope"].FirstOrDefault();

    if (grantType != "client_credentials")
    {
        return Results.BadRequest(new ErrorResponse
        {
            Error = "unsupported_grant_type",
            Message = "Only client_credentials grant type is supported"
        });
    }

    // Mock token response - in real implementation, validate client credentials
    var tokenResponse = new TokenResponse
    {
        AccessToken = "mock_access_token_" + Guid.NewGuid().ToString("N")[..16],
        TokenType = "Bearer",
        ExpiresIn = 3600,
        Scope = scope ?? "read:basic"
    };

    return Results.Ok(tokenResponse);
})
.WithTags("Auth")
.WithOpenApi();

// Execute query through MindsDB gateway
app.MapPost("/api/query", (QueryRequest request, ClaimsPrincipal user, HttpClient httpClient) =>
{
    if (!user.Identity?.IsAuthenticated == true)
    {
        return Results.Unauthorized();
    }

    var stopwatch = Stopwatch.StartNew();
    
    try
    {
        // Mock MindsDB query - in real implementation, forward to actual MindsDB instance
        var response = new QueryResponse
        {
            Query = request.Query,
            Results = new object[] 
            { 
                new { message = "Mock response", query = request.Query, timestamp = DateTime.UtcNow }
            },
            ExecutionTimeMs = (int)stopwatch.ElapsedMilliseconds,
            Status = "success"
        };

        return Results.Ok(response);
    }
    catch (Exception ex)
    {
        return Results.Problem(
            detail: ex.Message,
            statusCode: 500,
            title: "Query Failed");
    }
})
.RequireAuthorization()
.WithTags("Query")
.WithOpenApi();

// Global exception handler
app.UseExceptionHandler(errorApp =>
{
    errorApp.Run(async context =>
    {
        context.Response.StatusCode = 500;
        context.Response.ContentType = "application/json";
        
        var error = new ErrorResponse
        {
            Error = "internal_server_error",
            Message = "An unexpected error occurred"
        };
        
        await context.Response.WriteAsJsonAsync(error);
    });
});

// Start the server
app.Run($"http://{settings.Host}:{settings.Port}");
