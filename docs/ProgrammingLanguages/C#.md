C# [https://docs.microsoft.com/en-us/dotnet/csharp/](Programming Language)

ASP.NET Core [https://docs.microsoft.com/en-us/aspnet/core/](Web Framework)
.NET SDK [https://dotnet.microsoft.com/download](.NET SDK)

# C# Installation Guide (Latest Version)

## Overview

- Official C# docs: [https://docs.microsoft.com/en-us/dotnet/csharp/]
- C# is a modern, object-oriented programming language developed by Microsoft.
- Current version: **.NET 8** (LTS), **.NET 9** (latest)
- Framework: **ASP.NET Core** for web APIs and applications
- Package manager: **NuGet**
- Excellent for enterprise applications, web APIs, and cross-platform development

---

## Windows Installation

### Step 1: Download .NET SDK

- Visit: [https://dotnet.microsoft.com/download]
- Download the latest .NET SDK (includes runtime)
- Run the installer and follow the setup wizard
- **Recommended**: Install both .NET 8 (LTS) and .NET 9 (latest)

### Step 2: Install Visual Studio (Recommended)

- Visit: [https://visualstudio.microsoft.com/vs/community/]
- Download Visual Studio Community (free)
- During installation, select:
  - "ASP.NET and web development" workload
  - ".NET desktop development" workload
  - "Azure development" workload (optional)

### Alternative: Install Visual Studio Code

```powershell
# Using Chocolatey
choco install vscode

# Install C# extension
code --install-extension ms-dotnettools.csharp
```

### Step 3: Verify Installation

```powershell
dotnet --version
dotnet --list-sdks
dotnet --list-runtimes
```

---

## macOS Installation

### Step 1: Install .NET SDK

#### Using Homebrew (Recommended):
```bash
# Install Homebrew if not already installed
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install .NET SDK
brew install --cask dotnet-sdk
```

#### Manual Installation:
- Visit: [https://dotnet.microsoft.com/download]
- Download .NET SDK for macOS
- Run the installer package

### Step 2: Install Visual Studio Code

```bash
brew install --cask visual-studio-code

# Install C# extension
code --install-extension ms-dotnettools.csharp
```

### Step 3: Verify Installation

```bash
dotnet --version
dotnet --list-sdks
which dotnet
```

---

## Ubuntu / Debian Linux Installation

### Step 1: Add Microsoft Package Repository

#### For Ubuntu 22.04:
```bash
wget https://packages.microsoft.com/config/ubuntu/22.04/packages-microsoft-prod.deb -O packages-microsoft-prod.deb
sudo dpkg -i packages-microsoft-prod.deb
rm packages-microsoft-prod.deb
```

#### For Ubuntu 20.04:
```bash
wget https://packages.microsoft.com/config/ubuntu/20.04/packages-microsoft-prod.deb -O packages-microsoft-prod.deb
sudo dpkg -i packages-microsoft-prod.deb
rm packages-microsoft-prod.deb
```

### Step 2: Install .NET SDK

```bash
sudo apt update
sudo apt install dotnet-sdk-8.0

# Optional: Install .NET 9
sudo apt install dotnet-sdk-9.0
```

### Step 3: Install Visual Studio Code

```bash
# Add Microsoft GPG key and repository
wget -qO- https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > packages.microsoft.gpg
sudo install -o root -g root -m 644 packages.microsoft.gpg /etc/apt/trusted.gpg.d/
echo "deb [arch=amd64,arm64,armhf signed-by=/etc/apt/trusted.gpg.d/packages.microsoft.gpg] https://packages.microsoft.com/repos/code stable main" | sudo tee /etc/apt/sources.list.d/vscode.list

sudo apt update
sudo apt install code

# Install C# extension
code --install-extension ms-dotnettools.csharp
```

### Step 4: Verify Installation

```bash
dotnet --version
dotnet --list-sdks
```

---

## Quick Start: ASP.NET Core Web API

### Step 1: Create New Web API Project

```bash
dotnet new webapi -n MyCSharpApi
cd MyCSharpApi
```

### Step 2: Explore Project Structure

The generated project includes:
- `Program.cs` - Entry point and configuration
- `Controllers/WeatherForecastController.cs` - Sample controller
- `MyCSharpApi.csproj` - Project file
- `appsettings.json` - Configuration

### Step 3: Add Custom Controller

Create `Controllers/ApiController.cs`:
```csharp
using Microsoft.AspNetCore.Mvc;

namespace MyCSharpApi.Controllers;

[ApiController]
[Route("[controller]")]
public class ApiController : ControllerBase
{
    [HttpGet("health")]
    public IActionResult Health()
    {
        var response = new
        {
            message = "Hello from C# ASP.NET Core API!",
            status = "success",
            port = 8130,
            timestamp = DateTime.UtcNow
        };
        
        return Ok(response);
    }
    
    [HttpGet("users/{id}")]
    public IActionResult GetUser(string id)
    {
        var user = new
        {
            user_id = id,
            name = $"User {id}",
            platform = "C#",
            framework = ".NET Core"
        };
        
        return Ok(user);
    }
    
    [HttpPost("users")]
    public IActionResult CreateUser([FromBody] CreateUserRequest request)
    {
        var user = new
        {
            user_id = Guid.NewGuid().ToString(),
            name = request.Name,
            email = request.Email,
            platform = "C#",
            created_at = DateTime.UtcNow
        };
        
        return CreatedAtAction(nameof(GetUser), new { id = user.user_id }, user);
    }
}

public record CreateUserRequest(string Name, string Email);
```

### Step 4: Configure Program.cs

Update `Program.cs`:
```csharp
var builder = WebApplication.CreateBuilder(args);

// Add services to the container
builder.Services.AddControllers();
builder.Services.AddEndpointsApiExplorer();
builder.Services.AddSwaggerGen();

// Configure CORS
builder.Services.AddCors(options =>
{
    options.AddDefaultPolicy(policy =>
    {
        policy.AllowAnyOrigin()
              .AllowAnyMethod()
              .AllowAnyHeader();
    });
});

var app = builder.Build();

// Configure the HTTP request pipeline
if (app.Environment.IsDevelopment())
{
    app.UseSwagger();
    app.UseSwaggerUI();
}

app.UseCors();
app.UseAuthorization();
app.MapControllers();

// Custom port configuration
app.Urls.Add("http://localhost:8130");

Console.WriteLine("🚀 C# ASP.NET Core API starting on http://localhost:8130");
Console.WriteLine("Swagger UI available at: http://localhost:8130/swagger");

app.Run();
```

### Step 5: Run the Application

```bash
dotnet run
```

Visit:
- API: `http://localhost:8130/api/health`
- Swagger UI: `http://localhost:8130/swagger`

---

## Advanced Features

### Dependency Injection Example

Create `Services/IUserService.cs`:
```csharp
namespace MyCSharpApi.Services;

public interface IUserService
{
    Task<User> GetUserAsync(string id);
    Task<User> CreateUserAsync(CreateUserRequest request);
}

public class UserService : IUserService
{
    private readonly ILogger<UserService> _logger;
    
    public UserService(ILogger<UserService> logger)
    {
        _logger = logger;
    }
    
    public async Task<User> GetUserAsync(string id)
    {
        _logger.LogInformation("Getting user with ID: {UserId}", id);
        
        // Simulate async operation
        await Task.Delay(100);
        
        return new User
        {
            Id = id,
            Name = $"User {id}",
            Email = $"user{id}@example.com",
            CreatedAt = DateTime.UtcNow
        };
    }
    
    public async Task<User> CreateUserAsync(CreateUserRequest request)
    {
        _logger.LogInformation("Creating user: {UserName}", request.Name);
        
        await Task.Delay(200); // Simulate async operation
        
        return new User
        {
            Id = Guid.NewGuid().ToString(),
            Name = request.Name,
            Email = request.Email,
            CreatedAt = DateTime.UtcNow
        };
    }
}

public class User
{
    public string Id { get; set; } = string.Empty;
    public string Name { get; set; } = string.Empty;
    public string Email { get; set; } = string.Empty;
    public DateTime CreatedAt { get; set; }
}
```

### Entity Framework Core Integration

Add EF Core package:
```bash
dotnet add package Microsoft.EntityFrameworkCore.InMemory
```

Create `Data/AppDbContext.cs`:
```csharp
using Microsoft.EntityFrameworkCore;
using MyCSharpApi.Services;

namespace MyCSharpApi.Data;

public class AppDbContext : DbContext
{
    public AppDbContext(DbContextOptions<AppDbContext> options) : base(options)
    {
    }
    
    public DbSet<User> Users { get; set; }
}
```

Register in `Program.cs`:
```csharp
// Add to builder.Services
builder.Services.AddDbContext<AppDbContext>(options =>
    options.UseInMemoryDatabase("InMemoryDb"));
builder.Services.AddScoped<IUserService, UserService>();
```

### Minimal APIs (Alternative Approach)

Create `MinimalApiProgram.cs`:
```csharp
var builder = WebApplication.CreateBuilder(args);

builder.Services.AddEndpointsApiExplorer();
builder.Services.AddSwaggerGen();

var app = builder.Build();

if (app.Environment.IsDevelopment())
{
    app.UseSwagger();
    app.UseSwaggerUI();
}

// Minimal API endpoints
app.MapGet("/health", () => new
{
    message = "Hello from C# Minimal API!",
    status = "success",
    port = 8130
});

app.MapGet("/api/users/{id}", (string id) => new
{
    user_id = id,
    name = $"User {id}",
    platform = "C# Minimal API"
});

app.MapPost("/api/users", (CreateUserRequest request) => 
{
    var user = new
    {
        user_id = Guid.NewGuid().ToString(),
        name = request.Name,
        email = request.Email,
        created_at = DateTime.UtcNow
    };
    
    return Results.Created($"/api/users/{user.user_id}", user);
});

app.Run();

record CreateUserRequest(string Name, string Email);
```

---

## Package Management with NuGet

### Essential Packages

```bash
# Web API packages
dotnet add package Swashbuckle.AspNetCore  # Swagger/OpenAPI
dotnet add package Microsoft.AspNetCore.Cors

# Database packages
dotnet add package Microsoft.EntityFrameworkCore.SqlServer
dotnet add package Microsoft.EntityFrameworkCore.Tools

# Authentication
dotnet add package Microsoft.AspNetCore.Authentication.JwtBearer

# Logging
dotnet add package Serilog.AspNetCore

# Testing
dotnet add package Microsoft.AspNetCore.Mvc.Testing
dotnet add package xunit
dotnet add package FluentAssertions
```

### Global Tools

```bash
# Install useful global tools
dotnet tool install -g dotnet-ef  # Entity Framework CLI
dotnet tool install -g dotnet-outdated  # Check outdated packages
dotnet tool install -g dotnet-format  # Code formatter
```

---

## Summary Table

| Platform      | Installation    | IDE              | Package Manager | Web Framework   |
|---------------|----------------|------------------|-----------------|----------------|
| Windows       | .NET SDK       | Visual Studio    | NuGet           | ASP.NET Core   |
| macOS         | Homebrew/.NET  | VS Code/Rider    | NuGet           | ASP.NET Core   |
| Ubuntu/Debian | APT/.NET       | VS Code/Rider    | NuGet           | ASP.NET Core   |

---

## Why C# for APIs?

- **Performance** - Compiled language with excellent runtime performance
- **Type safety** - Strong typing prevents many runtime errors
- **Rich ecosystem** - Extensive NuGet package ecosystem
- **Cross-platform** - Runs on Windows, macOS, and Linux
- **Enterprise features** - Built-in dependency injection, configuration, logging
- **Modern language** - Regular updates with new features (records, pattern matching, etc.)
- **Tooling** - Excellent IDE support and debugging capabilities
- **Cloud-native** - First-class Azure support and containerization

---

## Development Best Practices

### Configuration Management

```csharp
// appsettings.json
{
  "ConnectionStrings": {
    "DefaultConnection": "Server=localhost;Database=MyApp;Trusted_Connection=true;"
  },
  "JwtSettings": {
    "SecretKey": "your-secret-key",
    "Issuer": "your-issuer",
    "Audience": "your-audience"
  },
  "Logging": {
    "LogLevel": {
      "Default": "Information",
      "Microsoft": "Warning",
      "Microsoft.Hosting.Lifetime": "Information"
    }
  }
}

// Access in controllers
public class ApiController : ControllerBase
{
    private readonly IConfiguration _configuration;
    
    public ApiController(IConfiguration configuration)
    {
        _configuration = configuration;
    }
    
    [HttpGet("config")]
    public IActionResult GetConfig()
    {
        var connectionString = _configuration.GetConnectionString("DefaultConnection");
        var jwtIssuer = _configuration["JwtSettings:Issuer"];
        
        return Ok(new { connectionString, jwtIssuer });
    }
}
```

### Error Handling Middleware

```csharp
public class ErrorHandlingMiddleware
{
    private readonly RequestDelegate _next;
    private readonly ILogger<ErrorHandlingMiddleware> _logger;
    
    public ErrorHandlingMiddleware(RequestDelegate next, ILogger<ErrorHandlingMiddleware> logger)
    {
        _next = next;
        _logger = logger;
    }
    
    public async Task InvokeAsync(HttpContext context)
    {
        try
        {
            await _next(context);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "An unhandled exception occurred");
            await HandleExceptionAsync(context, ex);
        }
    }
    
    private static async Task HandleExceptionAsync(HttpContext context, Exception exception)
    {
        context.Response.ContentType = "application/json";
        context.Response.StatusCode = 500;
        
        var response = new
        {
            error = "An error occurred while processing your request",
            message = exception.Message
        };
        
        await context.Response.WriteAsync(JsonSerializer.Serialize(response));
    }
}

// Register in Program.cs
app.UseMiddleware<ErrorHandlingMiddleware>();
```