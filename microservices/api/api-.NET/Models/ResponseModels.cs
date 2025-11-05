namespace NexpoApi.Models;

/// <summary>
/// Health check response model
/// </summary>
public class HealthResponse
{
    public string Status { get; set; } = "ok";
    public string Service { get; set; } = "nexpo-dotnet-api";
    public string Timestamp { get; set; } = DateTime.UtcNow.ToString("O");
    public string Version { get; set; } = "1.0.0";
    public string Environment { get; set; } = "development";
}

/// <summary>
/// User profile model
/// </summary>
public class UserProfile
{
    public string Sub { get; set; } = string.Empty;
    public string? Email { get; set; }
    public string? Name { get; set; }
    public string? Picture { get; set; }
    public bool EmailVerified { get; set; } = false;
    public Dictionary<string, object> Metadata { get; set; } = new();
}

/// <summary>
/// Error response model
/// </summary>
public class ErrorResponse
{
    public string Error { get; set; } = string.Empty;
    public string Message { get; set; } = string.Empty;
    public Dictionary<string, object>? Details { get; set; }
}

/// <summary>
/// Token exchange response
/// </summary>
public class TokenResponse
{
    public string AccessToken { get; set; } = string.Empty;
    public string TokenType { get; set; } = "Bearer";
    public int ExpiresIn { get; set; }
    public string? Scope { get; set; }
}

/// <summary>
/// Linked social account
/// </summary>
public class LinkedAccount
{
    public string Provider { get; set; } = string.Empty;
    public string UserId { get; set; } = string.Empty;
    public string Connection { get; set; } = string.Empty;
    public bool IsSocial { get; set; } = true;
    public string LinkedAt { get; set; } = DateTime.UtcNow.ToString("O");
}

/// <summary>
/// MindsDB query request
/// </summary>
public class QueryRequest
{
    public string Query { get; set; } = string.Empty;
}

/// <summary>
/// MindsDB query response
/// </summary>
public class QueryResponse
{
    public string Query { get; set; } = string.Empty;
    public object[]? Results { get; set; }
    public int ExecutionTimeMs { get; set; }
    public string Status { get; set; } = "success";
}
