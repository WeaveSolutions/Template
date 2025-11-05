namespace NexpoApi.Models;

/// <summary>
/// Application settings configuration
/// </summary>
public class Settings
{
    // Server Configuration
    public int Port { get; set; } = 7020;
    public string Host { get; set; } = "0.0.0.0";
    public string Environment { get; set; } = "development";

    // Auth0 Configuration
    public string Auth0Domain { get; set; } = string.Empty;
    public string Auth0Audience { get; set; } = "https://api.nexpo.dev";
    public string Auth0ClientId { get; set; } = string.Empty;
    public string Auth0ClientSecret { get; set; } = string.Empty;

    // MindsDB Configuration
    public string MindsDbUrl { get; set; } = "http://localhost:4040";

    // CORS Configuration
    public string[] CorsOrigins { get; set; } = { "http://localhost:3000" };

    /// <summary>
    /// Load settings from environment variables with fallback to defaults
    /// </summary>
    public static Settings LoadFromEnvironment()
    {
        var settings = new Settings();
        
        if (int.TryParse(System.Environment.GetEnvironmentVariable("DOTNET_API_PORT"), out int port))
            settings.Port = port;
            
        settings.Host = System.Environment.GetEnvironmentVariable("HOST") ?? settings.Host;
        settings.Environment = System.Environment.GetEnvironmentVariable("NODE_ENV") ?? settings.Environment;
        
        settings.Auth0Domain = System.Environment.GetEnvironmentVariable("AUTH0_DOMAIN") ?? settings.Auth0Domain;
        settings.Auth0Audience = System.Environment.GetEnvironmentVariable("AUTH0_AUDIENCE") ?? settings.Auth0Audience;
        settings.Auth0ClientId = System.Environment.GetEnvironmentVariable("AUTH0_CLIENT_ID") ?? settings.Auth0ClientId;
        settings.Auth0ClientSecret = System.Environment.GetEnvironmentVariable("AUTH0_CLIENT_SECRET") ?? settings.Auth0ClientSecret;
        
        settings.MindsDbUrl = System.Environment.GetEnvironmentVariable("MINDSDB_URL") ?? settings.MindsDbUrl;
        
        var corsOrigins = System.Environment.GetEnvironmentVariable("CORS_ORIGINS");
        if (!string.IsNullOrEmpty(corsOrigins))
            settings.CorsOrigins = corsOrigins.Split(',');
            
        return settings;
    }
}
