using System.IdentityModel.Tokens.Jwt;
using System.Security.Claims;
using Microsoft.IdentityModel.Tokens;
using System.Text;
using NexpoApi.Models;

namespace NexpoApi.Services;

/// <summary>
/// JWT token verification service
/// </summary>
public class JwtService
{
    private readonly Settings _settings;
    private readonly HttpClient _httpClient;
    private readonly ILogger<JwtService> _logger;

    public JwtService(Settings settings, HttpClient httpClient, ILogger<JwtService> logger)
    {
        _settings = settings;
        _httpClient = httpClient;
        _logger = logger;
    }

    /// <summary>
    /// Verify JWT token from Auth0
    /// </summary>
    public async Task<ClaimsPrincipal?> VerifyTokenAsync(string token)
    {
        try
        {
            var handler = new JwtSecurityTokenHandler();
            
            // Read token without validation first to get the kid (key id)
            var jsonToken = handler.ReadJwtToken(token);
            
            // Get JWKS from Auth0
            var jwks = await GetJwksAsync();
            var key = FindKeyInJwks(jwks, jsonToken.Header.Kid);
            
            if (key == null)
            {
                _logger.LogWarning("No matching key found in JWKS for kid: {Kid}", jsonToken.Header.Kid);
                return null;
            }

            var validationParameters = new TokenValidationParameters
            {
                ValidateIssuer = true,
                ValidIssuer = $"https://{_settings.Auth0Domain}/",
                ValidateAudience = true,
                ValidAudience = _settings.Auth0Audience,
                ValidateLifetime = true,
                ValidateIssuerSigningKey = true,
                IssuerSigningKey = key,
                ClockSkew = TimeSpan.FromMinutes(5)
            };

            var principal = handler.ValidateToken(token, validationParameters, out _);
            return principal;
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Token validation failed");
            return null;
        }
    }

    /// <summary>
    /// Get JWKS from Auth0
    /// </summary>
    private async Task<string> GetJwksAsync()
    {
        var jwksUri = $"https://{_settings.Auth0Domain}/.well-known/jwks.json";
        var response = await _httpClient.GetStringAsync(jwksUri);
        return response;
    }

    /// <summary>
    /// Find the appropriate key in JWKS
    /// </summary>
    private SecurityKey? FindKeyInJwks(string jwks, string? kid)
    {
        // This is a simplified implementation
        // In production, you'd want to use a proper JWKS client library
        try
        {
            var jwksData = System.Text.Json.JsonSerializer.Deserialize<Dictionary<string, object>>(jwks);
            if (jwksData?.ContainsKey("keys") == true)
            {
                var keys = System.Text.Json.JsonSerializer.Deserialize<List<Dictionary<string, object>>>(jwksData["keys"].ToString()!);
                var keyData = keys?.FirstOrDefault(k => k.ContainsKey("kid") && k["kid"].ToString() == kid);
                
                if (keyData?.ContainsKey("n") == true && keyData.ContainsKey("e") == true)
                {
                    var n = keyData["n"].ToString();
                    var e = keyData["e"].ToString();
                    
                    var rsa = new System.Security.Cryptography.RSACryptoServiceProvider();
                    rsa.ImportParameters(new System.Security.Cryptography.RSAParameters
                    {
                        Modulus = Convert.FromBase64String(AddPadding(n!)),
                        Exponent = Convert.FromBase64String(AddPadding(e!))
                    });
                    
                    return new RsaSecurityKey(rsa);
                }
            }
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Failed to parse JWKS");
        }
        
        return null;
    }

    /// <summary>
    /// Add padding to base64 string if needed
    /// </summary>
    private static string AddPadding(string base64)
    {
        var padding = 4 - (base64.Length % 4);
        if (padding != 4)
        {
            base64 += new string('=', padding);
        }
        return base64;
    }
}
