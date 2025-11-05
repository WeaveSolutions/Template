package models;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * Token exchange response
 */
@ApiModel(description = "Token exchange response")
public class TokenResponse {
    
    @ApiModelProperty(value = "Access token", example = "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9...")
    @JsonProperty("access_token")
    private String accessToken;
    
    @ApiModelProperty(value = "Token type", example = "Bearer")
    @JsonProperty("token_type")
    private String tokenType;
    
    @ApiModelProperty(value = "Expires in seconds", example = "3600")
    @JsonProperty("expires_in")
    private int expiresIn;
    
    @ApiModelProperty(value = "Refresh token", example = "refresh_token_123")
    @JsonProperty("refresh_token")
    private String refreshToken;
    
    @ApiModelProperty(value = "ID token", example = "id_token_123")
    @JsonProperty("id_token")
    private String idToken;
    
    @ApiModelProperty(value = "Token scope", example = "openid profile email")
    @JsonProperty("scope")
    private String scope;
    
    public TokenResponse() {}
    
    // Getters and setters
    public String getAccessToken() {
        return accessToken;
    }
    
    public void setAccessToken(String accessToken) {
        this.accessToken = accessToken;
    }
    
    public String getTokenType() {
        return tokenType;
    }
    
    public void setTokenType(String tokenType) {
        this.tokenType = tokenType;
    }
    
    public int getExpiresIn() {
        return expiresIn;
    }
    
    public void setExpiresIn(int expiresIn) {
        this.expiresIn = expiresIn;
    }
    
    public String getRefreshToken() {
        return refreshToken;
    }
    
    public void setRefreshToken(String refreshToken) {
        this.refreshToken = refreshToken;
    }
    
    public String getIdToken() {
        return idToken;
    }
    
    public void setIdToken(String idToken) {
        this.idToken = idToken;
    }
    
    public String getScope() {
        return scope;
    }
    
    public void setScope(String scope) {
        this.scope = scope;
    }
}
