package models;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * Token exchange request
 */
@ApiModel(description = "Token exchange request")
public class TokenRequest {
    
    @ApiModelProperty(value = "Grant type", example = "authorization_code", required = true)
    @JsonProperty("grant_type")
    private String grantType;
    
    @ApiModelProperty(value = "Authorization code", example = "abc123", required = false)
    @JsonProperty("code")
    private String code;
    
    @ApiModelProperty(value = "Redirect URI", example = "http://localhost:3000/callback", required = false)
    @JsonProperty("redirect_uri")
    private String redirectUri;
    
    @ApiModelProperty(value = "Refresh token", example = "refresh_token_123", required = false)
    @JsonProperty("refresh_token")
    private String refreshToken;
    
    public TokenRequest() {}
    
    // Getters and setters
    public String getGrantType() {
        return grantType;
    }
    
    public void setGrantType(String grantType) {
        this.grantType = grantType;
    }
    
    public String getCode() {
        return code;
    }
    
    public void setCode(String code) {
        this.code = code;
    }
    
    public String getRedirectUri() {
        return redirectUri;
    }
    
    public void setRedirectUri(String redirectUri) {
        this.redirectUri = redirectUri;
    }
    
    public String getRefreshToken() {
        return refreshToken;
    }
    
    public void setRefreshToken(String refreshToken) {
        this.refreshToken = refreshToken;
    }
}
