package models;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * Linked account information
 */
@ApiModel(description = "Linked account information")
public class LinkedAccount {
    
    @ApiModelProperty(value = "Provider name", example = "google-oauth2")
    @JsonProperty("provider")
    private String provider;
    
    @ApiModelProperty(value = "Provider user ID", example = "123456789")
    @JsonProperty("userId")
    private String userId;
    
    @ApiModelProperty(value = "Linked status", example = "true")
    @JsonProperty("linked")
    private boolean linked;
    
    @ApiModelProperty(value = "Linked timestamp", example = "2024-01-15T10:30:00Z")
    @JsonProperty("linkedAt")
    private String linkedAt;
    
    public LinkedAccount() {}
    
    public LinkedAccount(String provider, String userId, boolean linked, String linkedAt) {
        this.provider = provider;
        this.userId = userId;
        this.linked = linked;
        this.linkedAt = linkedAt;
    }
    
    // Getters and setters
    public String getProvider() {
        return provider;
    }
    
    public void setProvider(String provider) {
        this.provider = provider;
    }
    
    public String getUserId() {
        return userId;
    }
    
    public void setUserId(String userId) {
        this.userId = userId;
    }
    
    public boolean isLinked() {
        return linked;
    }
    
    public void setLinked(boolean linked) {
        this.linked = linked;
    }
    
    public String getLinkedAt() {
        return linkedAt;
    }
    
    public void setLinkedAt(String linkedAt) {
        this.linkedAt = linkedAt;
    }
}
