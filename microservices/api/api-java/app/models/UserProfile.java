package models;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import java.util.List;
import java.util.ArrayList;

/**
 * User profile information
 */
@ApiModel(description = "User profile information")
public class UserProfile {
    
    @ApiModelProperty(value = "User ID", example = "auth0|123456")
    @JsonProperty("id")
    private String id;
    
    @ApiModelProperty(value = "Email address", example = "user@example.com")
    @JsonProperty("email")
    private String email;
    
    @ApiModelProperty(value = "Full name", example = "John Doe")
    @JsonProperty("name")
    private String name;
    
    @ApiModelProperty(value = "Profile picture URL", example = "https://example.com/avatar.jpg")
    @JsonProperty("picture")
    private String picture;
    
    @ApiModelProperty(value = "Email verified status", example = "true")
    @JsonProperty("email_verified")
    private boolean emailVerified;
    
    @ApiModelProperty(value = "User roles", example = "[\"user\", \"admin\"]")
    @JsonProperty("roles")
    private List<String> roles = new ArrayList<>();
    
    @ApiModelProperty(value = "User permissions", example = "[\"read:profile\", \"write:profile\"]")
    @JsonProperty("permissions")
    private List<String> permissions = new ArrayList<>();
    
    @ApiModelProperty(value = "User metadata")
    @JsonProperty("metadata")
    private JsonNode metadata;
    
    public UserProfile() {}
    
    // Getters and setters
    public String getId() {
        return id;
    }
    
    public void setId(String id) {
        this.id = id;
    }
    
    public String getEmail() {
        return email;
    }
    
    public void setEmail(String email) {
        this.email = email;
    }
    
    public String getName() {
        return name;
    }
    
    public void setName(String name) {
        this.name = name;
    }
    
    public String getPicture() {
        return picture;
    }
    
    public void setPicture(String picture) {
        this.picture = picture;
    }
    
    public boolean isEmailVerified() {
        return emailVerified;
    }
    
    public void setEmailVerified(boolean emailVerified) {
        this.emailVerified = emailVerified;
    }
    
    public List<String> getRoles() {
        return roles;
    }
    
    public void setRoles(List<String> roles) {
        this.roles = roles;
    }
    
    public List<String> getPermissions() {
        return permissions;
    }
    
    public void setPermissions(List<String> permissions) {
        this.permissions = permissions;
    }
    
    public JsonNode getMetadata() {
        return metadata;
    }
    
    public void setMetadata(JsonNode metadata) {
        this.metadata = metadata;
    }
}
