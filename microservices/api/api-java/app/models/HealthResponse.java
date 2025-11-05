package models;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * Health check response
 */
@ApiModel(description = "Health check response")
public class HealthResponse {
    
    @ApiModelProperty(value = "Service status", example = "healthy")
    @JsonProperty("status")
    private String status;
    
    @ApiModelProperty(value = "Service name", example = "nexpo-api-java")
    @JsonProperty("service")
    private String service;
    
    @ApiModelProperty(value = "Service version", example = "1.0.0")
    @JsonProperty("version")
    private String version;
    
    @ApiModelProperty(value = "Current timestamp", example = "2024-01-15T10:30:00Z")
    @JsonProperty("timestamp")
    private String timestamp;
    
    public HealthResponse() {}
    
    public HealthResponse(String status, String service, String version, String timestamp) {
        this.status = status;
        this.service = service;
        this.version = version;
        this.timestamp = timestamp;
    }
    
    // Getters and setters
    public String getStatus() {
        return status;
    }
    
    public void setStatus(String status) {
        this.status = status;
    }
    
    public String getService() {
        return service;
    }
    
    public void setService(String service) {
        this.service = service;
    }
    
    public String getVersion() {
        return version;
    }
    
    public void setVersion(String version) {
        this.version = version;
    }
    
    public String getTimestamp() {
        return timestamp;
    }
    
    public void setTimestamp(String timestamp) {
        this.timestamp = timestamp;
    }
}
