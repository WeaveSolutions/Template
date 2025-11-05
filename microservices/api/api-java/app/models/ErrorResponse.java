package models;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;

/**
 * Error response
 */
@ApiModel(description = "Error response")
public class ErrorResponse {
    
    @ApiModelProperty(value = "Error message", example = "Invalid request")
    @JsonProperty("error")
    private String error;
    
    @ApiModelProperty(value = "Error code", example = "INVALID_REQUEST")
    @JsonProperty("code")
    private String code;
    
    @ApiModelProperty(value = "HTTP status code", example = "400")
    @JsonProperty("status")
    private int status = 400;
    
    @ApiModelProperty(value = "Additional details")
    @JsonProperty("details")
    private JsonNode details;
    
    public ErrorResponse() {}
    
    public ErrorResponse(String error, String code, int status) {
        this.error = error;
        this.code = code;
        this.status = status;
    }
    
    public ErrorResponse(String error, String code, int status, JsonNode details) {
        this.error = error;
        this.code = code;
        this.status = status;
        this.details = details;
    }
    
    // Getters and setters
    public String getError() {
        return error;
    }
    
    public void setError(String error) {
        this.error = error;
    }
    
    public String getCode() {
        return code;
    }
    
    public void setCode(String code) {
        this.code = code;
    }
    
    public int getStatus() {
        return status;
    }
    
    public void setStatus(int status) {
        this.status = status;
    }
    
    public JsonNode getDetails() {
        return details;
    }
    
    public void setDetails(JsonNode details) {
        this.details = details;
    }
}
