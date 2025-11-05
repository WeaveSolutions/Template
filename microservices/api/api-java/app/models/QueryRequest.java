package models;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;

/**
 * MindsDB query request
 */
@ApiModel(description = "MindsDB query request")
public class QueryRequest {
    
    @ApiModelProperty(value = "SQL query", example = "SELECT * FROM users LIMIT 10", required = true)
    @JsonProperty("query")
    private String query;
    
    @ApiModelProperty(value = "Query parameters", required = false)
    @JsonProperty("parameters")
    private JsonNode parameters;
    
    @ApiModelProperty(value = "Query timeout in seconds", example = "30", required = false)
    @JsonProperty("timeout")
    private Integer timeout;
    
    public QueryRequest() {}
    
    // Getters and setters
    public String getQuery() {
        return query;
    }
    
    public void setQuery(String query) {
        this.query = query;
    }
    
    public JsonNode getParameters() {
        return parameters;
    }
    
    public void setParameters(JsonNode parameters) {
        this.parameters = parameters;
    }
    
    public Integer getTimeout() {
        return timeout;
    }
    
    public void setTimeout(Integer timeout) {
        this.timeout = timeout;
    }
}
