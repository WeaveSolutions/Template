package models;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;

/**
 * Query execution response
 */
@ApiModel(description = "Query execution response")
public class QueryResponse {
    
    @ApiModelProperty(value = "Query results")
    @JsonProperty("data")
    private ArrayNode data;
    
    @ApiModelProperty(value = "Row count", example = "10")
    @JsonProperty("count")
    private int count;
    
    @ApiModelProperty(value = "Execution time in ms", example = "125")
    @JsonProperty("executionTime")
    private long executionTime;
    
    @ApiModelProperty(value = "Query metadata")
    @JsonProperty("metadata")
    private JsonNode metadata;
    
    public QueryResponse() {}
    
    public QueryResponse(ArrayNode data, int count, long executionTime, JsonNode metadata) {
        this.data = data;
        this.count = count;
        this.executionTime = executionTime;
        this.metadata = metadata;
    }
    
    // Getters and setters
    public ArrayNode getData() {
        return data;
    }
    
    public void setData(ArrayNode data) {
        this.data = data;
    }
    
    public int getCount() {
        return count;
    }
    
    public void setCount(int count) {
        this.count = count;
    }
    
    public long getExecutionTime() {
        return executionTime;
    }
    
    public void setExecutionTime(long executionTime) {
        this.executionTime = executionTime;
    }
    
    public JsonNode getMetadata() {
        return metadata;
    }
    
    public void setMetadata(JsonNode metadata) {
        this.metadata = metadata;
    }
}
