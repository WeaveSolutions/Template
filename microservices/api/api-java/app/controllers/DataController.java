package controllers;

import actions.AuthRequest;
import com.auth0.jwt.interfaces.DecodedJWT;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.typesafe.config.Config;
import io.swagger.annotations.*;
import models.ErrorResponse;
import models.QueryRequest;
import models.QueryResponse;
import play.Logger;
import play.libs.Json;
import play.libs.ws.WSClient;
import play.mvc.Controller;
import play.mvc.Result;
import play.mvc.Http;
import javax.inject.Inject;
import javax.inject.Singleton;
import java.time.Duration;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;

/**
 * Data controller for MindsDB integration
 */
@Api(value = "Data", description = "Data query operations")
@Singleton
public class DataController extends Controller {
    
    private static final Logger.ALogger logger = Logger.of(DataController.class);
    
    private final WSClient wsClient;
    private final String mindsdbUrl;
    private final Duration mindsdbTimeout;
    
    @Inject
    public DataController(WSClient wsClient, Config config) {
        this.wsClient = wsClient;
        this.mindsdbUrl = config.getString("mindsdb.url");
        this.mindsdbTimeout = config.getDuration("mindsdb.timeout");
    }
    
    /**
     * Query MindsDB
     */
    @ApiOperation(
        value = "Query MindsDB",
        notes = "Execute SQL queries against MindsDB",
        httpMethod = "POST",
        response = QueryResponse.class,
        authorizations = {@Authorization("Bearer")}
    )
    @ApiImplicitParams({
        @ApiImplicitParam(
            name = "body",
            value = "Query request",
            required = true,
            dataType = "models.QueryRequest",
            paramType = "body"
        )
    })
    @ApiResponses(value = {
        @ApiResponse(code = 200, message = "Query results", response = QueryResponse.class),
        @ApiResponse(code = 400, message = "Bad request", response = ErrorResponse.class),
        @ApiResponse(code = 401, message = "Unauthorized", response = ErrorResponse.class),
        @ApiResponse(code = 500, message = "Server error", response = ErrorResponse.class)
    })
    @AuthRequest
    public CompletionStage<Result> queryMindsDB(Http.Request request) {
        JsonNode json = request.body().asJson();
        if (json == null) {
            return CompletableFuture.completedFuture(
                badRequest(Json.toJson(new ErrorResponse("Missing request body", "MISSING_BODY", 400)))
            );
        }
        
        QueryRequest queryRequest;
        try {
            queryRequest = Json.fromJson(json, QueryRequest.class);
        } catch (Exception e) {
            return CompletableFuture.completedFuture(
                badRequest(Json.toJson(new ErrorResponse("Invalid request body", "VALIDATION_ERROR", 400)))
            );
        }
        
        long startTime = System.currentTimeMillis();
        String queryUrl = mindsdbUrl + "/api/sql/query";
        
        ObjectNode requestBody = Json.newObject();
        requestBody.put("query", queryRequest.getQuery());
        
        Duration timeout = queryRequest.getTimeout() != null 
            ? Duration.ofSeconds(queryRequest.getTimeout()) 
            : mindsdbTimeout;
        
        return wsClient.url(queryUrl)
            .addHeader("Content-Type", "application/json")
            .addHeader("Accept", "application/json")
            .setRequestTimeout(timeout)
            .post(requestBody)
            .thenApply(response -> {
                long executionTime = System.currentTimeMillis() - startTime;
                
                if (response.getStatus() == 200) {
                    JsonNode responseJson = response.asJson();
                    ArrayNode data;
                    
                    if (responseJson.isArray()) {
                        data = (ArrayNode) responseJson;
                    } else if (responseJson.isObject()) {
                        data = Json.newArray();
                        data.add(responseJson);
                    } else {
                        data = Json.newArray();
                    }
                    
                    ObjectNode metadata = Json.newObject();
                    metadata.put("query", queryRequest.getQuery());
                    metadata.put("timestamp", System.currentTimeMillis());
                    
                    QueryResponse queryResponse = new QueryResponse(
                        data,
                        data.size(),
                        executionTime,
                        metadata
                    );
                    
                    return ok(Json.toJson(queryResponse));
                } else if (response.getStatus() == 400) {
                    ObjectNode details = Json.newObject();
                    details.put("query", queryRequest.getQuery());
                    return badRequest(Json.toJson(new ErrorResponse(
                        "Invalid query syntax",
                        "QUERY_SYNTAX_ERROR",
                        400,
                        details
                    )));
                } else {
                    ObjectNode details = Json.newObject();
                    details.put("status", response.getStatus());
                    details.put("body", response.getBody());
                    return internalServerError(Json.toJson(new ErrorResponse(
                        "MindsDB query failed",
                        "MINDSDB_ERROR",
                        500,
                        details
                    )));
                }
            })
            .exceptionally(e -> {
                if (e.getCause() instanceof java.util.concurrent.TimeoutException) {
                    logger.error("MindsDB query timeout", e);
                    ObjectNode details = Json.newObject();
                    details.put("timeout", timeout.toString());
                    return status(408, Json.toJson(new ErrorResponse(
                        "Query timeout",
                        "QUERY_TIMEOUT",
                        408,
                        details
                    )));
                } else {
                    logger.error("MindsDB query error", e);
                    return internalServerError(Json.toJson(new ErrorResponse(
                        "Internal server error",
                        "SERVER_ERROR",
                        500
                    )));
                }
            });
    }
}
