package controllers;

import com.typesafe.config.Config;
import io.swagger.annotations.*;
import models.HealthResponse;
import play.libs.Json;
import play.mvc.Controller;
import play.mvc.Result;
import javax.inject.Inject;
import javax.inject.Singleton;
import java.time.Instant;

/**
 * Health check controller
 */
@Api(value = "Health", description = "Health check operations")
@Singleton
public class HealthController extends Controller {
    
    private final String appName;
    private final String appVersion;
    
    @Inject
    public HealthController(Config config) {
        this.appName = config.getString("app.name");
        this.appVersion = config.getString("app.version");
    }
    
    /**
     * Root endpoint
     */
    @ApiOperation(
        value = "Root endpoint",
        notes = "Returns service information",
        httpMethod = "GET",
        response = HealthResponse.class
    )
    @ApiResponses(value = {
        @ApiResponse(code = 200, message = "Service information", response = HealthResponse.class)
    })
    public Result index() {
        HealthResponse response = new HealthResponse(
            "healthy",
            appName,
            appVersion,
            Instant.now().toString()
        );
        return ok(Json.toJson(response));
    }
    
    /**
     * Health check endpoint
     */
    @ApiOperation(
        value = "Health check",
        notes = "Returns the health status of the service",
        httpMethod = "GET",
        response = HealthResponse.class
    )
    @ApiResponses(value = {
        @ApiResponse(code = 200, message = "Service is healthy", response = HealthResponse.class)
    })
    public Result health() {
        HealthResponse response = new HealthResponse(
            "healthy",
            appName,
            appVersion,
            Instant.now().toString()
        );
        return ok(Json.toJson(response));
    }
}
