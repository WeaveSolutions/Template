import com.fasterxml.jackson.databind.node.ObjectNode;
import models.ErrorResponse;
import play.Logger;
import play.http.HttpErrorHandler;
import play.libs.Json;
import play.mvc.Http;
import play.mvc.Result;
import play.mvc.Results;
import javax.inject.Singleton;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;

/**
 * Global error handler
 */
@Singleton
public class ErrorHandler implements HttpErrorHandler {
    
    private static final Logger.ALogger logger = Logger.of(ErrorHandler.class);
    
    @Override
    public CompletionStage<Result> onClientError(Http.RequestHeader request, int statusCode, String message) {
        String errorCode;
        if (statusCode == 400) {
            errorCode = "BAD_REQUEST";
        } else if (statusCode == 401) {
            errorCode = "UNAUTHORIZED";
        } else if (statusCode == 403) {
            errorCode = "FORBIDDEN";
        } else if (statusCode == 404) {
            errorCode = "NOT_FOUND";
        } else if (statusCode == 405) {
            errorCode = "METHOD_NOT_ALLOWED";
        } else {
            errorCode = "CLIENT_ERROR";
        }
        
        ErrorResponse error = new ErrorResponse(message, errorCode, statusCode);
        
        return CompletableFuture.completedFuture(
            Results.status(statusCode, Json.toJson(error))
        );
    }
    
    @Override
    public CompletionStage<Result> onServerError(Http.RequestHeader request, Throwable exception) {
        logger.error("Server error on " + request.path(), exception);
        
        ObjectNode details = Json.newObject();
        details.put("path", request.path());
        details.put("method", request.method());
        
        ErrorResponse error = new ErrorResponse(
            "Internal server error",
            "SERVER_ERROR",
            500,
            details
        );
        
        return CompletableFuture.completedFuture(
            Results.internalServerError(Json.toJson(error))
        );
    }
}
