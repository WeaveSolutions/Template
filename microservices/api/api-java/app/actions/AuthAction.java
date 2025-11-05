package actions;

import com.auth0.jwt.interfaces.DecodedJWT;
import models.ErrorResponse;
import play.libs.Json;
import play.mvc.Action;
import play.mvc.Http;
import play.mvc.Result;
import services.JwtService;
import javax.inject.Inject;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;

/**
 * Custom action for JWT authentication
 */
public class AuthAction extends Action<AuthRequest> {
    
    private final JwtService jwtService;
    
    @Inject
    public AuthAction(JwtService jwtService) {
        this.jwtService = jwtService;
    }
    
    @Override
    public CompletionStage<Result> call(Http.Request req) {
        // Extract token from Authorization header
        String authHeader = req.header("Authorization").orElse("");
        
        if (!authHeader.startsWith("Bearer ")) {
            ErrorResponse error = new ErrorResponse("Missing authorization token", "NO_TOKEN", 401);
            return CompletableFuture.completedFuture(
                unauthorized(Json.toJson(error))
            );
        }
        
        String token = authHeader.substring(7);
        
        return jwtService.validateToken(token).thenCompose(either -> {
            if (either.isLeft()) {
                ErrorResponse error = new ErrorResponse(
                    "Invalid token: " + either.getLeft(), 
                    "INVALID_TOKEN", 
                    401
                );
                return CompletableFuture.completedFuture(
                    unauthorized(Json.toJson(error))
                );
            } else {
                // Create authenticated request
                DecodedJWT decodedToken = either.getRight();
                Http.Request authenticatedRequest = req.addAttr(
                    AuthRequest.TOKEN, 
                    decodedToken
                );
                return delegate.call(authenticatedRequest);
            }
        });
    }
}
