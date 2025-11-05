package controllers;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.typesafe.config.Config;
import io.swagger.annotations.*;
import models.ErrorResponse;
import models.TokenRequest;
import models.TokenResponse;
import play.Logger;
import play.libs.Json;
import play.libs.ws.WSClient;
import play.mvc.Controller;
import play.mvc.Result;
import play.mvc.Http;
import javax.inject.Inject;
import javax.inject.Singleton;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;

/**
 * Authentication controller
 */
@Api(value = "Auth", description = "Authentication operations")
@Singleton
public class AuthController extends Controller {
    
    private static final Logger.ALogger logger = Logger.of(AuthController.class);
    
    private final WSClient wsClient;
    private final String auth0Domain;
    private final String auth0ClientId;
    private final String auth0ClientSecret;
    private final String auth0Audience;
    
    @Inject
    public AuthController(WSClient wsClient, Config config) {
        this.wsClient = wsClient;
        this.auth0Domain = config.getString("auth0.domain");
        this.auth0ClientId = config.getString("auth0.clientId");
        this.auth0ClientSecret = config.getString("auth0.clientSecret");
        this.auth0Audience = config.getString("auth0.audience");
    }
    
    /**
     * Exchange authorization code for tokens
     */
    @ApiOperation(
        value = "Exchange token",
        notes = "Exchange authorization code or refresh token for access tokens",
        httpMethod = "POST",
        response = TokenResponse.class
    )
    @ApiImplicitParams({
        @ApiImplicitParam(
            name = "body",
            value = "Token request",
            required = true,
            dataType = "models.TokenRequest",
            paramType = "body"
        )
    })
    @ApiResponses(value = {
        @ApiResponse(code = 200, message = "Token response", response = TokenResponse.class),
        @ApiResponse(code = 400, message = "Bad request", response = ErrorResponse.class),
        @ApiResponse(code = 401, message = "Unauthorized", response = ErrorResponse.class)
    })
    public CompletionStage<Result> exchangeToken(Http.Request request) {
        JsonNode json = request.body().asJson();
        if (json == null) {
            return CompletableFuture.completedFuture(
                badRequest(Json.toJson(new ErrorResponse("Missing request body", "MISSING_BODY", 400)))
            );
        }
        
        TokenRequest tokenRequest;
        try {
            tokenRequest = Json.fromJson(json, TokenRequest.class);
        } catch (Exception e) {
            return CompletableFuture.completedFuture(
                badRequest(Json.toJson(new ErrorResponse("Invalid request body", "VALIDATION_ERROR", 400)))
            );
        }
        
        String tokenUrl = "https://" + auth0Domain + "/oauth/token";
        
        ObjectNode requestBody = Json.newObject();
        requestBody.put("grant_type", tokenRequest.getGrantType());
        requestBody.put("client_id", auth0ClientId);
        requestBody.put("client_secret", auth0ClientSecret);
        requestBody.put("audience", auth0Audience);
        
        if ("authorization_code".equals(tokenRequest.getGrantType())) {
            requestBody.put("code", tokenRequest.getCode());
            requestBody.put("redirect_uri", tokenRequest.getRedirectUri());
        } else if ("refresh_token".equals(tokenRequest.getGrantType())) {
            requestBody.put("refresh_token", tokenRequest.getRefreshToken());
        }
        
        return wsClient.url(tokenUrl)
            .addHeader("Content-Type", "application/json")
            .post(requestBody)
            .thenApply(response -> {
                if (response.getStatus() == 200) {
                    JsonNode responseJson = response.asJson();
                    TokenResponse tokenResponse = new TokenResponse();
                    tokenResponse.setAccessToken(responseJson.get("access_token").asText());
                    tokenResponse.setTokenType(responseJson.get("token_type").asText());
                    tokenResponse.setExpiresIn(responseJson.get("expires_in").asInt());
                    
                    if (responseJson.has("refresh_token")) {
                        tokenResponse.setRefreshToken(responseJson.get("refresh_token").asText());
                    }
                    if (responseJson.has("id_token")) {
                        tokenResponse.setIdToken(responseJson.get("id_token").asText());
                    }
                    if (responseJson.has("scope")) {
                        tokenResponse.setScope(responseJson.get("scope").asText());
                    }
                    
                    return ok(Json.toJson(tokenResponse));
                } else if (response.getStatus() == 400) {
                    JsonNode errorJson = response.asJson();
                    return badRequest(Json.toJson(new ErrorResponse(
                        errorJson.has("error_description") ? errorJson.get("error_description").asText() : "Bad request",
                        errorJson.has("error") ? errorJson.get("error").asText() : null,
                        400
                    )));
                } else if (response.getStatus() == 401) {
                    JsonNode errorJson = response.asJson();
                    return unauthorized(Json.toJson(new ErrorResponse(
                        errorJson.has("error_description") ? errorJson.get("error_description").asText() : "Unauthorized",
                        errorJson.has("error") ? errorJson.get("error").asText() : null,
                        401
                    )));
                } else {
                    return internalServerError(Json.toJson(new ErrorResponse(
                        "Token exchange failed",
                        "EXCHANGE_ERROR",
                        500
                    )));
                }
            })
            .exceptionally(e -> {
                logger.error("Token exchange error", e);
                return internalServerError(Json.toJson(new ErrorResponse(
                    "Internal server error",
                    "SERVER_ERROR",
                    500
                )));
            });
    }
}
