package controllers;

import actions.AuthRequest;
import com.auth0.jwt.interfaces.DecodedJWT;
import com.fasterxml.jackson.databind.node.ObjectNode;
import io.swagger.annotations.*;
import models.LinkedAccount;
import models.UserProfile;
import play.libs.Json;
import play.mvc.Controller;
import play.mvc.Result;
import play.mvc.Http;
import services.JwtService;
import javax.inject.Inject;
import javax.inject.Singleton;
import java.util.Arrays;
import java.util.List;

/**
 * User management controller
 */
@Api(value = "User", description = "User management operations")
@Singleton
public class UserController extends Controller {
    
    private final JwtService jwtService;
    
    @Inject
    public UserController(JwtService jwtService) {
        this.jwtService = jwtService;
    }
    
    /**
     * Get user profile
     */
    @ApiOperation(
        value = "Get user profile",
        notes = "Returns the authenticated user's profile",
        httpMethod = "GET",
        response = UserProfile.class,
        authorizations = {@Authorization("Bearer")}
    )
    @ApiResponses(value = {
        @ApiResponse(code = 200, message = "User profile", response = UserProfile.class),
        @ApiResponse(code = 401, message = "Unauthorized", response = models.ErrorResponse.class)
    })
    @AuthRequest
    public Result getProfile(Http.Request request) {
        DecodedJWT token = request.attrs().get(AuthRequest.TOKEN);
        
        UserProfile profile = new UserProfile();
        profile.setId(jwtService.getUserId(token));
        profile.setEmail(jwtService.getUserEmail(token));
        
        // Get user name from token claims
        String name = token.getClaim("name").asString();
        profile.setName(name != null ? name : profile.getEmail().split("@")[0]);
        
        profile.setPicture(token.getClaim("picture").asString());
        profile.setEmailVerified(
            token.getClaim("email_verified").asBoolean() != null 
            ? token.getClaim("email_verified").asBoolean() 
            : false
        );
        
        profile.setRoles(jwtService.getUserRoles(token));
        profile.setPermissions(jwtService.getUserPermissions(token));
        
        // Add metadata
        ObjectNode metadata = Json.newObject();
        metadata.put("last_login", token.getIssuedAt().toString());
        metadata.put("token_exp", token.getExpiresAt().toString());
        profile.setMetadata(metadata);
        
        return ok(Json.toJson(profile));
    }
    
    /**
     * Get linked accounts
     */
    @ApiOperation(
        value = "Get linked accounts",
        notes = "Returns the user's linked social accounts",
        httpMethod = "GET",
        response = LinkedAccount.class,
        responseContainer = "List",
        authorizations = {@Authorization("Bearer")}
    )
    @ApiResponses(value = {
        @ApiResponse(code = 200, message = "List of linked accounts", response = LinkedAccount.class, responseContainer = "List"),
        @ApiResponse(code = 401, message = "Unauthorized", response = models.ErrorResponse.class)
    })
    @AuthRequest
    public Result getLinkedAccounts(Http.Request request) {
        // Mock data - in production, this would query Auth0 Management API
        List<LinkedAccount> linkedAccounts = Arrays.asList(
            new LinkedAccount("google-oauth2", "123456789", true, "2024-01-01T10:00:00Z"),
            new LinkedAccount("github", "987654321", true, "2024-01-05T15:30:00Z")
        );
        
        return ok(Json.toJson(linkedAccounts));
    }
}
