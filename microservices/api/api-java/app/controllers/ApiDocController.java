package controllers;

import io.swagger.config.FilterFactory;
import io.swagger.config.Scanner;
import io.swagger.config.ScannerFactory;
import io.swagger.core.filter.SpecFilter;
import io.swagger.models.*;
import io.swagger.models.auth.ApiKeyAuthDefinition;
import io.swagger.models.auth.In;
import io.swagger.util.Json;
import play.mvc.Controller;
import play.mvc.Result;
import play.mvc.Http;
import javax.inject.Inject;
import javax.inject.Singleton;

/**
 * Swagger API documentation controller
 */
@Singleton
public class ApiDocController extends Controller {
    
    /**
     * Return Swagger JSON specification
     */
    public Result swagger(Http.Request request) {
        Scanner scanner = ScannerFactory.getScanner();
        Swagger swagger = new io.swagger.jaxrs.Reader(new Swagger()).read(scanner.classes());
        
        // Configure Swagger info
        Info info = new Info()
            .title("Nexpo API - Java Play Framework")
            .version("1.0.0")
            .description("Production-ready API backend with Java Play Framework and Auth0")
            .contact(new Contact()
                .name("API Support")
                .email("support@nexpo.com"))
            .license(new License()
                .name("MIT")
                .url("https://opensource.org/licenses/MIT"));
        
        swagger.setInfo(info);
        swagger.setHost(request.host());
        swagger.setBasePath("/");
        swagger.addScheme(request.secure() ? Scheme.HTTPS : Scheme.HTTP);
        
        // Add security definitions
        ApiKeyAuthDefinition bearerAuth = new ApiKeyAuthDefinition();
        bearerAuth.setType("apiKey");
        bearerAuth.setIn(In.HEADER);
        bearerAuth.setName("Authorization");
        bearerAuth.setDescription("Auth0 JWT token. Format: Bearer {token}");
        
        swagger.addSecurityDefinition("Bearer", bearerAuth);
        
        // Add tags
        swagger.addTag(new Tag()
            .name("Health")
            .description("Health check operations"));
        swagger.addTag(new Tag()
            .name("Auth")
            .description("Authentication operations"));
        swagger.addTag(new Tag()
            .name("User")
            .description("User management operations"));
        swagger.addTag(new Tag()
            .name("Data")
            .description("Data query operations"));
        
        // Apply filters if any
        if (FilterFactory.getFilter() != null) {
            swagger = new SpecFilter().filter(swagger, FilterFactory.getFilter(), null, null, null);
        }
        
        return ok(Json.pretty(swagger)).as("application/json");
    }
}
