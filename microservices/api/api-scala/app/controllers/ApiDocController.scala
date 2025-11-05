package controllers

import javax.inject._
import play.api.mvc._
import play.api.libs.json._
import io.swagger.config.{FilterFactory, Scanner}
import io.swagger.core.filter.SpecFilter
import io.swagger.models.Swagger
import io.swagger.util.Json

/**
 * Swagger API documentation controller
 */
@Singleton
class ApiDocController @Inject()(
  val controllerComponents: ControllerComponents
) extends BaseController {

  /**
   * Return Swagger JSON specification
   */
  def swagger = Action { implicit request =>
    val scanner = Scanner.getScannerFromProperty()
    val classes = scanner.classes()
    val swagger = new io.swagger.jaxrs.Reader(new Swagger()).read(classes)
    
    // Configure Swagger info
    val info = new io.swagger.models.Info()
      .title("Nexpo API - Scala Play Framework")
      .version("1.0.0")
      .description("Production-ready API backend with Play Framework and Auth0")
      .contact(new io.swagger.models.Contact()
        .name("API Support")
        .email("support@nexpo.com"))
      .license(new io.swagger.models.License()
        .name("MIT")
        .url("https://opensource.org/licenses/MIT"))
    
    swagger.setInfo(info)
    swagger.setHost(request.host)
    swagger.setBasePath("/")
    swagger.addScheme(if (request.secure) io.swagger.models.Scheme.HTTPS else io.swagger.models.Scheme.HTTP)
    
    // Add security definitions
    val bearerAuth = new io.swagger.models.auth.ApiKeyAuthDefinition()
    bearerAuth.setType("apiKey")
    bearerAuth.setIn(io.swagger.models.auth.In.HEADER)
    bearerAuth.setName("Authorization")
    bearerAuth.setDescription("Auth0 JWT token. Format: Bearer {token}")
    
    swagger.addSecurityDefinition("Bearer", bearerAuth)
    
    // Add tags
    swagger.addTag(new io.swagger.models.Tag()
      .name("Health")
      .description("Health check operations"))
    swagger.addTag(new io.swagger.models.Tag()
      .name("Auth")
      .description("Authentication operations"))
    swagger.addTag(new io.swagger.models.Tag()
      .name("User")
      .description("User management operations"))
    swagger.addTag(new io.swagger.models.Tag()
      .name("Data")
      .description("Data query operations"))
    
    // Apply filters if any
    val filter = FilterFactory.getFilter()
    if (filter != null) {
      val filtered = new SpecFilter().filter(swagger, filter, null, null, null)
      Ok(Json.pretty(filtered)).as("application/json")
    } else {
      Ok(Json.pretty(swagger)).as("application/json")
    }
  }
}
