import play.api._
import play.api.ApplicationLoader.Context
import play.api.routing.Router
import play.filters.HttpFiltersComponents
import play.filters.cors.CORSComponents
import play.api.mvc._
import play.api.libs.ws.ahc.AhcWSComponents
import modules.{AuthModule, SwaggerModule}
import router.Routes

/**
 * Application loader with compile-time dependency injection
 */
class ApplicationLoader extends play.api.ApplicationLoader {
  def load(context: Context): Application = {
    LoggerConfigurator(context.environment.classLoader).foreach {
      _.configure(context.environment, context.initialConfiguration, Map.empty)
    }
    new ApplicationComponents(context).application
  }
}

/**
 * Application components
 */
class ApplicationComponents(context: Context)
    extends BuiltInComponentsFromContext(context)
    with HttpFiltersComponents
    with CORSComponents
    with AhcWSComponents
    with _root_.controllers.AssetsComponents {

  // Load modules
  private val authModule = new AuthModule(environment, configuration)
  private val swaggerModule = new SwaggerModule(environment, configuration)
  
  // Services
  lazy val jwtService = new services.JwtService(configuration)(executionContext)
  lazy val authAction = new actions.AuthAction(playBodyParsers.default, jwtService)(executionContext)
  
  // Controllers
  lazy val healthController = new controllers.HealthController(controllerComponents, configuration)
  lazy val authController = new controllers.AuthController(controllerComponents, wsClient, configuration)(executionContext)
  lazy val userController = new controllers.UserController(controllerComponents, authAction, jwtService)(executionContext)
  lazy val dataController = new controllers.DataController(controllerComponents, authAction, wsClient, configuration)(executionContext)
  lazy val apiDocController = new controllers.ApiDocController(controllerComponents)
  
  // Routes
  lazy val router: Router = new Routes(
    httpErrorHandler,
    healthController,
    assets,
    apiDocController,
    authController,
    userController,
    dataController
  )
  
  // Override http filters
  override lazy val httpFilters: Seq[play.api.mvc.EssentialFilter] = {
    super.httpFilters ++ Seq(corsFilter)
  }
}
