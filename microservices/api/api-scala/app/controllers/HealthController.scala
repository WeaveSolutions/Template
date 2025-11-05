package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import play.api.libs.json._
import models.HealthResponse
import io.swagger.annotations._
import java.time.Instant

/**
 * Health check controller
 */
@Api(value = "Health", description = "Health check operations")
@Singleton
class HealthController @Inject()(
  val controllerComponents: ControllerComponents,
  config: Configuration
) extends BaseController {

  private val appName = config.get[String]("app.name")
  private val appVersion = config.get[String]("app.version")

  /**
   * Root endpoint
   */
  @ApiOperation(
    value = "Root endpoint",
    notes = "Returns service information",
    httpMethod = "GET",
    response = classOf[HealthResponse]
  )
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Service information", response = classOf[HealthResponse])
  ))
  def index = Action { implicit request: Request[AnyContent] =>
    val response = HealthResponse(
      status = "healthy",
      service = appName,
      version = appVersion,
      timestamp = Instant.now().toString
    )
    Ok(Json.toJson(response))
  }

  /**
   * Health check endpoint
   */
  @ApiOperation(
    value = "Health check",
    notes = "Returns the health status of the service",
    httpMethod = "GET",
    response = classOf[HealthResponse]
  )
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Service is healthy", response = classOf[HealthResponse])
  ))
  def health = Action { implicit request: Request[AnyContent] =>
    val response = HealthResponse(
      status = "healthy",
      service = appName,
      version = appVersion,
      timestamp = Instant.now().toString
    )
    Ok(Json.toJson(response))
  }
}
