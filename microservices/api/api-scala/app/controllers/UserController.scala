package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import play.api.libs.json._
import models._
import services.JwtService
import actions.AuthAction
import io.swagger.annotations._
import scala.concurrent.{ExecutionContext, Future}

/**
 * User management controller
 */
@Api(value = "User", description = "User management operations")
@Singleton
class UserController @Inject()(
  val controllerComponents: ControllerComponents,
  authAction: AuthAction,
  jwtService: JwtService
)(implicit ec: ExecutionContext) extends BaseController with Logging {

  /**
   * Get user profile
   */
  @ApiOperation(
    value = "Get user profile",
    notes = "Returns the authenticated user's profile",
    httpMethod = "GET",
    response = classOf[UserProfile],
    authorizations = Array(new Authorization("Bearer"))
  )
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "User profile", response = classOf[UserProfile]),
    new ApiResponse(code = 401, message = "Unauthorized", response = classOf[ErrorResponse])
  ))
  def getProfile = authAction.async { implicit request =>
    Future.successful {
      val token = request.token
      val userId = jwtService.getUserId(token)
      val email = jwtService.getUserEmail(token).getOrElse("")
      val roles = jwtService.getUserRoles(token)
      val permissions = jwtService.getUserPermissions(token)
      
      // Get user name from token claims
      val name = Option(token.getClaim("name").asString()).getOrElse(email.split("@").head)
      val picture = Option(token.getClaim("picture").asString())
      val emailVerified = Option(token.getClaim("email_verified").asBoolean()).getOrElse(false)
      
      val profile = UserProfile(
        id = userId,
        email = email,
        name = name,
        picture = picture,
        email_verified = emailVerified,
        roles = roles,
        permissions = permissions,
        metadata = Some(Json.obj(
          "last_login" -> token.getIssuedAt.toString,
          "token_exp" -> token.getExpiresAt.toString
        ))
      )
      
      Ok(Json.toJson(profile))
    }
  }

  /**
   * Get linked accounts
   */
  @ApiOperation(
    value = "Get linked accounts",
    notes = "Returns the user's linked social accounts",
    httpMethod = "GET",
    response = classOf[LinkedAccount],
    responseContainer = "List",
    authorizations = Array(new Authorization("Bearer"))
  )
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "List of linked accounts", response = classOf[Array[LinkedAccount]]),
    new ApiResponse(code = 401, message = "Unauthorized", response = classOf[ErrorResponse])
  ))
  def getLinkedAccounts = authAction.async { implicit request =>
    Future.successful {
      // Mock data - in production, this would query Auth0 Management API
      val linkedAccounts = Seq(
        LinkedAccount(
          provider = "google-oauth2",
          userId = "123456789",
          linked = true,
          linkedAt = Some("2024-01-01T10:00:00Z")
        ),
        LinkedAccount(
          provider = "github",
          userId = "987654321",
          linked = true,
          linkedAt = Some("2024-01-05T15:30:00Z")
        )
      )
      
      Ok(Json.toJson(linkedAccounts))
    }
  }
}
