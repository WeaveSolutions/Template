package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.ws._
import models._
import io.swagger.annotations._
import scala.concurrent.{ExecutionContext, Future}

/**
 * Authentication controller
 */
@Api(value = "Auth", description = "Authentication operations")
@Singleton
class AuthController @Inject()(
  val controllerComponents: ControllerComponents,
  ws: WSClient,
  config: Configuration
)(implicit ec: ExecutionContext) extends BaseController with Logging {

  private val auth0Domain = config.get[String]("app.auth0.domain")
  private val auth0ClientId = config.get[String]("app.auth0.clientId")
  private val auth0ClientSecret = config.get[String]("app.auth0.clientSecret")
  private val auth0Audience = config.get[String]("app.auth0.audience")

  /**
   * Exchange authorization code for tokens
   */
  @ApiOperation(
    value = "Exchange token",
    notes = "Exchange authorization code or refresh token for access tokens",
    httpMethod = "POST",
    response = classOf[TokenResponse]
  )
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Token request",
      required = true,
      dataType = "models.TokenRequest",
      paramType = "body"
    )
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Token response", response = classOf[TokenResponse]),
    new ApiResponse(code = 400, message = "Bad request", response = classOf[ErrorResponse]),
    new ApiResponse(code = 401, message = "Unauthorized", response = classOf[ErrorResponse])
  ))
  def exchangeToken = Action.async(parse.json) { implicit request =>
    request.body.validate[TokenRequest] match {
      case JsError(errors) =>
        Future.successful(BadRequest(Json.toJson(
          ErrorResponse("Invalid request body", Some("VALIDATION_ERROR"), 400)
        )))
        
      case JsSuccess(tokenRequest, _) =>
        val tokenUrl = s"https://$auth0Domain/oauth/token"
        
        val requestBody = Json.obj(
          "grant_type" -> tokenRequest.grant_type,
          "client_id" -> auth0ClientId,
          "client_secret" -> auth0ClientSecret,
          "audience" -> auth0Audience
        ) ++ (tokenRequest.grant_type match {
          case "authorization_code" =>
            Json.obj(
              "code" -> tokenRequest.code.getOrElse(""),
              "redirect_uri" -> tokenRequest.redirect_uri.getOrElse("")
            )
          case "refresh_token" =>
            Json.obj(
              "refresh_token" -> tokenRequest.refresh_token.getOrElse("")
            )
          case _ => Json.obj()
        })
        
        ws.url(tokenUrl)
          .withHttpHeaders("Content-Type" -> "application/json")
          .post(requestBody)
          .map { response =>
            response.status match {
              case 200 =>
                val json = response.json
                val tokenResponse = TokenResponse(
                  access_token = (json \ "access_token").as[String],
                  token_type = (json \ "token_type").as[String],
                  expires_in = (json \ "expires_in").as[Int],
                  refresh_token = (json \ "refresh_token").asOpt[String],
                  id_token = (json \ "id_token").asOpt[String],
                  scope = (json \ "scope").asOpt[String]
                )
                Ok(Json.toJson(tokenResponse))
                
              case 400 =>
                BadRequest(Json.toJson(
                  ErrorResponse(
                    (response.json \ "error_description").asOpt[String].getOrElse("Bad request"),
                    (response.json \ "error").asOpt[String],
                    400
                  )
                ))
                
              case 401 =>
                Unauthorized(Json.toJson(
                  ErrorResponse(
                    (response.json \ "error_description").asOpt[String].getOrElse("Unauthorized"),
                    (response.json \ "error").asOpt[String],
                    401
                  )
                ))
                
              case _ =>
                InternalServerError(Json.toJson(
                  ErrorResponse("Token exchange failed", Some("EXCHANGE_ERROR"), 500)
                ))
            }
          }
          .recover {
            case e: Exception =>
              logger.error("Token exchange error", e)
              InternalServerError(Json.toJson(
                ErrorResponse("Internal server error", Some("SERVER_ERROR"), 500)
              ))
          }
    }
  }
}
