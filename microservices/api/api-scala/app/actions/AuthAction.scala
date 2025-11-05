package actions

import javax.inject.Inject
import play.api.mvc._
import play.api.libs.json.Json
import services.JwtService
import models.ErrorResponse
import scala.concurrent.{ExecutionContext, Future}
import com.auth0.jwt.interfaces.DecodedJWT

/**
 * Request with authenticated user token
 */
class AuthRequest[A](val token: DecodedJWT, request: Request[A]) extends WrappedRequest[A](request)

/**
 * Action builder for authenticated endpoints
 */
class AuthAction @Inject()(
  parser: BodyParsers.Default,
  jwtService: JwtService
)(implicit ec: ExecutionContext) extends ActionBuilder[AuthRequest, AnyContent] {

  override def parser: BodyParser[AnyContent] = parser
  override protected def executionContext: ExecutionContext = ec

  override def invokeBlock[A](
    request: Request[A],
    block: AuthRequest[A] => Future[Result]
  ): Future[Result] = {
    
    // Extract token from Authorization header
    val tokenOpt = request.headers.get("Authorization").flatMap { authHeader =>
      if (authHeader.startsWith("Bearer ")) {
        Some(authHeader.substring(7))
      } else {
        None
      }
    }
    
    tokenOpt match {
      case None =>
        Future.successful(Results.Unauthorized(Json.toJson(
          ErrorResponse("Missing authorization token", Some("NO_TOKEN"), 401)
        )))
        
      case Some(token) =>
        jwtService.validateToken(token).flatMap {
          case Left(error) =>
            Future.successful(Results.Unauthorized(Json.toJson(
              ErrorResponse(s"Invalid token: $error", Some("INVALID_TOKEN"), 401)
            )))
            
          case Right(decodedToken) =>
            block(new AuthRequest(decodedToken, request))
        }
    }
  }
}
