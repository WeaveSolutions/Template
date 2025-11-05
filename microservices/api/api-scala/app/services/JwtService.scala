package services

import com.auth0.jwk.{Jwk, JwkProvider, UrlJwkProvider}
import com.auth0.jwt.JWT
import com.auth0.jwt.algorithms.Algorithm
import com.auth0.jwt.interfaces.{DecodedJWT, JWTVerifier}
import com.auth0.jwt.exceptions.{JWTVerificationException, TokenExpiredException}
import play.api.{Configuration, Logging}
import javax.inject.{Inject, Singleton}
import scala.util.{Try, Success, Failure}
import java.security.interfaces.RSAPublicKey
import scala.concurrent.{ExecutionContext, Future}

/**
 * JWT validation service for Auth0 tokens
 */
@Singleton
class JwtService @Inject()(
  config: Configuration
)(implicit ec: ExecutionContext) extends Logging {

  private val auth0Domain = config.get[String]("app.auth0.domain")
  private val auth0Audience = config.get[String]("app.auth0.audience")
  private val auth0Issuer = config.get[String]("app.auth0.issuer")
  private val jwksUrl = config.get[String]("app.auth0.jwksUrl")
  
  private lazy val jwkProvider: JwkProvider = new UrlJwkProvider(jwksUrl)
  
  /**
   * Validate JWT token
   */
  def validateToken(token: String): Future[Either[String, DecodedJWT]] = Future {
    Try {
      // Decode the token to get the key ID
      val decodedToken = JWT.decode(token)
      val keyId = decodedToken.getKeyId
      
      if (keyId == null) {
        Left("No key ID found in token")
      } else {
        // Get the JWK from Auth0
        val jwk = jwkProvider.get(keyId)
        val publicKey = jwk.getPublicKey.asInstanceOf[RSAPublicKey]
        val algorithm = Algorithm.RSA256(publicKey, null)
        
        // Create verifier with all required claims
        val verifier: JWTVerifier = JWT.require(algorithm)
          .withIssuer(auth0Issuer)
          .withAudience(auth0Audience)
          .build()
        
        // Verify the token
        val verified = verifier.verify(token)
        Right(verified)
      }
    } match {
      case Success(result) => result
      case Failure(e: TokenExpiredException) =>
        logger.debug("Token expired", e)
        Left("Token expired")
      case Failure(e: JWTVerificationException) =>
        logger.debug("Token verification failed", e)
        Left(s"Invalid token: ${e.getMessage}")
      case Failure(e) =>
        logger.error("Unexpected error validating token", e)
        Left("Token validation error")
    }
  }
  
  /**
   * Extract user ID from token
   */
  def getUserId(token: DecodedJWT): String = {
    token.getSubject
  }
  
  /**
   * Extract user email from token
   */
  def getUserEmail(token: DecodedJWT): Option[String] = {
    Option(token.getClaim("email").asString())
  }
  
  /**
   * Extract user roles from token
   */
  def getUserRoles(token: DecodedJWT): Seq[String] = {
    val rolesClaim = token.getClaim("roles")
    if (rolesClaim.isNull) {
      Seq.empty
    } else {
      rolesClaim.asArray(classOf[String]).toSeq
    }
  }
  
  /**
   * Extract user permissions from token
   */
  def getUserPermissions(token: DecodedJWT): Seq[String] = {
    val permissionsClaim = token.getClaim("permissions")
    if (permissionsClaim.isNull) {
      Seq.empty
    } else {
      permissionsClaim.asArray(classOf[String]).toSeq
    }
  }
  
  /**
   * Check if token has specific scope
   */
  def hasScope(token: DecodedJWT, scope: String): Boolean = {
    val scopeClaim = token.getClaim("scope")
    if (scopeClaim.isNull) {
      false
    } else {
      scopeClaim.asString().split(" ").contains(scope)
    }
  }
  
  /**
   * Check if token has specific permission
   */
  def hasPermission(token: DecodedJWT, permission: String): Boolean = {
    getUserPermissions(token).contains(permission)
  }
}
