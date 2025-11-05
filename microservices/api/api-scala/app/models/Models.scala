package models

import play.api.libs.json._
import io.swagger.annotations._
import scala.annotation.meta.field

/**
 * Health check response
 */
@ApiModel(description = "Health check response")
case class HealthResponse(
  @(ApiModelProperty @field)(value = "Service status", example = "healthy") status: String,
  @(ApiModelProperty @field)(value = "Service name", example = "nexpo-api-scala") service: String,
  @(ApiModelProperty @field)(value = "Service version", example = "1.0.0") version: String,
  @(ApiModelProperty @field)(value = "Current timestamp", example = "2024-01-15T10:30:00Z") timestamp: String
)

object HealthResponse {
  implicit val format: Format[HealthResponse] = Json.format[HealthResponse]
}

/**
 * User profile
 */
@ApiModel(description = "User profile information")
case class UserProfile(
  @(ApiModelProperty @field)(value = "User ID", example = "auth0|123456") id: String,
  @(ApiModelProperty @field)(value = "Email address", example = "user@example.com") email: String,
  @(ApiModelProperty @field)(value = "Full name", example = "John Doe") name: String,
  @(ApiModelProperty @field)(value = "Profile picture URL", example = "https://example.com/avatar.jpg") picture: Option[String] = None,
  @(ApiModelProperty @field)(value = "Email verified status", example = "true") email_verified: Boolean = false,
  @(ApiModelProperty @field)(value = "User roles", example = "[\"user\", \"admin\"]") roles: Seq[String] = Seq.empty,
  @(ApiModelProperty @field)(value = "User permissions", example = "[\"read:profile\", \"write:profile\"]") permissions: Seq[String] = Seq.empty,
  @(ApiModelProperty @field)(value = "Custom metadata") metadata: Option[JsObject] = None
)

object UserProfile {
  implicit val format: Format[UserProfile] = Json.format[UserProfile]
}

/**
 * Linked account
 */
@ApiModel(description = "Linked account information")
case class LinkedAccount(
  @(ApiModelProperty @field)(value = "Provider name", example = "google-oauth2") provider: String,
  @(ApiModelProperty @field)(value = "Provider user ID", example = "123456789") userId: String,
  @(ApiModelProperty @field)(value = "Linked status", example = "true") linked: Boolean,
  @(ApiModelProperty @field)(value = "Linked timestamp", example = "2024-01-15T10:30:00Z") linkedAt: Option[String] = None
)

object LinkedAccount {
  implicit val format: Format[LinkedAccount] = Json.format[LinkedAccount]
}

/**
 * Token request
 */
@ApiModel(description = "Token exchange request")
case class TokenRequest(
  @(ApiModelProperty @field)(value = "Grant type", example = "authorization_code", required = true) grant_type: String,
  @(ApiModelProperty @field)(value = "Authorization code", example = "abc123", required = false) code: Option[String] = None,
  @(ApiModelProperty @field)(value = "Redirect URI", example = "http://localhost:3000/callback", required = false) redirect_uri: Option[String] = None,
  @(ApiModelProperty @field)(value = "Refresh token", example = "refresh_token_123", required = false) refresh_token: Option[String] = None
)

object TokenRequest {
  implicit val format: Format[TokenRequest] = Json.format[TokenRequest]
}

/**
 * Token response
 */
@ApiModel(description = "Token exchange response")
case class TokenResponse(
  @(ApiModelProperty @field)(value = "Access token", example = "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9...") access_token: String,
  @(ApiModelProperty @field)(value = "Token type", example = "Bearer") token_type: String,
  @(ApiModelProperty @field)(value = "Expires in seconds", example = "3600") expires_in: Int,
  @(ApiModelProperty @field)(value = "Refresh token", example = "refresh_token_123") refresh_token: Option[String] = None,
  @(ApiModelProperty @field)(value = "ID token", example = "id_token_123") id_token: Option[String] = None,
  @(ApiModelProperty @field)(value = "Token scope", example = "openid profile email") scope: Option[String] = None
)

object TokenResponse {
  implicit val format: Format[TokenResponse] = Json.format[TokenResponse]
}

/**
 * MindsDB query request
 */
@ApiModel(description = "MindsDB query request")
case class QueryRequest(
  @(ApiModelProperty @field)(value = "SQL query", example = "SELECT * FROM users LIMIT 10", required = true) query: String,
  @(ApiModelProperty @field)(value = "Query parameters", required = false) parameters: Option[JsObject] = None,
  @(ApiModelProperty @field)(value = "Query timeout in seconds", example = "30", required = false) timeout: Option[Int] = None
)

object QueryRequest {
  implicit val format: Format[QueryRequest] = Json.format[QueryRequest]
}

/**
 * Query response
 */
@ApiModel(description = "Query execution response")
case class QueryResponse(
  @(ApiModelProperty @field)(value = "Query results") data: JsArray,
  @(ApiModelProperty @field)(value = "Row count", example = "10") count: Int,
  @(ApiModelProperty @field)(value = "Execution time in ms", example = "125") executionTime: Long,
  @(ApiModelProperty @field)(value = "Query metadata") metadata: Option[JsObject] = None
)

object QueryResponse {
  implicit val format: Format[QueryResponse] = Json.format[QueryResponse]
}

/**
 * Error response
 */
@ApiModel(description = "Error response")
case class ErrorResponse(
  @(ApiModelProperty @field)(value = "Error message", example = "Invalid request") error: String,
  @(ApiModelProperty @field)(value = "Error code", example = "INVALID_REQUEST") code: Option[String] = None,
  @(ApiModelProperty @field)(value = "HTTP status code", example = "400") status: Int = 400,
  @(ApiModelProperty @field)(value = "Additional details") details: Option[JsObject] = None
)

object ErrorResponse {
  implicit val format: Format[ErrorResponse] = Json.format[ErrorResponse]
}
