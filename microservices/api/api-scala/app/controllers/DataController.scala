package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.ws._
import models._
import services.JwtService
import actions.AuthAction
import io.swagger.annotations._
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._

/**
 * Data controller for MindsDB integration
 */
@Api(value = "Data", description = "Data query operations")
@Singleton
class DataController @Inject()(
  val controllerComponents: ControllerComponents,
  authAction: AuthAction,
  ws: WSClient,
  config: Configuration
)(implicit ec: ExecutionContext) extends BaseController with Logging {

  private val mindsdbUrl = config.get[String]("app.mindsdb.url")
  private val mindsdbTimeout = config.get[Duration]("app.mindsdb.timeout")

  /**
   * Query MindsDB
   */
  @ApiOperation(
    value = "Query MindsDB",
    notes = "Execute SQL queries against MindsDB",
    httpMethod = "POST",
    response = classOf[QueryResponse],
    authorizations = Array(new Authorization("Bearer"))
  )
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Query request",
      required = true,
      dataType = "models.QueryRequest",
      paramType = "body"
    )
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Query results", response = classOf[QueryResponse]),
    new ApiResponse(code = 400, message = "Bad request", response = classOf[ErrorResponse]),
    new ApiResponse(code = 401, message = "Unauthorized", response = classOf[ErrorResponse]),
    new ApiResponse(code = 500, message = "Server error", response = classOf[ErrorResponse])
  ))
  def queryMindsDB = authAction.async(parse.json) { implicit request =>
    request.body.validate[QueryRequest] match {
      case JsError(errors) =>
        Future.successful(BadRequest(Json.toJson(
          ErrorResponse("Invalid request body", Some("VALIDATION_ERROR"), 400)
        )))
        
      case JsSuccess(queryRequest, _) =>
        val startTime = System.currentTimeMillis()
        val queryUrl = s"$mindsdbUrl/api/sql/query"
        
        ws.url(queryUrl)
          .withHttpHeaders(
            "Content-Type" -> "application/json",
            "Accept" -> "application/json"
          )
          .withRequestTimeout(queryRequest.timeout.map(_.seconds).getOrElse(mindsdbTimeout))
          .post(Json.obj("query" -> queryRequest.query))
          .map { response =>
            val executionTime = System.currentTimeMillis() - startTime
            
            response.status match {
              case 200 =>
                val data = response.json match {
                  case JsArray(arr) => JsArray(arr)
                  case obj: JsObject => JsArray(Seq(obj))
                  case _ => JsArray(Seq())
                }
                
                val queryResponse = QueryResponse(
                  data = data,
                  count = data.value.size,
                  executionTime = executionTime,
                  metadata = Some(Json.obj(
                    "query" -> queryRequest.query,
                    "timestamp" -> System.currentTimeMillis()
                  ))
                )
                Ok(Json.toJson(queryResponse))
                
              case 400 =>
                BadRequest(Json.toJson(
                  ErrorResponse(
                    "Invalid query syntax",
                    Some("QUERY_SYNTAX_ERROR"),
                    400,
                    Some(Json.obj("query" -> queryRequest.query))
                  )
                ))
                
              case _ =>
                InternalServerError(Json.toJson(
                  ErrorResponse(
                    "MindsDB query failed",
                    Some("MINDSDB_ERROR"),
                    500,
                    Some(Json.obj(
                      "status" -> response.status,
                      "body" -> response.body
                    ))
                  )
                ))
            }
          }
          .recover {
            case e: java.util.concurrent.TimeoutException =>
              logger.error("MindsDB query timeout", e)
              RequestTimeout(Json.toJson(
                ErrorResponse(
                  "Query timeout",
                  Some("QUERY_TIMEOUT"),
                  408,
                  Some(Json.obj("timeout" -> mindsdbTimeout.toString))
                )
              ))
              
            case e: Exception =>
              logger.error("MindsDB query error", e)
              InternalServerError(Json.toJson(
                ErrorResponse("Internal server error", Some("SERVER_ERROR"), 500)
              ))
          }
    }
  }
}
