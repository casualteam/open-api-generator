package casualteam.openapigenerator

import casualteam.openapigenerator.Operation.RequestBody
import io.swagger.v3.oas.models.parameters.{ RequestBody => OpenApiRequestBody }
import io.swagger.v3.oas.models.{ Operation => OpenApiOperation }

import scala.jdk.CollectionConverters._

case class Operation(
  method: String,
  name: String,
  parameters: List[Parameter],
  requestBody: Option[RequestBody],
  responses: Map[String, Response])

object Operation {

  case class RequestBody(
    required: Boolean,
    contentTypeModels: Map[String, MediaTypeModel])

  def getRequestBody(requestBody: OpenApiRequestBody): RequestBody = {
    val contentTypeModels = Option(requestBody.getContent).map(MediaTypeModel.getMediaTypeModels).getOrElse(Map.empty)
    RequestBody(
      required = requestBody.getRequired,
      contentTypeModels = contentTypeModels)
  }

  def getOperation(method: String)(operation: OpenApiOperation): Operation = {
    val requestBody = Option(operation.getRequestBody).map(getRequestBody)
    val parameters = Option(operation.getParameters).map(_.asScala).getOrElse(Nil).map(Parameter.getParameter).toList
    val responses = operation.getResponses.asScala.toMap.view.mapValues(Response.getResponses).toMap
    Operation(
      method = method,
      parameters = parameters,
      name = operation.getOperationId,
      requestBody = requestBody,
      responses = responses)
  }

}