package casualteam.openapigenerator

import casualteam.openapigenerator.MediaTypeModel.{ ApplicationForm, MultipartForm }
import casualteam.openapigenerator.Response.BaseResponse
import io.swagger.v3.oas.models.media._
import io.swagger.v3.oas.models.parameters.{ RequestBody => OpenApiRequestBody }
import io.swagger.v3.oas.models.responses.ApiResponse
import io.swagger.v3.oas.models.{ OpenAPI, Operation => OpenApiOperation }
import io.swagger.v3.oas.models.parameters.{ Parameter => OpenApiParameter }

import scala.jdk.CollectionConverters._

trait ApiProcess {

  def appendComputedValue(name: EntityName, value: String): EntityName =
    name.fold(n => Left(n :+ value), n => Left(List(n, value)))

  def getParameter(componentParamenters: Map[String, OpenApiParameter], parameter: OpenApiParameter): Parameter = {
    val (parameterName, actualParameter) = Option(parameter.get$ref)
      .map { ref =>
        val name = ref.split("/").last
        (name, componentParamenters.getOrElse(name, throw new Exception(s"Parameter $name not found")))
      }
      .getOrElse(parameter.getName, parameter)
    val model = getModel(Left(List(parameterName)), actualParameter.getSchema)
    actualParameter.getIn match {
      case "query" =>
        Parameter.Query(parameter.getName, model)
      case "path" =>
        Parameter.Path(parameter.getName, model)
      case "header" =>
        Parameter.Header(parameter.getName, model)
    }
  }

  def getMediaTypeModel(name: EntityName, contentType: String, mediaType: MediaType): MediaTypeModel = {
    val defaultModelName = appendComputedValue(name, contentType)
    contentType match {
      case "application/json" =>
        MediaTypeModel.ApplicationJson(
          model = getModel(defaultModelName, mediaType.getSchema))
      case "application/xml" =>
        MediaTypeModel.ApplicationXml(
          model = getModel(defaultModelName, mediaType.getSchema))
      case "application/x-www-form-urlencoded" =>
        ApplicationForm()
      case "multipart/form-data" =>
        MultipartForm()
    }
  }

  def getMediaTypeModels(name: EntityName, content: Content): Map[String, MediaTypeModel] = {
    content.asScala.iterator
      .map {
        case (contentType, mediaType) =>
          contentType -> getMediaTypeModel(name, contentType, mediaType)
      }.toMap
  }

  def getRequestBody(name: EntityName, requestBody: OpenApiRequestBody): RequestBody = {
    val contentTypeModels = Option(requestBody.getContent).map(getMediaTypeModels(name, _)).getOrElse(Map.empty)
    RequestBody(
      name = name,
      required = requestBody.getRequired,
      contentTypeModels = contentTypeModels)
  }

  def getResponse(name: EntityName, apiResponse: ApiResponse): Response = {
    Option(apiResponse.get$ref)
      .map(Response.Ref)
      .getOrElse {
        val contentTypeModels = getMediaTypeModels(name, apiResponse.getContent)
        Response.BaseResponse(
          name = name,
          contentTypeModels = contentTypeModels)
      }
  }

  def getOperation(method: String, componentParamenters: Map[String, OpenApiParameter], operation: OpenApiOperation): Operation = {
    val computedName = Left(List(operation.getOperationId))
    val requestBody = Option(operation.getRequestBody).map(getRequestBody(computedName, _))
    val parameters = Option(operation.getParameters).map(_.asScala).getOrElse(Nil).map(getParameter(componentParamenters, _)).toList
    val responses = operation.getResponses.asScala.iterator
      .map {
        case (k, v) =>
          val entityName = appendComputedValue(computedName, k)
          k -> getResponse(entityName, v)
      }
      .toMap
    Operation(
      method = method,
      parameters = parameters,
      name = operation.getOperationId,
      requestBody = requestBody,
      responses = responses)
  }

  def getModel(defaultName: EntityName, schema: Schema[_]): Model = {
    Option(schema.get$ref())
      .map(Model.Ref)
      .getOrElse {
        val modelName = Option(schema.getName).map(Right(_)).getOrElse(defaultName)
        schema match {
          case s: ObjectSchema =>
            val fields = Option(s.getProperties).map(_.asScala).getOrElse(Nil)
              .map { case (k, v) => k -> getModel(appendComputedValue(modelName, k), v) }
              .toMap
            Model.Object(
              name = modelName,
              fields = fields)
          case s: StringSchema =>
            Model.String(
              name = modelName)
          case s: ArraySchema =>
            Model.Array(
              name = modelName,
              itemModel = getModel(appendComputedValue(modelName, "Item"), s.getItems))
          case s: IntegerSchema =>
            Model.Integer(
              name = modelName)
          case s: DateTimeSchema =>
            Model.DateTime(
              name = modelName)
          case s: BooleanSchema =>
            Model.Boolean(
              name = modelName)
          case s: MapSchema =>
            s.getAdditionalProperties match {
              case true =>
                Model.FreeMap(
                  name = modelName)
              case getAdditionalPropertiesSchema: Schema[_] =>
                val model = getModel(appendComputedValue(modelName, "Values"), getAdditionalPropertiesSchema)
                Model.TypedMap(
                  name = modelName,
                  valuesModel = model)
            }
        }
      }
  }

  def process(openAPI: OpenAPI): (List[Model], List[Response], List[Operation]) = {
    def getModels(mediaTypeModel: MediaTypeModel) = {
      MediaTypeModel.fold(mediaTypeModel)(
        m => Some(m.model),
        m => Some(m.model),
        _ => None,
        _ => None)
    }
    val componentParameters = Option(openAPI.getComponents).flatMap(c => Option(c.getParameters)).map(_.asScala.toMap).getOrElse(Map.empty)
    val operations = openAPI.getPaths.asScala.values
      .flatMap { path =>
        Seq(
          Option(path.getDelete).map(getOperation("DELETE", componentParameters, _)),
          Option(path.getGet).map(getOperation("GET", componentParameters, _)),
          Option(path.getHead).map(getOperation("HEAD", componentParameters, _)),
          Option(path.getOptions).map(getOperation("OPTIONS", componentParameters, _)),
          Option(path.getPatch).map(getOperation("PATCH", componentParameters, _)),
          Option(path.getPost).map(getOperation("POST", componentParameters, _)),
          Option(path.getPut).map(getOperation("PUT", componentParameters, _)),
          Option(path.getTrace).map(getOperation("TRACE", componentParameters, _))).flatten
      }
      .toList
    val componentModels = Option(openAPI.getComponents)
      .flatMap(c => Option(c.getSchemas))
      .map(_.asScala.iterator)
      .getOrElse(Iterator.empty)
      .map { case (k, v) => getModel(Right(k), v) }
      .toList
      .distinct
    val operationModels = operations.flatMap { op =>
      val modelsFromResponses = op.responses.values
        .collect { case m: BaseResponse => m.contentTypeModels.values.flatMap(getModels) }
        .flatten
      val modelsFromRequests = op.requestBody.toList
        .flatMap(_.contentTypeModels.values)
        .flatMap(getModels)
      modelsFromResponses ++ modelsFromRequests
    }
    val componentResponses = Option(openAPI.getComponents)
      .flatMap(c => Option(c.getResponses))
      .map(_.asScala.iterator)
      .getOrElse(Iterator.empty)
      .map { case (k, v) => getResponse(Right(k), v) }
      .toList
      .distinct
    val operationResponses = operations.flatMap(_.responses.values)

    (componentModels ++ operationModels, componentResponses ++ operationResponses, operations)
  }

}