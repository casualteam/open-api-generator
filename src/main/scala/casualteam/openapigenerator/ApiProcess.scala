package casualteam.openapigenerator

import casualteam.openapigenerator.MediaTypeModel.{ ApplicationForm, MultipartForm, OctetStream }
import casualteam.openapigenerator.Response.BaseResponse
import io.swagger.v3.oas.models.headers.{ Header => OpenApiHeader }
import io.swagger.v3.oas.models.media._
import io.swagger.v3.oas.models.parameters.{ Parameter => OpenApiParameter, RequestBody => OpenApiRequestBody }
import io.swagger.v3.oas.models.responses.ApiResponse
import io.swagger.v3.oas.models.{ OpenAPI, Operation => OpenApiOperation }

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
    val model = getModel(List(parameterName), None, actualParameter.getSchema)
    actualParameter.getIn match {
      case "query" =>
        Parameter.Query(parameter.getName, model)
      case "path" =>
        Parameter.Path(parameter.getName, model)
      case "header" =>
        Parameter.Header(parameter.getName, model)
    }
  }

  def getMediaTypeModels(computedName: List[String], content: Content): Map[String, MediaTypeModel] = {
    content.asScala.iterator
      .map {
        case (contentType, mediaType) =>
          val newComputeName = computedName :+ contentType
          val mediaTypeModel = contentType match {
            case "application/json" =>
              MediaTypeModel.ApplicationJson(model = getModel(newComputeName, None, mediaType.getSchema))
            case "application/xml" =>
              MediaTypeModel.ApplicationXml(model = getModel(newComputeName, None, mediaType.getSchema))
            case "application/x-www-form-urlencoded" =>
              ApplicationForm(model = getModel(newComputeName, None, mediaType.getSchema))
            case "multipart/form-data" =>
              MultipartForm(model = getModel(newComputeName, None, mediaType.getSchema))
            case "application/octet-stream" =>
              OctetStream(model = getModel(newComputeName, None, mediaType.getSchema))
            case _ =>
              throw new IllegalArgumentException(s"Unknown content type $contentType in content $content")
          }
          contentType -> mediaTypeModel
      }.toMap
  }

  def getRequestBody(computedName: List[String], name: Option[String], requestBody: OpenApiRequestBody): RequestBody = {
    Option(requestBody.get$ref())
      .map(RequestBody.Ref)
      .getOrElse {
        val contentTypeModels = Option(requestBody.getContent).map(getMediaTypeModels(computedName, _)).getOrElse(Map.empty)
        val basicName = name.map(Right(_)).getOrElse(Left(computedName))
        RequestBody.Basic(
          name = basicName,
          required = requestBody.getRequired,
          contentTypeModels = contentTypeModels)
      }
  }

  def getHeader(componentHeaders: Map[String, OpenApiHeader], computedName: List[String], header: OpenApiHeader): Header = {
    val actualHeader = Option(header.get$ref)
      .map { ref =>
        val name = ref.split("/").last
        componentHeaders.getOrElse(name, throw new Exception(s"Header $name not found"))
      }
      .getOrElse(header)
    Header(
      model = getModel(computedName :+ "header", None, actualHeader.getSchema))
  }

  def getResponse(componentHeaders: Map[String, OpenApiHeader], computedName: List[String], name: Option[String], apiResponse: ApiResponse): Response = {
    Option(apiResponse.get$ref)
      .map(Response.Ref)
      .getOrElse {
        val contentTypeModels = Option(apiResponse.getContent).map(getMediaTypeModels(computedName, _)).getOrElse(Map.empty)
        val headers = Option(apiResponse.getHeaders).map(_.asScala)
          .getOrElse(Nil)
          .map { case (k, v) => k -> getHeader(componentHeaders, computedName :+ k, v) }
          .toMap
        Response.BaseResponse(
          name = name.map(Right(_)).getOrElse(Left(computedName)),
          headers = headers,
          contentTypeModels = contentTypeModels)
      }
  }

  def getOperation(method: String, componentParamenters: Map[String, OpenApiParameter], componentHeaders: Map[String, OpenApiHeader], operation: OpenApiOperation): Operation = {
    val computedName = List(operation.getOperationId)
    val requestBody = Option(operation.getRequestBody).map(getRequestBody(computedName, None, _))
    val parameters = Option(operation.getParameters).map(_.asScala).getOrElse(Nil).map(getParameter(componentParamenters, _)).toList
    val responses = operation.getResponses.asScala.iterator
      .map { case (k, v) => k -> getResponse(componentHeaders, computedName :+ k, None, v) }
      .toMap
    Operation(
      method = method,
      parameters = parameters,
      name = operation.getOperationId,
      requestBody = requestBody,
      responses = responses)
  }

  def getModel(computedName: List[String], name: Option[String], schema: Schema[_]): Model = {
    Option(schema.get$ref())
      .map(Model.Ref)
      .getOrElse {
        val modelName = name.map(Right(_)).getOrElse(Left(computedName))
        schema match {
          case s: StringSchema =>
            Model.String(
              name = modelName)
          case s: ArraySchema =>
            Model.Array(
              name = modelName,
              itemModel = getModel(computedName :+ "Item", None, s.getItems))
          case s: IntegerSchema =>
            Model.Integer(
              name = modelName)
          case s: DateTimeSchema =>
            Model.DateTime(
              name = modelName)
          case s: BooleanSchema =>
            Model.Boolean(
              name = modelName)
          case s: FileSchema if s.getFormat == "binary" =>
            Model.File(
              name = modelName)
          case s: BinarySchema =>
            Model.File(
              name = modelName)
          case s: MapSchema =>
            s.getAdditionalProperties match {
              case true =>
                Model.FreeMap(
                  name = modelName)
              case getAdditionalPropertiesSchema: Schema[_] =>
                val model = getModel(computedName :+ "Values", None, getAdditionalPropertiesSchema)
                Model.TypedMap(
                  name = modelName,
                  valuesModel = model)
            }
          case s =>
            val fields = Option(s.getProperties).map(_.asScala).getOrElse(Nil)
              .map { case (k, v) => k -> getModel(computedName :+ k, None, v) }
              .toMap
            Model.Object(
              name = modelName,
              fields = fields)
        }
      }
  }

  def process(openAPI: OpenAPI): (List[Model], List[Response], List[RequestBody.Basic], List[Operation]) = {
    def getModels(mediaTypeModel: MediaTypeModel) = {
      MediaTypeModel.fold(mediaTypeModel)(
        m => Some(m.model),
        m => Some(m.model),
        m => Some(m.model),
        m => Some(m.model),
        m => Some(m.model))
    }

    def expandModel(model: Model): List[Model] = {
      val additionalModels = Model.fold(model)(
        m => Nil,
        m => m.fields.values.flatMap(expandModel).toList,
        m => expandModel(m.valuesModel),
        m => Nil,
        m => Nil,
        m => Nil,
        m => Nil,
        m => Nil,
        m => Nil,
        m => Nil)
      model :: additionalModels
    }

    //operations
    val componentParameters = Option(openAPI.getComponents).flatMap(c => Option(c.getParameters)).map(_.asScala.toMap).getOrElse(Map.empty)
    val componentHeaders = Option(openAPI.getComponents).flatMap(c => Option(c.getHeaders)).map(_.asScala.toMap).getOrElse(Map.empty)
    val operations = openAPI.getPaths.asScala.values
      .flatMap { path =>
        Seq(
          Option(path.getDelete).map(getOperation("DELETE", componentParameters, componentHeaders, _)),
          Option(path.getGet).map(getOperation("GET", componentParameters, componentHeaders, _)),
          Option(path.getHead).map(getOperation("HEAD", componentParameters, componentHeaders, _)),
          Option(path.getOptions).map(getOperation("OPTIONS", componentParameters, componentHeaders, _)),
          Option(path.getPatch).map(getOperation("PATCH", componentParameters, componentHeaders, _)),
          Option(path.getPost).map(getOperation("POST", componentParameters, componentHeaders, _)),
          Option(path.getPut).map(getOperation("PUT", componentParameters, componentHeaders, _)),
          Option(path.getTrace).map(getOperation("TRACE", componentParameters, componentHeaders, _))).flatten
      }
      .toList

    //models
    val componentModels = Option(openAPI.getComponents)
      .flatMap(c => Option(c.getSchemas))
      .map(_.asScala.iterator)
      .getOrElse(Iterator.empty)
      .map { case (k, v) => getModel(Nil, Some(k), v) }
      .toList
      .distinct
    val operationModels = operations.flatMap { op =>
      val modelsFromResponses = op.responses.values
        .collect { case m: BaseResponse => m.contentTypeModels.values.flatMap(getModels) ++ m.headers.values.map(_.model) }
        .flatten
      val modelsFromRequests = op.requestBody
        .map(RequestBody.fold(_)(basic => basic.contentTypeModels.values, ref => Nil))
        .toList.flatten
        .flatMap(getModels)
      modelsFromResponses ++ modelsFromRequests
    }

    //responses
    val componentResponses = Option(openAPI.getComponents)
      .flatMap(c => Option(c.getResponses))
      .map(_.asScala.iterator)
      .getOrElse(Iterator.empty)
      .map { case (k, v) => getResponse(componentHeaders, Nil, Some(k), v) }
      .toList
      .distinct
    val operationResponses = operations.flatMap(_.responses.values)

    //requestBody
    val componentRequestBodies = Option(openAPI.getComponents)
      .flatMap(c => Option(c.getRequestBodies))
      .map(_.asScala.iterator)
      .getOrElse(Iterator.empty)
      .map { case (k, v) => getRequestBody(Nil, Some(k), v) }
      .toList
      .distinct
    val operationRequestBodies = operations.flatMap(_.requestBody)

    val allModels = (componentModels ++ operationModels).flatMap(expandModel).distinct
    val allResponses = (componentResponses ++ operationResponses).distinct
    val allRequestBodies = (componentRequestBodies ++ operationRequestBodies).distinct
      .collect { case r: RequestBody.Basic => r }
    (allModels, allResponses, allRequestBodies, operations)
  }

}