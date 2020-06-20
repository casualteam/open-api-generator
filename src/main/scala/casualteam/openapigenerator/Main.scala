package casualteam.openapigenerator

import better.files._
import io.swagger.v3.oas.models.OpenAPI
import io.swagger.v3.parser.OpenAPIV3Parser
import io.swagger.v3.parser.core.models.ParseOptions

import scala.jdk.CollectionConverters._

object Main extends App with ApiProcess {
  def firstCharUpper(s: String) = s.headOption.map(h => h.toUpper + s.tail).getOrElse(s)
  def firstCharLower(s: String) = s.headOption.map(h => h.toLower + s.tail).getOrElse(s)
  def toComputedType(names: List[String]) = firstCharUpper(names.flatMap(_.split("/|-|\\.|\\+")).map(firstCharUpper).mkString(""))
  def toComputedName(names: List[String]) = firstCharLower(names.flatMap(_.split("/|-|\\.|\\+")).map(firstCharUpper).mkString(""))

  def withOption(string: String, required: Boolean) = if (required) string else s"Option[$string]"

  def getModelType(model: Model, required: Boolean): String = {
    val modelType = Model.fold(model)(
      m => m.ref.split("/").last,
      m => m.name.fold(toComputedType, identity),
      m => m.name.fold(toComputedType, identity),
      m => m.name.fold(toComputedType, identity),
      m => m.name.fold(_ => "Predef.String", identity),
      m => m.name.fold(_ => "Int", identity),
      m => m.name.fold(_ => "java.time.Instant", identity),
      m => m.name.fold(_ => "Boolean", identity),
      m => m.name.fold(_ => s"List[${getModelType(m.itemModel, true)}]", identity),
      m => m.name.fold(_ => s"List[scala.Byte]", identity))
    withOption(modelType, required)
  }

  def getPathParamMatcher(pathParam: Parameter): String = {
    Model.fold(pathParam.model)(
      m => ???,
      m => ???,
      m => ???,
      m => ???,
      m => m.name.fold(_ => "Segment", identity),
      m => m.name.fold(_ => "IntNumber", identity),
      m => ???,
      m => ???,
      m => ???,
      m => ???)
  }

  def getResponseType(response: Response): String = {
    Response.fold(response)(
      r => r.name.fold(toComputedType, identity),
      r => r.ref.split("/").last)
  }

  def getActualResponse(responses: List[Response])(response: Response): Response.BaseResponse = {
    Response.fold(response)(
      base => base,
      ref => {
        responses.collectFirst {
          case basic: Response.BaseResponse if basic.name.fold(identity, identity) == ref.ref.split("/").last => basic
        }.get
      })
  }

  def getActualRequestBody(requestBodies: List[RequestBody])(requestBody: RequestBody): RequestBody.Basic = {
    RequestBody.fold(requestBody)(
      base => base,
      ref => {
        requestBodies.collectFirst {
          case basic: RequestBody.Basic if basic.name.fold(identity, identity) == ref.ref.split("/").last => basic
        }.get
      })
  }

  def getRequestBodyTypeParam(requestBodies: List[RequestBody])(requestBody: RequestBody): String = {
    val actualBodyType = RequestBody.fold(requestBody)(
      basic => basic,
      ref => {
        requestBodies.collectFirst {
          case basic: RequestBody.Basic if basic.name.fold(identity, identity) == ref.ref.split("/").last => basic
        }.get
      })
    withOption(getRequestBodyType(actualBodyType), actualBodyType.required)
  }

  def getRequestBodyType(requestBody: RequestBody): String = {
    RequestBody.fold(requestBody)(
      basic => basic.name.fold(toComputedType, identity),
      ref => ref.ref.split("/").last)
  }

  def getRequestBodyName(requestBody: RequestBody): String = {
    RequestBody.fold(requestBody)(
      basic => basic.name.fold(toComputedName, identity),
      ref => firstCharLower(ref.ref.split("/").last))
  }

  def getMediaTypeModelType(mediaTypeModel: MediaTypeModel): String = {
    MediaTypeModel.fold(mediaTypeModel)(
      m => getModelType(m.model, true),
      m => getModelType(m.model, true),
      m => getModelType(m.model, true),
      m => getModelType(m.model, true),
      m => getModelType(m.model, true))
  }

  def getMediaTypeModelEncoder(mediaTypeModel: MediaTypeModel): String = {
    MediaTypeModel.fold(mediaTypeModel)(
      _ => "encodeJson",
      _ => "encodeXml",
      _ => ???,
      _ => ???,
      _ => ???)
  }

  def getMediaTypeModelDecoder(mediaTypeModel: MediaTypeModel, required: Boolean): String = {
    MediaTypeModel.fold(mediaTypeModel)(
      m => s"decodeJson[${getModelType(m.model, required)}](requestTimeout,requestMaxBytes,defaultCharset)",
      m => s"decodeXml[${getModelType(m.model, required)}]",
      m => s"decodeXml[${getModelType(m.model, required)}]",
      m => s"decodeXml[${getModelType(m.model, required)}]",
      m => s"decodeXml[${getModelType(m.model, required)}]")
  }

  def getHeaderType(header: Header): String =
    getModelType(header.model, header.required)

  def getOperationName(op: Operation): String =
    toComputedName(List(op.name))

  def getValidationCode(model: Model) = {
    val validations = casualteam.openapigenerator.Model.fold(model)(
      ref => Nil,
      _object => Nil,
      typedMap => Nil,
      freeMap => Nil,
      string => {
        Seq(
          string.minLength.map(minLength => s"Validation.minLengthString($minLength)"),
          string.maxLength.map(maxLength => s"Validation.maxLengthString($maxLength)")).flatten
      },
      integer => {
        Seq(
          integer.min.map(min => s"Validation.min[${getModelType(model, true)}]($min)"),
          integer.max.map(max => s"Validation.max[${getModelType(model, true)}]($max)")).flatten
      },
      dateTime => Nil,
      boolean => Nil,
      array => {
        Seq(
          array.minLength.map(minLength => s"Validation.minLength[${getModelType(model, true)}]($minLength)"),
          array.maxLength.map(maxLength => s"Validation.maxLength[${getModelType(model, true)}]($maxLength)")).flatten
      },
      file => Nil)
    if (validations.nonEmpty) validations.mkString(" _ combine ")
    else ""
  }

  def generateCode(apiPath: String, directory: File): Unit = {
    def inOneLine(s: Any) =
      s.toString.replaceAll("(\n|\r| )+", " ").trim
    def cleanTemplate(s: Any) =
      s.toString.replaceAll("(^(\n|\r) *)+|( *(\n|\r))+", "\n").trim

    val parseOptions = {
      val p = new ParseOptions()
      p.setResolve(false)
      p.setResolveFully(false)
      p
    }
    val openAPI: OpenAPI = new OpenAPIV3Parser().read(apiPath, Nil.asJava, parseOptions)
    val (_models, _responses, _requestBodies, _operations) = process(openAPI)
    val errorTypeFile = (directory / "ErrorType.scala").clear()
    val locationFile = (directory / "Location.scala").clear()
    val fieldErrorFile = (directory / "FieldError.scala").clear()
    val operationsFile = (directory / "Operations.scala").clear()
    val operationsHandlerFile = (directory / "OperationsHandler.scala").clear()
    val modelsFile = (directory / "Models.scala").clear()
    val requestsFile = (directory / "Requests.scala").clear()
    val responsesFile = (directory / "Responses.scala").clear()
    val parameterHandlersFile = (directory / "ParameterDecoder.scala").clear()
    val jsonHandlersFile = (directory / "JsonHandlers.scala").clear()
    val xmlHandlersFile = (directory / "XmlHandlers.scala").clear()
    val validationFile = (directory / "Validation.scala").clear()

    //models
    _models
      .collect {
        case m: Model.Object => models.txt.objectModel(m, getModelType)
        case m: Model.TypedMap => models.txt.typedMapModel(m, getModelType(_, true))
      }
      .map(inOneLine)
      .foreach(modelsFile.appendLine)
    //request bodies
    _requestBodies
      .map(requestBody => requests.txt.requestBody(requestBody, getRequestBodyType, getMediaTypeModelType))
      .foreach(s => requestsFile.appendLine(cleanTemplate(s)))
    //responses
    _responses
      .collect {
        case r: Response.BaseResponse => responses.txt.response(r, getResponseType, getMediaTypeModelType, getHeaderType)
      }
      .map(inOneLine)
      .foreach(responsesFile.appendLine)
    //handlers
    validationFile.appendLine(cleanTemplate(txt.validation()))
    errorTypeFile.appendLine(cleanTemplate(txt.errorType()))
    fieldErrorFile.appendLine(cleanTemplate(txt.fieldError()))
    locationFile.appendLine(cleanTemplate(txt.location()))
    parameterHandlersFile.appendLine(cleanTemplate(handlers.txt.parameter()))
    jsonHandlersFile.appendLine(cleanTemplate(handlers.json.txt.handler(_models, getModelType, getValidationCode)))
    xmlHandlersFile.appendLine(cleanTemplate(handlers.xml.txt.handler(_models, getModelType)))
    //operations
    operationsFile.appendLine(cleanTemplate(operation.txt.interface(_operations, getOperationName, getResponseType, getModelType, getRequestBodyName, getRequestBodyTypeParam(_requestBodies))))
    //operations handler
    operationsHandlerFile.appendLine(
      cleanTemplate(txt.operationsHandler(
        _operations,
        getOperationName,
        getActualResponse(_responses),
        getModelType,
        getRequestBodyName,
        getRequestBodyType,
        getMediaTypeModelEncoder,
        getMediaTypeModelDecoder,
        getActualRequestBody(_requestBodies),
        getValidationCode)))
  }

  def generate(config: Config): Unit = {
    (config.directory / "project" / "build.properties")
      .createFileIfNotExists(true)
      .write(sbt.txt.buildProperties.apply().toString)
    (config.directory / "build.sbt")
      .createFileIfNotExists(true)
      .write(sbt.txt.buildSbt(config.scalaVersion, config.version, config.organization, config.organizationName, config.name).toString)
    val codeFile = (config.directory / "src" / "main" / "scala")
      .createDirectoryIfNotExists(true)
    generateCode(config.apiPath, codeFile)
  }

  case class Config(
    apiPath: String,
    scalaVersion: String,
    version: String,
    organization: String,
    organizationName: String,
    name: String,
    directory: File)

  val api1 = Config(
    apiPath = "src/main/resources/example-api-v3.yaml",
    scalaVersion = "2.13.1",
    version = "0.1.0",
    organization = "test",
    organizationName = "test",
    name = "test",
    directory = "target/out1".toFile)

  generate(api1)
}