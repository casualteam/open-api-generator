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

  def getPathParamMatcher(pathParam: Parameter.Path): String = {
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

  def getRequestBodyType(requestBody: RequestBody): String = {
    RequestBody.fold(requestBody)(
      basic => basic.name.fold(toComputedType, identity),
      ref => ref.ref.split("/").last)
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

  def getHeaderType(header: Header): String =
    getModelType(header.model, header.required)

  def getOperationName(op: Operation): String =
    toComputedName(List(op.name))

  def generateCode(apiPath: String, directory: File): Unit = {
    def inOneLine(s: Any) = s.toString.replaceAll("(\n|\r| )+", " ").trim
    def cleanTemplate(s: Any) = s.toString.replaceAll("( *(\n|\r))+", "\n").trim

    val parseOptions = {
      val p = new ParseOptions()
      p.setResolve(false)
      p.setResolveFully(false)
      p
    }
    val openAPI: OpenAPI = new OpenAPIV3Parser().read(apiPath, Nil.asJava, parseOptions)
    val (_models, _responses, _requestBodies, _operations) = process(openAPI)
    val codeFile = (directory / "stuff.scala").clear()

    codeFile.appendLine("//models")
    _models
      .collect {
        case m: Model.Object => models.txt.objectModel(m, getModelType)
        case m: Model.TypedMap => models.txt.typedMapModel(m, getModelType(_, true))
      }
      .map(inOneLine)
      .foreach(codeFile.appendLine)
    codeFile.appendLine("//request bodies")
    _requestBodies
      .map(requestBody => requests.txt.requestBody(requestBody, getRequestBodyType, getMediaTypeModelType))
      .foreach(s => codeFile.appendLine(cleanTemplate(s)))
    codeFile.appendLine("//responses")
    _responses
      .collect {
        case r: Response.BaseResponse => responses.txt.response(r, getResponseType, getMediaTypeModelType, getHeaderType)
      }
      .map(inOneLine)
      .foreach(codeFile.appendLine)
    codeFile.appendLine("//operations")
    codeFile.appendLine(cleanTemplate(operation.txt.interface(_operations, getOperationName, getResponseType, getModelType, getRequestBodyName, getRequestBodyTypeParam(_requestBodies))))
    codeFile.appendLine("//operation impl")
    codeFile.appendLine(cleanTemplate(txt.requestHandler(_operations, getOperationName, getResponseType, getModelType, getRequestBodyName, getRequestBodyTypeParam(_requestBodies))))
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
    directory = "target/out2".toFile)
  val api2 = api1.copy(
    apiPath = "https://petstore.swagger.io/v2/swagger.json",
    directory = "target/out1".toFile)

  generate(api1)
  generate(api2)
}