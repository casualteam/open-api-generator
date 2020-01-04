package casualteam.openapigenerator

import java.io.{ File, PrintWriter }

import io.swagger.v3.oas.models.OpenAPI
import io.swagger.v3.parser.OpenAPIV3Parser
import io.swagger.v3.parser.core.models.ParseOptions

import scala.jdk.CollectionConverters._

object Main extends App with ApiProcess {
  case class A(a: String)(b: Int)

  def firstCharUpper(s: String) = s.headOption.map(h => h.toUpper + s.tail).getOrElse(s)
  def firstCharLower(s: String) = s.headOption.map(h => h.toLower + s.tail).getOrElse(s)
  def toComputedType(names: List[String]) = firstCharUpper(names.flatMap(_.split("/|-|\\.|\\+")).map(firstCharUpper).mkString(""))
  def toComputedName(names: List[String]) = firstCharLower(names.flatMap(_.split("/|-|\\.|\\+")).map(firstCharUpper).mkString(""))

  def withOption(string: String, required: Boolean) = {
    if (required) string
    else s"Option[$string]"
  }

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

  def generateCode(apiPath: String, out: File): Unit = {
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
    val writer = new PrintWriter(out)
    def writeln(s: Any) = writer.write(s.toString + "\n")

    writeln("//models")
    _models
      .collect {
        case m: Model.Object => models.txt.objectModel(m, getModelType)
        case m: Model.TypedMap => models.txt.typedMapModel(m, getModelType(_, true))
      }
      .map(inOneLine)
      .foreach(writeln)
    writeln("//request bodies")
    _requestBodies
      .map(requestBody => requests.txt.requestBody(requestBody, getRequestBodyType, getMediaTypeModelType))
      .foreach(s => writeln(cleanTemplate(s)))
    writeln("//responses")
    _responses
      .collect {
        case r: Response.BaseResponse => responses.txt.response(r, getResponseType, getMediaTypeModelType, getHeaderType)
      }
      .map(inOneLine)
      .foreach(writeln)
    writeln("//operations")
    writeln(cleanTemplate(txt.operations(_operations, getOperationName, getResponseType, getModelType, getRequestBodyName, getRequestBodyTypeParam(_requestBodies))))
    writer.close()
  }

  generateCode("src/main/resources/example-api-v3.yaml", new File("target/out2.scala"))
  generateCode("https://petstore.swagger.io/v2/swagger.json", new File("target/out1.scala"))
}
