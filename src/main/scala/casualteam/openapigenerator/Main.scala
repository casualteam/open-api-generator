package casualteam.openapigenerator

import java.io.{ File, PrintWriter }

import io.swagger.v3.oas.models.OpenAPI
import io.swagger.v3.parser.OpenAPIV3Parser

object Main extends App with ApiProcess {
  def inOneLine(s: String) = s.replaceAll("(\n|\r| )+", " ").trim
  def cleanTemplate(s: String) = s.replaceAll("( *(\n|\r))+", "\n").trim
  def firstCharCapital(s: String) = s.headOption.map(h => h.toUpper + s.tail).getOrElse(s)
  def toComputedType(names: List[String]) = names.flatMap(_.split("/|-|\\+")).map(firstCharCapital).mkString("")
  def toComputedName(names: List[String]) = names.flatMap(_.split("/|-|\\+")).mkString("")

  def getModelType(model: Model): String = {
    Model.fold(model)(
      m => m.ref.split("/").last,
      m => m.name.fold(toComputedType, identity),
      m => m.name.fold(toComputedType, identity),
      m => m.name.fold(toComputedType, identity),
      m => m.name.fold(_ => "scala.Predef.String", identity),
      m => m.name.fold(_ => "scala.Int", identity),
      m => m.name.fold(_ => "java.time.Instant", identity),
      m => m.name.fold(_ => "scala.Boolean", identity),
      m => m.name.fold(_ => s"scala.List[${getModelType(m.itemModel)}]", identity),
      m => m.name.fold(_ => s"scala.List[scala.Byte]", identity))
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
  def getRequestBodyName(requestBody: RequestBody): String = {
    RequestBody.fold(requestBody)(
      basic => basic.name.fold(toComputedName, identity),
      ref => ref.ref.split("/").last)
  }

  def getMediaTypeModelType(mediaTypeModel: MediaTypeModel): String = {
    MediaTypeModel.fold(mediaTypeModel)(
      m => getModelType(m.model),
      m => getModelType(m.model),
      m => getModelType(m.model),
      m => getModelType(m.model),
      m => getModelType(m.model))
  }

  def generateCode(apiPath: String, out: File): Unit = {
    val openAPI: OpenAPI = new OpenAPIV3Parser().read(apiPath)
    val (_models, _responses, _requestBodies, _operations) = process(openAPI)
    val writer = new PrintWriter(out)

    _models
      .collect {
        case m: Model.Object => models.txt.objectModel(m, getModelType).toString
        case m: Model.TypedMap => models.txt.typedMapModel(m, getModelType).toString
      }
      .map(inOneLine)
      .foreach(s => writer.write(s + "\n"))
    _requestBodies
      .map(requestBody => requests.txt.requestBody(requestBody, getRequestBodyType, getMediaTypeModelType))
      .foreach(s => writer.write(cleanTemplate(s.toString) + "\n"))
    _responses
      .collect {
        case r: Response.BaseResponse => responses.txt.response(r, getResponseType, getMediaTypeModelType).toString
      }
      .map(inOneLine)
      .foreach(s => writer.write(s + "\n"))
    writer.write(cleanTemplate(txt.operations(_operations, getResponseType, getModelType, getRequestBodyName, getRequestBodyType).toString))
    writer.close()
  }

  generateCode("src/main/resources/example-api-v3.yaml", new File("target/out2.scala"))
  generateCode("https://petstore.swagger.io/v2/swagger.json", new File("target/out1.scala"))
}
