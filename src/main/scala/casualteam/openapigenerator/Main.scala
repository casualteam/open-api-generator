package casualteam.openapigenerator

import java.io.{ File, PrintWriter }

import io.swagger.v3.oas.models.OpenAPI
import io.swagger.v3.parser.OpenAPIV3Parser

object Main extends App with ApiProcess {
  def inOneLine(s: String) = s.replaceAll("(\n|\r| )+", " ").trim
  def cleanTemplate(s: String) = s.replaceAll("( *(\n|\r))+", "\n").trim
  def firstCapital(s:String) = s.headOption.map(h => h.toUpper + s.tail).getOrElse(s)
  def toComputedName(names: List[String]) = names.flatMap(_.split("/|-|\\+")).map(firstCapital).mkString("")

  def getModelType(model: Model): String = {
    Model.fold(model)(
      m => m.ref.split("/").last,
      m => m.name.fold(toComputedName, identity),
      m => m.name.fold(toComputedName, identity),
      m => m.name.fold(toComputedName, identity),
      m => m.name.fold(_ => "scala.Predef.String", identity),
      m => m.name.fold(_ => "scala.Int", identity),
      m => m.name.fold(_ => "java.time.Instant", identity),
      m => m.name.fold(_ => "scala.Boolean", identity),
      m => m.name.fold(_ => s"scala.List[${getModelType(m.itemModel)}]", identity),
      m => m.name.fold(_ => s"scala.List[scala.Byte]", identity))
  }

  def getResponseType(response: Response): String = {
    Response.fold(response)(
      r => r.name.fold(toComputedName, identity),
      r => r.ref.split("/").last)
  }

  def getRequestBodyType(requestBody: RequestBody): String = {
    requestBody.name.fold(toComputedName, identity)
  }
  def getRequestBodyName(requestBody: RequestBody): String = {
    requestBody.name.fold(toComputedName, identity)
  }

  def getMediaTypeModelType(mediaTypeModel: MediaTypeModel): String = {
    MediaTypeModel.fold(mediaTypeModel)(
      m => getModelType(m.model),
      m => getModelType(m.model),
      m => getModelType(m.model),
      m => getModelType(m.model))
  }

  def getDecoders(op: Operation) = {
    op.requestBody.toList
      .flatMap(requestBody => requestBody.contentTypeModels.iterator.map { case (contentType, mediaTypeModel) => "decode " + contentType + " " + getMediaTypeModelType(mediaTypeModel) })
  }

  val openAPI: OpenAPI = new OpenAPIV3Parser().read("https://petstore.swagger.io/v2/swagger.json")
  val outPath = "target/out.scala"
  val (_models, _responses, _operations) = process(openAPI)
  val writer = new PrintWriter(new File(outPath))

  _models
    .collect {
      case m: Model.Object => models.txt.objectModel(m, getModelType).toString
      case m: Model.TypedMap => models.txt.typedMapModel(m, getModelType).toString
    }
    .map(inOneLine)
    .foreach(s => writer.write(s + "\n"))
  _operations.flatMap(_.requestBody)
    .map(requestBody => cleanTemplate(requests.txt.requestBody(requestBody, getRequestBodyType, getMediaTypeModelType).toString))
    .foreach(s => writer.write(s + "\n"))
  _responses
    .collect {
      case r: Response.BaseResponse => responses.txt.response(r, getResponseType, getMediaTypeModelType).toString
    }
    .map(inOneLine)
    .foreach(s => writer.write(s + "\n"))
  writer.write(cleanTemplate(txt.operations(_operations, getResponseType, getModelType, getRequestBodyName, getRequestBodyType).toString))
  //operations    .flatMap(getDecoders)    .distinct    .map(inOneLine)    .foreach(println)
  writer.close()
}
