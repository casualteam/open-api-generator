package casualteam.openapigenerator

import java.io.{ File, PrintWriter }

import io.swagger.v3.oas.models.OpenAPI
import io.swagger.v3.parser.OpenAPIV3Parser

object Main extends App with ApiProcess {
  def inOneLine(s: String) = s.replaceAll("(\n|\r| )+", " ").trim
  def cleanTemplate(s: String) = s.replaceAll("( *(\n|\r))+", "\n").trim
  def toComputedName(names: List[String]) = names.map(_.replaceAll("/|-|\\+", "")).mkString("")

  def getModelType(model: Model): String = {
    model match {
      case m: Model.Ref =>
        m.ref.split("/").last
      case m: Model.Object =>
        m.name.fold(toComputedName, identity)
      case m: Model.TypedMap =>
        m.name.fold(toComputedName, identity)
      case m: Model.String =>
        m.name.fold(_ => "String", identity)
      case m: Model.Integer =>
        m.name.fold(_ => "Int", identity)
      case m: Model.DateTime =>
        m.name.fold(_ => "java.time.Instant", identity)
      case m: Model.Boolean =>
        m.name.fold(_ => "Boolean", identity)
      case m: Model.Array =>
        m.name.fold(_ => s"List[${getModelType(m.itemModel)}]", identity)
    }
  }

  def getResponseType(response: Response): String = {
    response match {
      case r: Response.Ref =>
        r.ref.split("/").last
      case r: Response.BaseResponse =>
        r.name.fold(toComputedName, identity)
    }
  }

  def getRequestBodyName(requestBody: RequestBody): String = {
    requestBody.name.fold(toComputedName, identity)
  }

  def getRequestBodyType(requestBody: RequestBody): String = {
    requestBody.name.fold(toComputedName, identity)
  }

  def getMediaTypeModelType(mediaTypeModel: MediaTypeModel): String = {
    MediaTypeModel.fold(mediaTypeModel)(
      m => getModelType(m.model),
      m => getModelType(m.model),
      _ => "Form",
      _ => "Multipart")
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
  _responses
    .collect {
      case r: Response.BaseResponse => responses.txt.responseModel(r, getResponseType, getMediaTypeModelType).toString()
    }
    .map(inOneLine)
    .foreach(s => writer.write(s + "\n"))
  writer.write(cleanTemplate(txt.operations(_operations, getResponseType, getModelType, getRequestBodyName, getRequestBodyType).toString))
  //operations    .flatMap(getDecoders)    .distinct    .map(inOneLine)    .foreach(println)
  writer.close()
}
