package casualteam.openapigenerator

import io.swagger.v3.oas.models.OpenAPI
import io.swagger.v3.parser.OpenAPIV3Parser

object Main extends App with ApiProcess {
  def inOneLine(s: String) = s.replaceAll("(\n|\r| )+", " ")
  def getModelType(model: Model): String = {
    model match {
      case m: Model.Ref =>
        m.ref.split("/").last
      case m: Model.Object =>
        m.name.get
      case m: Model.String =>
        m.name.getOrElse("String")
      case m: Model.Integer =>
        m.name.getOrElse("Int")
      case m: Model.DateTime =>
        m.name.getOrElse("java.time.Instant")
      case m: Model.Boolean =>
        m.name.getOrElse("Boolean")
      case m: Model.Array =>
        m.name.getOrElse(s"List[${getModelType(m.itemModel)}]")
    }
  }
  val openAPI: OpenAPI = new OpenAPIV3Parser().read("https://petstore.swagger.io/v2/swagger.json")
  val models = getModels(openAPI)
  models.collect { case objectModel: Model.Object => model.txt.objectModel(objectModel, getModelType).toString }.map(inOneLine).foreach(println)
}
