package casualteam.openapigenerator

import io.swagger.v3.oas.models.OpenAPI
import io.swagger.v3.parser.OpenAPIV3Parser

object Main extends App with ApiProcess {
  val openAPI: OpenAPI = new OpenAPIV3Parser().read("https://petstore.swagger.io/v2/swagger.json")
  val models = getModels(openAPI)
  models.collect { case objectModel: Model.Object => model.txt.objectModel(objectModel) }.foreach(println)
}
