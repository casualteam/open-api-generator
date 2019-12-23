package casualteam.openapigenerator

import io.swagger.v3.oas.models.OpenAPI

import scala.jdk.CollectionConverters._

trait ApiProcess {

  def getOperations(openAPI: OpenAPI) = {
    openAPI.getPaths.asScala.values
      .flatMap { path =>
        Seq(
          Option(path.getDelete).map(Operation.getOperation("DELETE")),
          Option(path.getGet).map(Operation.getOperation("GET")),
          Option(path.getHead).map(Operation.getOperation("HEAD")),
          Option(path.getOptions).map(Operation.getOperation("OPTIONS")),
          Option(path.getPatch).map(Operation.getOperation("PATCH")),
          Option(path.getPost).map(Operation.getOperation("POST")),
          Option(path.getPut).map(Operation.getOperation("PUT")),
          Option(path.getTrace).map(Operation.getOperation("TRACE"))).flatten
      }
      .toList
  }

  def getModels(openAPI: OpenAPI): List[Model] = {
    Option(openAPI.getComponents)
      .flatMap(c => Option(c.getSchemas))
      .map(_.asScala.toList)
      .getOrElse(Nil)
      .map { case (k, v) => Model.getModel(Option(k), v) }.distinct
  }

}

