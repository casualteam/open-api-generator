package casualteam.openapigenerator

import io.swagger.v3.oas.models.parameters.{ Parameter => OpenApiParameter }

trait Parameter

object Parameter {
  case class Query(param: String) extends Parameter
  case class Path(param: String) extends Parameter
  case class Header(param: String) extends Parameter
  case class Ref(ref: String) extends Parameter

  def getParameter(parameter: OpenApiParameter): Parameter = {
    Option(parameter.get$ref).map(Ref)
      .getOrElse {
        parameter.getIn match {
          case "query" => Parameter.Query(parameter.getName)
          case "path" => Parameter.Path(parameter.getName)
          case "header" => Parameter.Header(parameter.getName)
        }
      }
  }
}