package casualteam.openapigenerator

case class Operation(
  method: String,
  name: String,
  path: List[Either[String, Parameter]],
  queryParameters: List[Parameter],
  headerParameters: List[Parameter],
  requestBody: Option[RequestBody],
  responses: Map[String, Response],
  hasInput: Boolean)