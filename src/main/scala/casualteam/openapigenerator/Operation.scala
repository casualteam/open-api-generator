package casualteam.openapigenerator

case class Operation(
  method: String,
  name: String,
  path: List[Either[String, Parameter.Path]],
  queryParameters: List[Parameter.Query],
  headerParameters: List[Parameter.Header],
  requestBody: Option[RequestBody],
  responses: Map[String, Response])