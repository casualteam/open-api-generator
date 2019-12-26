package casualteam.openapigenerator

case class Operation(
  method: String,
  name: String,
  parameters: List[Parameter],
  requestBody: Option[RequestBody],
  responses: Map[String, Response])