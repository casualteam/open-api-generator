package casualteam.openapigenerator

case class RequestBody(
  name: EntityName,
  required: Boolean,
  contentTypeModels: Map[String, MediaTypeModel])