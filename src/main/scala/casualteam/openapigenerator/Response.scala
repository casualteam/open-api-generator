package casualteam.openapigenerator

import io.swagger.v3.oas.models.responses.ApiResponse

trait Response

object Response {

  case class BaseResponse(
    contentTypeModels: Map[String, MediaTypeModel]) extends Response

  case class Ref(
    ref: String) extends Response

  def getResponses(apiResponse: ApiResponse): Response = {
    Option(apiResponse.get$ref)
      .map(Ref)
      .getOrElse {
        val contentTypeModels = MediaTypeModel.getMediaTypeModels(apiResponse.getContent)
        BaseResponse(
          contentTypeModels = contentTypeModels)
      }
  }
}