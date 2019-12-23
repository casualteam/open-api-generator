package casualteam.openapigenerator

import io.swagger.v3.oas.models.media.{ Content, MediaType }
import scala.jdk.CollectionConverters._

trait MediaTypeModel

object MediaTypeModel {
  case class ApplicationJson(
    model: Model) extends MediaTypeModel
  case class ApplicationXml(
    model: Model) extends MediaTypeModel
  case class ApplicationForm() extends MediaTypeModel
  case class MultipartForm() extends MediaTypeModel

  def getMediaTypeModel(contentType: String, mediaType: MediaType): MediaTypeModel = {
    contentType match {
      case "application/json" =>
        MediaTypeModel.ApplicationJson(
          model = Model.getModel(None, mediaType.getSchema))
      case "application/xml" =>
        MediaTypeModel.ApplicationXml(
          model = Model.getModel(None, mediaType.getSchema))
      case "application/x-www-form-urlencoded" =>
        ApplicationForm()
      case "multipart/form-data" =>
        MultipartForm()
    }
  }

  def getMediaTypeModels(content: Content): Map[String, MediaTypeModel] = {
    content.asScala.iterator
      .map {
        case (key, value) =>
          key -> getMediaTypeModel(key, value)
      }.toMap
  }
}
