package casualteam.openapigenerator

trait MediaTypeModel

object MediaTypeModel {
  case class ApplicationJson(model: Model) extends MediaTypeModel
  case class ApplicationXml(model: Model) extends MediaTypeModel
  case class ApplicationForm(model: Model) extends MediaTypeModel
  case class MultipartForm(model: Model) extends MediaTypeModel

  def fold[T](mediaTypeModel: MediaTypeModel)(_1: ApplicationJson => T, _2: ApplicationXml => T, _3: ApplicationForm => T, _4: MultipartForm => T): T = {
    mediaTypeModel match {
      case m: ApplicationJson => _1(m)
      case m: ApplicationXml => _2(m)
      case m: ApplicationForm => _3(m)
      case m: MultipartForm => _4(m)
    }
  }
}
