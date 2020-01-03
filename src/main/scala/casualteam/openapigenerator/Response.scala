package casualteam.openapigenerator

trait Response

object Response {

  case class BaseResponse(
    name: EntityName,
    headers: Map[String, Header],
    contentTypeModels: Map[String, MediaTypeModel]) extends Response

  case class Ref(
    ref: String) extends Response

  def fold[T](response: Response)(_1: BaseResponse => T, _2: Ref => T): T = {
    response match {
      case r: BaseResponse => _1(r)
      case r: Ref => _2(r)
    }
  }
}