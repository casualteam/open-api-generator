package casualteam.openapigenerator

trait RequestBody

object RequestBody {

  case class Basic(
    name: EntityName,
    required: Boolean,
    contentTypeModels: Map[String, MediaTypeModel]) extends RequestBody

  case class Ref(ref: String) extends RequestBody

  def fold[T](requestBody: RequestBody)(_1: Basic => T, _2: Ref => T): T = {
    requestBody match {
      case r: Basic => _1(r)
      case r: Ref => _2(r)
    }
  }
}

