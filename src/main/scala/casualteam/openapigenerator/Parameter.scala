package casualteam.openapigenerator

trait Parameter

object Parameter {
  case class Query(param: String) extends Parameter
  case class Path(param: String) extends Parameter
  case class Header(param: String) extends Parameter
  case class Ref(ref: String) extends Parameter
}