package casualteam.openapigenerator

trait Parameter {
  def name: String
  def model: Model
}

object Parameter {
  case class Query(name: String, model: Model) extends Parameter
  case class Path(name: String, model: Model) extends Parameter
  case class Header(name: String, model: Model) extends Parameter
}