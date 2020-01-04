package casualteam.openapigenerator

trait Parameter {
  def name: String
  def model: Model
  def required: Boolean
}

object Parameter {
  case class Query(name: String, model: Model, required: Boolean) extends Parameter
  case class Path(name: String, model: Model, required: Boolean) extends Parameter
  case class Header(name: String, model: Model, required: Boolean) extends Parameter
}