package casualteam.openapigenerator

trait Model

object Model {

  case class Ref(
    ref: scala.Predef.String) extends Model

  case class Object(
    name: EntityName,
    fields: Map[scala.Predef.String, ObjectField]) extends Model

  case class TypedMap(
    name: EntityName,
    valuesModel: Model) extends Model

  case class FreeMap(
    name: EntityName) extends Model

  case class String(
    name: EntityName) extends Model

  case class Integer(
    name: EntityName) extends Model

  case class DateTime(
    name: EntityName) extends Model

  case class Boolean(
    name: EntityName) extends Model

  case class Array(
    name: EntityName,
    itemModel: Model) extends Model

  case class File(
    name: EntityName) extends Model

  def fold[T](model: Model)(_1: Ref => T, _2: Object => T, _3: TypedMap => T, _4: FreeMap => T, _5: String => T, _6: Integer => T, _7: DateTime => T, _8: Boolean => T, _9: Array => T, _10: File => T): T = {
    model match {
      case m: Ref => _1(m)
      case m: Object => _2(m)
      case m: TypedMap => _3(m)
      case m: FreeMap => _4(m)
      case m: String => _5(m)
      case m: Integer => _6(m)
      case m: DateTime => _7(m)
      case m: Boolean => _8(m)
      case m: Array => _9(m)
      case m: File => _10(m)
    }
  }

}