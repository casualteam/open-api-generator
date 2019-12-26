package casualteam.openapigenerator

trait Model

object Model {

  case class Ref(
    ref: scala.Predef.String) extends Model

  case class Object(
    name: EntityName,
    fields: Map[scala.Predef.String, Model]) extends Model

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

}