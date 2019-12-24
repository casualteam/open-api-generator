package casualteam.openapigenerator

import io.swagger.v3.oas.models.media.{ ArraySchema, BooleanSchema, DateTimeSchema, IntegerSchema, ObjectSchema, Schema, StringSchema }

import scala.jdk.CollectionConverters._

trait Model

object Model {

  case class Ref(
    ref: scala.Predef.String) extends Model

  case class Object(
    name: Option[scala.Predef.String],
    fields: Map[scala.Predef.String, Model]) extends Model

  case class String(
    name: Option[scala.Predef.String]) extends Model

  case class Integer(
    name: Option[scala.Predef.String]) extends Model

  case class DateTime(
    name: Option[scala.Predef.String]) extends Model

  case class Boolean(
    name: Option[scala.Predef.String]) extends Model

  case class Array(
    name: Option[scala.Predef.String],
    itemModel: Model) extends Model

  def getModel(name: Option[scala.Predef.String], schema: Schema[_]): Model = {
    Option(schema.get$ref())
      .map(Model.Ref)
      .getOrElse {
        val modelName = Option(schema.getName).orElse(name)
        schema match {
          case s: ObjectSchema =>
            Object(
              name = modelName,
              fields = Option(s.getProperties).map(_.asScala.view.mapValues(getModel(None, _)).toMap).getOrElse(Map.empty))
          case s: StringSchema =>
            String(
              name = modelName)
          case s: ArraySchema =>
            Array(
              name = modelName,
              itemModel = getModel(None, s.getItems))
          case s: IntegerSchema =>
            Integer(
              name = modelName)
          case s: DateTimeSchema =>
            DateTime(
              name = modelName)
          case s: BooleanSchema =>
            Boolean(
              name = modelName)
        }
      }
  }
}