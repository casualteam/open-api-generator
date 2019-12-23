package casualteam.openapigenerator

import io.swagger.v3.oas.models.media.{ ArraySchema, ObjectSchema, Schema, StringSchema }

import scala.jdk.CollectionConverters._

trait Model

object Model {

  case class Ref(
    ref: scala.Predef.String) extends Model

  case class Object(
    id: scala.Predef.String,
    name: scala.Predef.String,
    fields: Map[scala.Predef.String, Model]) extends Model

  case class String(
    id: scala.Predef.String) extends Model

  case class Array(
    id: scala.Predef.String) extends Model

  def getModel(name: Option[scala.Predef.String], schema: Schema[_]): Model = {
    Option(schema.get$ref())
      .map(Model.Ref)
      .getOrElse {
        val modelName = Option(schema.getName).orElse(name).getOrElse("_empty")
        schema match {
          case s: ObjectSchema =>
            Object(
              id = modelName,
              name = modelName,
              fields = Option(s.getProperties).map(_.asScala.view.mapValues(getModel(None, _)).toMap).getOrElse(Map.empty))
          case s: StringSchema =>
            String(
              id = modelName)
          case s: ArraySchema =>
            Array(
              id = modelName)
          case s =>
            Ref(
              ref = modelName)
        }
      }
  }
}