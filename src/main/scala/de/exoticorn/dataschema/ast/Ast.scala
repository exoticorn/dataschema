package de.exoticorn.dataschema.ast

abstract class Element extends util.parsing.input.Positional

case class Type(name: String, path: Seq[String]) extends Element
case class AnnotationField(name: String, typeName: Option[String], value: Option[Any]) extends Element
case class StructField(name: String, fieldType: Type, annotations: Seq[AnnotationField]) extends Element
case class Struct(name: String, parent: Option[Type], fields: Seq[StructField], annotations: Seq[AnnotationField]) extends Element
