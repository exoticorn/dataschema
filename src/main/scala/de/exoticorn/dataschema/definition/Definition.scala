package de.exoticorn.dataschema.definition

sealed abstract class Element extends util.parsing.input.Positional

abstract class TypeDecl(val annotations: Map[String, Any]) extends Element
abstract class PrimitiveType(annotations: Map[String, Any]) extends TypeDecl(annotations)
case class StringType(override val annotations: Map[String, Any]) extends PrimitiveType(annotations)
case class IntType(override val annotations: Map[String, Any]) extends PrimitiveType(annotations)
case class FloatType(override val annotations: Map[String, Any]) extends PrimitiveType(annotations)