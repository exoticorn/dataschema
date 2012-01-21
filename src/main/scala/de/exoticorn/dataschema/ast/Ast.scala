package de.exoticorn.dataschema.ast

sealed abstract class Element extends util.parsing.input.Positional

sealed abstract class TypeRefBase extends Element
case class TypeRef(name: String, path: Seq[String]) extends TypeRefBase
case class ArrayTypeRef(name: String, path: Seq[String], dimension: Int) extends TypeRefBase

case class AnnotationField(name: String, typeName: Option[String], value: Option[Any]) extends Element
case class StructField(name: String, fieldType: TypeRefBase, annotations: Seq[AnnotationField]) extends Element

abstract class TypeDecl extends Element
case class Struct(name: String, parent: Option[TypeRef], fields: Seq[StructField], annotations: Seq[AnnotationField]) extends TypeDecl
case class Typedef(name: String, baseType: TypeRef, annotations: Seq[AnnotationField]) extends TypeDecl

case class Namespace(name: String, types: Seq[TypeDecl], children: Seq[Namespace]) extends Element
