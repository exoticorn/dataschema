package de.exoticorn.dataschema

import scala.util.parsing.combinator._
import de.exoticorn.dataschema.ast._

object Parser extends RegexParsers {
  // format: OFF
  
  def ident = """[a-zA-Z_]\w*""".r

  def typeRef = namespacePath ~ ident                      ^^ { case path ~ name => TypeRef(name, path) }

  def namespacePath = rep(ident <~ "::")
  
  def annotationType = "string" | "int" | "float"
  
  def annotationField = opt(annotationType) ~ ident ~ opt("=" ~> literal)    ^^
                                                              { case t ~ n ~ v => AnnotationField(n, t, v) }
  
  def literal = stringLiteral | floatLiteral | intLiteral  
  def stringLiteral = """"[^"]*"""".r                      ^^ { _.tail.dropRight(1) }
  def intLiteral = """-?\d+""".r                           ^^ { _.toInt }
  def floatLiteral = """-?\d+\.\d+""".r                    ^^ { _.toFloat }
  
  def annotation = opt("<<" ~> rep(annotationField) <~ ">>") ^^
                                                              { _.getOrElse(Seq.empty) }
  
  def structField = (
    typeRef ~ ident ~ ("[" ~> opt(intLiteral) <~ "]") ~ annotation
                                                           ^^ { case t ~ n ~ d ~ a =>
                                                                  val at = ArrayTypeRef(t.name, t.path, d.getOrElse(0))
                                                                  StructField(n, at, a)
                                                              }
  | typeRef ~ ident ~ annotation                           ^^ { case t ~ n ~ a => StructField(n, t, a) }
  )
  
  def struct = "struct" ~> ident ~ opt(":" ~> typeRef) ~ annotation ~ ("{" ~> rep(structField <~ ";") <~ "}")    ^^
                                                              { case name ~ parent ~ annotation ~ fields => Struct(name, parent, fields, annotation) }

  def typedef = "typedef" ~> typeRef ~ ident ~ annotation  ^^ { case t ~ n ~ a => Typedef(n, t, a) }

  def namespace = "namespace" ~> ident ~ ("{" ~> rep(typeDecl) <~ "}")    ^^
                                                              { case name ~ types => Namespace(name, types, Seq.empty) }
  
  def typeDecl = (struct | typedef) <~ ";"

  // format: ON
}
