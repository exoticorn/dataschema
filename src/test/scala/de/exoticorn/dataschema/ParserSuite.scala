package de.exoticorn.dataschema

import org.scalatest.{ FunSuite, TestFailedException }
import de.exoticorn.dataschema.ast._

class ParserSuite extends FunSuite {
  def fixCallstack(f: => Unit) {
    try {
      f
    } catch {
      case e: TestFailedException =>
        e.setStackTrace(e.getStackTrace().drop(3))
        throw e
    }
  }

  def parse[A](parser: Parser.Parser[A], input: String, output: A) {
    val result = Parser.parseAll(parser, input)
    fixCallstack {
      assert(result.successful)
      assert(result.get === output)
    }
  }

  def parseFail[A](parser: Parser.Parser[A], input: String) {
    val result = Parser.parseAll(parser, input)
    fixCallstack {
      assert(!result.successful)
    }
  }

  test("ident") {
    parse(Parser.ident, "abc", "abc")
    parse(Parser.ident, "ab4f_a", "ab4f_a")
    parseFail(Parser.ident, "-fd")
    parseFail(Parser.ident, "12abc")
  }

  test("type reference") {
    parse(Parser.typeRef, "abc", TypeRef("abc", Seq.empty))
    parse(Parser.typeRef, "foo::bar::abc", TypeRef("abc", Seq("foo", "bar")))
  }

  test("namespace path") {
    parse(Parser.namespacePath, "foo::", Seq("foo"))
    parse(Parser.namespacePath, "foo::bar::", Seq("foo", "bar"))
    parseFail(Parser.namespacePath, "foo::bar")
  }

  test("annotationType") {
    parse(Parser.annotationType, "string", "string")
    parse(Parser.annotationType, "int", "int")
    parse(Parser.annotationType, "float", "float")
  }

  test("annotation field") {
    parse(Parser.annotationField, "string foo", AnnotationField("foo", Some("string"), None))
    parse(Parser.annotationField, "string foo = \"bar\"", AnnotationField("foo", Some("string"), Some("bar")))
    parse(Parser.annotationField, "foo = 42.1", AnnotationField("foo", None, Some(42.1f)))
  }

  test("literal") {
    parse(Parser.literal, "\"foo\"", "foo")
    parse(Parser.literal, "1", 1)
    parse(Parser.literal, "-32", -32)
    parse(Parser.literal, "1.2", 1.2f)
    parse(Parser.literal, "-2.4", -2.4f)
  }

  test("annotation") {
    parse(Parser.annotation, "<< foo = 1 >>", Seq(AnnotationField("foo", None, Some(1))))
  }

  test("struct field") {
    parse(Parser.structField, "foo bar", StructField("bar", TypeRef("foo", Seq.empty), Seq.empty))
    parse(Parser.structField, "foo bar << float abc = -2.2 >>",
      StructField("bar", TypeRef("foo", Seq.empty), Seq(AnnotationField("abc", Some("float"), Some(-2.2f)))))
    parse(Parser.structField, "foo bar[]", StructField("bar", ArrayTypeRef("foo", Seq.empty, 0), Seq.empty))
  }

  test("struct") {
    parse(Parser.struct, "struct foo { foo bar; }", Struct("foo", None, Seq(StructField("bar", TypeRef("foo", Seq.empty), Seq.empty)), Seq.empty))
    parse(Parser.struct, "struct foo << foo = 1 >> {}", Struct("foo", None, Seq.empty, Seq(AnnotationField("foo", None, Some(1)))))
    parse(Parser.struct, "struct foo : bar << foo = 1 >> {}", Struct("foo", Some(TypeRef("bar", Seq.empty)), Seq.empty, Seq(AnnotationField("foo", None, Some(1)))))
  }

  test("typedef") {
    parse(Parser.typedef, "typedef foo bar", Typedef("bar", TypeRef("foo", Seq.empty), Seq.empty))
    parse(Parser.typedef, "typedef foo bar << foo = 1>>", Typedef("bar", TypeRef("foo", Seq.empty), Seq(AnnotationField("foo", None, Some(1)))))
  }

  test("namespace") {
    parse(Parser.namespace, "namespace foo {}", Namespace("foo", Seq.empty, Seq.empty))
    parse(Parser.namespace, "namespace foo { struct bar{}; }", Namespace("foo", Seq(Struct("bar", None, Seq.empty, Seq.empty)), Seq.empty))
    parse(Parser.namespace, "namespace foo { typedef bar baz; }", Namespace("foo", Seq(Typedef("baz", TypeRef("bar", Seq.empty), Seq.empty)), Seq.empty))
    //parse(Parser.namespace, "namespace foo { namespace bar {} }", Namespace("foo", Seq.empty, Seq(Namespace("bar", Seq.empty, Seq.empty))))
  }
}
