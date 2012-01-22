package de.exoticorn.dataschema

import org.scalatest.FunSuite

class TypeResolverSuite extends FunSuite with testhelper.FixCallstack {
  private val emptyState = TypeResolver.State(Map.empty, Seq.empty)

  test("primitive types") {
    assert(TypeResolver(ast.TypeRef("string", Seq.empty), emptyState) === definition.StringType(Map.empty))
    assert(TypeResolver(ast.TypeRef("int", Seq.empty), emptyState) === definition.IntType(Map.empty))
    assert(TypeResolver(ast.TypeRef("float", Seq.empty), emptyState) === definition.FloatType(Map.empty))
  }

  private val simpleVariables =
    Map("foo" -> definition.StringType(Map.empty),
      "bar::foo" -> definition.IntType(Map.empty),
      "baz::foo" -> definition.FloatType(Map.empty))

  test("simple type lookup") {
    val state = TypeResolver.State(simpleVariables, Seq.empty)
    assert(TypeResolver(ast.TypeRef("foo", Seq.empty), state) === definition.StringType(Map.empty))
  }

  test("namespace type lookup") {
    val state = TypeResolver.State(simpleVariables, Seq.empty)
    assert(TypeResolver(ast.TypeRef("foo", Seq("bar")), state) === definition.IntType(Map.empty))
  }

  test("type lookup from namespace") {
    val state = TypeResolver.State(simpleVariables, Seq("bar"))
    assert(TypeResolver(ast.TypeRef("foo", Seq.empty), state) === definition.IntType(Map.empty))
    assert(TypeResolver(ast.TypeRef("foo", Seq("baz")), state) === definition.FloatType(Map.empty))
  }
}