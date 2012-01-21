package de.exoticorn.dataschema

object TypeResolver {
  case class State(types: Map[String, definition.TypeDecl], namespace: Seq[String])

  def apply(element: ast.TypeRefBase, state: State): definition.TypeDecl = element match {
    case ast.TypeRef("string", Seq()) => definition.StringType(Map.empty)
    case ast.TypeRef("int", Seq()) => definition.IntType(Map.empty)
    case ast.TypeRef("float", Seq()) => definition.FloatType(Map.empty)
    case ast.TypeRef(name, path) => state.types(name)
  }
}