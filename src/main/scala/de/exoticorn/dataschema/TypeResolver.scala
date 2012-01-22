package de.exoticorn.dataschema

object TypeResolver {
  case class State(types: Map[String, definition.TypeDecl], namespace: Seq[String])

  def apply(element: ast.TypeRefBase, state: State): definition.TypeDecl = element match {
    case ast.TypeRef("string", Seq()) => definition.StringType(Map.empty)
    case ast.TypeRef("int", Seq()) => definition.IntType(Map.empty)
    case ast.TypeRef("float", Seq()) => definition.FloatType(Map.empty)
    case ast.TypeRef(name, path) => findType(state.types, state.namespace, (path :+ name).mkString("::"))
  }

  private def findType(types: Map[String, definition.TypeDecl], path: Seq[String], name: String): definition.TypeDecl = {
    val fullName = (path :+ name).mkString("::")
    types.get(fullName) match {
      case Some(t) => t
      case None =>
        if (path.isEmpty) throw new Exception("Type not found: " + fullName)
        else findType(types, path.tail, name)
    }
  }
}