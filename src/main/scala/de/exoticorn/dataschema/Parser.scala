package de.exoticorn.dataschema

import scala.util.parsing.combinator._

object Parser extends RegexParsers {
  // format: OFF
  
  def ident = """[a-zA-Z_]\w*""".r

  // format: ON
}
