package de.exoticorn.dataschema

import org.scalatest.{ FunSuite, TestFailedException }

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
}
