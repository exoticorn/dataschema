package de.exoticorn.dataschema.testhelper

import org.scalatest.TestFailedException

trait FixCallstack {
  def fixCallstack(f: => Unit) {
    try {
      f
    } catch {
      case e: TestFailedException =>
        e.setStackTrace(e.getStackTrace().drop(3))
        throw e
    }
  }
}