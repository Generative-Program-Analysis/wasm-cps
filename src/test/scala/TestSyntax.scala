package wasm

import wasm.parser.Parser
import org.scalatest.funsuite.AnyFunSuite

class TestSyntax extends AnyFunSuite {
  def testFile(filename: String) = {
    val script = Parser.parseScriptFile(filename)
    println(s"script = $script")
    assert(script != None, "this syntax is not defined in antlr grammar")
  }

  test("basic script") {
    testFile("./benchmarks/wasm/script/script_basic.wast")
  }
}

