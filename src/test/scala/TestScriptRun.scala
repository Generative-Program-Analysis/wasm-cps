package wasm

import wasm.parser.Parser
import wasm.miniwasmscript.ScriptRunner

import org.scalatest.funsuite.AnyFunSuite

class TestScriptRun extends AnyFunSuite {
  def testFile(filename: String): Unit = {
    val script = Parser.parseScriptFile(filename).get
    val runner = new ScriptRunner()
    runner.run(script)
  }

  test("simple script") {
    testFile("./benchmarks/wasm/script/script_basic.wast")
  }

  test("simple bin script") {
    testFile("./benchmarks/wasm/script/script_basic.bin.wast")
  }

}
