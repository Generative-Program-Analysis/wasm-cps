package wasm

import wasm.ast._
import wasm.parser._
import wasm.memory._
import wasm.miniwasm._
import wasm.miniwasmscript.ScriptRunner
import collection.mutable.ArrayBuffer

import org.scalatest.funsuite.AnyFunSuite

class TestEval extends AnyFunSuite {
  abstract class ExpResult
  case class ExpInt(i: Int) extends ExpResult
  case class ExpStack(stack: List[Value]) extends ExpResult
  case object Ignore extends ExpResult

  implicit def toI32V(i: Int): Value = I32V(i)

  def testFile(filename: String, main: Option[String] = None, expected: ExpResult = Ignore) = {
    val module = Parser.parseFile(filename)
    //println(module)
    val evaluator = Evaluator(ModuleInstance(module))
    val haltK: evaluator.Cont[Unit] = stack => {
      println(s"halt cont: $stack")
      expected match {
        case ExpInt(e) => assert(stack(0) == I32V(e))
        case ExpStack(e) => assert(stack == e)
        case Ignore    => ()
      }
    }
    evaluator.evalTop(haltK, main)
  }

  def testWastFile(filename: String): Unit = {
    val script = Parser.parseScriptFile(filename).get
    val runner = new ScriptRunner()
    runner.run(script)
  }

  // TODO: the power test can be used to test the stack
  // For now: 2^10 works, but 2^31 * 2 = 0 and it's 0 for all the above, so maybe not a good benchmark(?)
  // and 2^1000 results in a stack overflow
  test("ack") { testFile("./benchmarks/wasm/ack.wat", Some("real_main"), ExpInt(7)) }
  test("power") { testFile("./benchmarks/wasm/pow.wat", Some("real_main"), ExpInt(1024)) }
  test("start") { testFile("./benchmarks/wasm/start.wat") }
  test("fact") { testFile("./benchmarks/wasm/fact.wat", None, ExpInt(120)) }
  test("loop") { testFile("./benchmarks/wasm/loop.wat", None, ExpInt(10)) }
  test("even-odd") { testFile("./benchmarks/wasm/even_odd.wat", None, ExpInt(1)) }
  test("load") { testFile("./benchmarks/wasm/load.wat", None, ExpInt(1)) }
  test("btree") { testFile("./benchmarks/wasm/btree/2o1u-unlabeled.wat") }
  test("fib") { testFile("./benchmarks/wasm/fib.wat", None, ExpInt(144)) }
  test("tribonacci") { testFile("./benchmarks/wasm/tribonacci.wat", None, ExpInt(504)) }

  test("return") {
    intercept[wasm.miniwasm.Trap] {
      testFile("./benchmarks/wasm/return.wat", Some("$real_main"))
    }
  }

  test("block input") {
    testFile("./benchmarks/wasm/block.wat", Some("real_main"), ExpInt(9))
  }
  test("loop block input") {
    testFile("./benchmarks/wasm/block.wat", Some("test_loop_input"), ExpInt(55))
  }
  test("if block input") {
    testFile("./benchmarks/wasm/block.wat", Some("test_if_input"), ExpInt(25))
  }
  test("block input - poly br") {
    testFile("./benchmarks/wasm/block.wat", Some("test_poly_br"), ExpInt(0))
  }
  test("loop block - poly br") {
    testFile("./benchmarks/wasm/loop_poly.wat", None, ExpStack(List(2, 1)))
  }

  // just for parsing
  test("fx types") {
    testFile("./benchmarks/wasm/wasmfx/cont1-stripped.wat")
  }

  // Spec tests
  test("spectest_block") {
    testWastFile("./benchmarks/wasm/spectest/block.bin.wast")
  }

  test("spectest_br_if") {
    testWastFile("./benchmarks/wasm/spectest/br_if.bin.wast")
  }

  test("spectest_br") {
    testWastFile("./benchmarks/wasm/spectest/return_call.bin.wast")
  }

  test("spectest_call") {
    testWastFile("./benchmarks/wasm/spectest/call.bin.wast")
  }

  test("spectest_loop") {
    testWastFile("./benchmarks/wasm/spectest/loop.bin.wast")
  }

  test("spectest_return") {
    testWastFile("./benchmarks/wasm/spectest/return.bin.wast")
  }

}
