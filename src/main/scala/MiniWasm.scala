package wasm.miniwasm

import wasm.ast._
import wasm.memory._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import Console.{GREEN, RED, RESET, YELLOW_B, UNDERLINED}

/* This is the vanilla interpreter for μWasm */

case class Evaluator(module: ModuleInstance) {
  implicit val m: ModuleInstance = module

  type Cont[A] = List[Value] => A

  def evalCall[Ans](rest: List[Instr],
                    stack: List[Value],
                    frame: Frame,
                    kont: Cont[Ans],
                    trail: List[Cont[Ans]],
                    funcIndex: Int,
                    isTail: Boolean): Ans =
    module.funcs(funcIndex) match
      case FuncDef(_, FuncBodyDef(ty, _, locals, body)) =>
        val args = stack.take(ty.inps.size).reverse
        val newStack = stack.drop(ty.inps.size)
        val frameLocals = args ++ locals.map(zero(_))
        val newFrame = Frame(ArrayBuffer(frameLocals: _*))
        val restK: Cont[Ans] = (retStack) =>
          eval(rest, retStack.take(ty.out.size) ++ newStack, frame, kont, trail)
        eval(body, List(), newFrame, restK, List(restK))
      case Import("console", "log", _) =>
        val I32V(v) :: newStack = stack
        println(v)
        eval(rest, newStack, frame, kont, trail)
      case Import("spectest", "print_i32", _) =>
        val I32V(v) :: newStack = stack
        println(v)
        eval(rest, newStack, frame, kont, trail)
      case Import(_, _, _) => throw new Exception(s"Unknown import at $funcIndex")
      case _               => throw new Exception(s"Definition at $funcIndex is not callable")

  def eval[Ans](insts: List[Instr],
                stack: List[Value],
                frame: Frame,
                kont: Cont[Ans],
                trail: List[Cont[Ans]]): Ans =
    if (insts.isEmpty) return kont(stack)
    val inst = insts.head
    val rest = insts.tail
    inst match
      case Drop => eval(rest, stack.tail, frame, kont, trail)
      case Select(_) =>
        val I32V(cond) :: v2 :: v1 :: newStack = stack
        val value = if (cond == 0) v1 else v2
        eval(rest, value :: newStack, frame, kont, trail)
      case LocalGet(i) =>
        eval(rest, frame.locals(i) :: stack, frame, kont, trail)
      case LocalSet(i) =>
        val value :: newStack = stack
        frame.locals(i) = value
        eval(rest, newStack, frame, kont, trail)
      case LocalTee(i) =>
        val value :: newStack = stack
        frame.locals(i) = value
        eval(rest, stack, frame, kont, trail)
      case GlobalGet(i) =>
        eval(rest, module.globals(i).value :: stack, frame, kont, trail)
      case GlobalSet(i) =>
        val value :: newStack = stack
        module.globals(i).ty match
          case GlobalType(tipe, true) if value.tipe == tipe =>
            module.globals(i).value = value
          case GlobalType(_, true) => throw new Exception("Invalid type")
          case _                   => throw new Exception("Cannot set immutable global")
        eval(rest, newStack, frame, kont, trail)
      case MemorySize =>
        eval(rest, I32V(module.memory.head.size) :: stack, frame, kont, trail)
      case MemoryGrow =>
        val I32V(delta) :: newStack = stack
        val mem = module.memory.head
        val oldSize = mem.size
        mem.grow(delta) match
          case Some(e) =>
            eval(rest, I32V(-1) :: newStack, frame, kont, trail)
          case _ =>
            eval(rest, I32V(oldSize) :: newStack, frame, kont, trail)
      case MemoryFill =>
        val I32V(value) :: I32V(offset) :: I32V(size) :: newStack = stack
        if (memOutOfBound(module, 0, offset, size))
          throw new Exception("Out of bounds memory access") // GW: turn this into a `trap`?
        else
          module.memory.head.fill(offset, size, value.toByte)
          eval(rest, newStack, frame, kont, trail)
      case MemoryCopy =>
        val I32V(n) :: I32V(src) :: I32V(dest) :: newStack = stack
        if (memOutOfBound(module, 0, src, n) || memOutOfBound(module, 0, dest, n))
          throw new Exception("Out of bounds memory access")
        else
          module.memory.head.copy(dest, src, n)
          eval(rest, newStack, frame, kont, trail)
      case Const(n) => eval(rest, n :: stack, frame, kont, trail)
      case Binary(op) =>
        val v2 :: v1 :: newStack = stack
        eval(rest, evalBinOp(op, v1, v2) :: newStack, frame, kont, trail)
      case Unary(op) =>
        val v :: newStack = stack
        eval(rest, evalUnaryOp(op, v) :: newStack, frame, kont, trail)
      case Compare(op) =>
        val v2 :: v1 :: newStack = stack
        eval(rest, evalRelOp(op, v1, v2) :: newStack, frame, kont, trail)
      case Test(op) =>
        val v :: newStack = stack
        eval(rest, evalTestOp(op, v) :: newStack, frame, kont, trail)
      case Store(StoreOp(align, offset, ty, None)) =>
        val I32V(v) :: I32V(addr) :: newStack = stack
        module.memory(0).storeInt(addr + offset, v)
        eval(rest, newStack, frame, kont, trail)
      case Load(LoadOp(align, offset, ty, None, None)) =>
        val I32V(addr) :: newStack = stack
        val value = module.memory(0).loadInt(addr + offset)
        eval(rest, I32V(value) :: newStack, frame, kont, trail)
      case Nop =>
        eval(rest, stack, frame, kont, trail)
      case Unreachable => throw Trap()
      case Block(ty, inner) =>
        val funcTy = getFuncType(ty)
        val (inputs, restStack) = stack.splitAt(funcTy.inps.size)
        val restK: Cont[Ans] = (retStack) =>
          eval(rest, retStack.take(funcTy.out.size) ++ restStack, frame, kont, trail)
        eval(inner, inputs, frame, restK, restK :: trail)
      case Loop(ty, inner) =>
        // We construct two continuations, one for the break (to the begining of the loop),
        // and one for fall-through to the next instruction following the syntactic structure
        // of the program.
        val funcTy = getFuncType(ty)
        val (inputs, restStack) = stack.splitAt(funcTy.inps.size)
        val restK: Cont[Ans] = (retStack) =>
          eval(rest, retStack.take(funcTy.out.size) ++ restStack, frame, kont, trail)
        def loop(retStack: List[Value]): Ans =
          eval(inner, retStack.take(funcTy.inps.size), frame, restK, loop _ :: trail)
        loop(inputs)
      case If(ty, thn, els) =>
        val funcTy = getFuncType(ty)
        val I32V(cond) :: newStack = stack
        val inner = if (cond != 0) thn else els
        val (inputs, restStack) = newStack.splitAt(funcTy.inps.size)
        val restK: Cont[Ans] = (retStack) =>
          eval(rest, retStack.take(funcTy.out.size) ++ restStack, frame, kont, trail)
        eval(inner, inputs, frame, restK, restK :: trail)
      case Br(label) =>
        trail(label)(stack)
      case BrIf(label) =>
        val I32V(cond) :: newStack = stack
        if (cond != 0) trail(label)(newStack)
        else eval(rest, newStack, frame, kont, trail)
      case BrTable(labels, default) =>
        val I32V(cond) :: newStack = stack
        val goto = if (cond < labels.length) labels(cond) else default
        trail(goto)(newStack)
      case Return        => trail.last(stack)
      case Call(f)       => evalCall(rest, stack, frame, kont, trail, f, false)
      case _             => throw new Exception(s"instruction $inst not implemented")

  // If `main` is given, then we use that function as the entry point of the program;
  // otherwise, we look up the top-level `start` instruction to locate the entry point.
  def evalTop[Ans](halt: Cont[Ans], main: Option[String] = None): Ans =
    val instrs = extractMainInstrs(module, main)
    val locals = extractLocals(module, main)
    if (instrs.isEmpty) println("Warning: nothing is executed")
    eval(instrs, List(), Frame(ArrayBuffer(I32V(0))), halt, List(halt))

  def evalTop(m: ModuleInstance): Unit = evalTop(stack => ())
}