package wasm.miniwasm

import wasm.ast._
import wasm.memory._

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import Console.{GREEN, RED, RESET, YELLOW_B, UNDERLINED}

case class Trap() extends Exception

case class ModuleInstance(
  defs: List[Definition],
  types: List[FuncLikeType],
  tags: List[FuncType],
  funcs: HashMap[Int, Callable],
  memory: List[RTMemory] = List(RTMemory()),
  globals: List[RTGlobal] = List(),
  exports: List[Export] = List()
)

case class Frame(locals: ArrayBuffer[Value])

object ModuleInstance {
  def apply(module: Module): ModuleInstance = {
    val types = module.definitions
      .collect({
        case TypeDef(_, ft) => ft
      })
      .toList
    val tags = module.definitions
      .collect({
        case Tag(id, ty) => ty
      })
      .toList

    val funcs = module.definitions
      .collect({
        case FuncDef(_, fndef @ FuncBodyDef(_, _, _, _)) => fndef
      })
      .toList

    val globals = module.definitions
      .collect({
        case Global(_, GlobalValue(ty, e)) =>
          (e.head) match {
            case Const(c) => RTGlobal(ty, c)
            // Q: What is the default behavior if case in non-exhaustive
            case _ => ???
          }
      })
      .toList

    // TODO: correct the behavior for memory
    val memory = module.definitions
      .collect({
        case Memory(id, MemoryType(min, max_opt)) =>
          RTMemory(min, max_opt)
      })
      .toList

    val exports = module.definitions
      .collect({
        case e @ Export(_, ExportFunc(_)) => e
      })
      .toList

    ModuleInstance(module.definitions, types, tags, module.funcEnv, memory, globals, exports)
  }
}

def evalBinOp(op: BinOp, lhs: Value, rhs: Value): Value = op match
  case Add(_) =>
    (lhs, rhs) match
      case (I32V(v1), I32V(v2)) => I32V(v1 + v2)
      case (I64V(v1), I64V(v2)) => I64V(v1 + v2)
      case (F32V(v1), F32V(v2)) => F32V(v1 + v2)
      case (F64V(v1), F64V(v2)) => F64V(v1 + v2)
      case _                    => throw new Exception("Invalid types")
  case Mul(_) =>
    (lhs, rhs) match
      case (I32V(v1), I32V(v2)) => I32V(v1 * v2)
      case (I64V(v1), I64V(v2)) => I64V(v1 * v2)
      case _                    => throw new Exception("Invalid types")
  case Sub(_) =>
    (lhs, rhs) match
      case (I32V(v1), I32V(v2)) => I32V(v1 - v2)
      case (I64V(v1), I64V(v2)) => I64V(v1 - v2)
      case _                    => throw new Exception("Invalid types")
  case Shl(_) =>
    (lhs, rhs) match
      case (I32V(v1), I32V(v2)) => I32V(v1 << v2)
      case (I64V(v1), I64V(v2)) => I64V(v1 << v2)
      case _                    => throw new Exception("Invalid types")
  case ShrU(_) =>
    (lhs, rhs) match
      case (I32V(v1), I32V(v2)) => I32V(v1 >>> v2)
      case (I64V(v1), I64V(v2)) => I64V(v1 >>> v2)
      case _                    => throw new Exception("Invalid types")
  case And(_) =>
    (lhs, rhs) match
      case (I32V(v1), I32V(v2)) => I32V(v1 & v2)
      case (I64V(v1), I64V(v2)) => I64V(v1 & v2)
      case _                    => throw new Exception("Invalid types")
  case _ => ???

def evalUnaryOp(op: UnaryOp, value: Value) = op match
  case Clz(_) =>
    value match
      case I32V(v) => I32V(Integer.numberOfLeadingZeros(v))
      case I64V(v) => I64V(java.lang.Long.numberOfLeadingZeros(v))
      case _       => throw new Exception("Invalid types")
  case Ctz(_) =>
    value match
      case I32V(v) => I32V(Integer.numberOfTrailingZeros(v))
      case I64V(v) => I64V(java.lang.Long.numberOfTrailingZeros(v))
      case _       => throw new Exception("Invalid types")
  case Popcnt(_) =>
    value match
      case I32V(v) => I32V(Integer.bitCount(v))
      case I64V(v) => I64V(java.lang.Long.bitCount(v))
      case _       => throw new Exception("Invalid types")
  case _ => ???

def evalRelOp(op: RelOp, lhs: Value, rhs: Value) = op match
  case Eq(_) =>
    (lhs, rhs) match
      case (I32V(v1), I32V(v2)) => I32V(if (v1 == v2) 1 else 0)
      case (I64V(v1), I64V(v2)) => I32V(if (v1 == v2) 1 else 0)
      case _                    => throw new Exception("Invalid types")
  case Ne(_) =>
    (lhs, rhs) match
      case (I32V(v1), I32V(v2)) => I32V(if (v1 != v2) 1 else 0)
      case (I64V(v1), I64V(v2)) => I32V(if (v1 != v2) 1 else 0)
      case _                    => throw new Exception("Invalid types")
  case LtS(_) =>
    (lhs, rhs) match
      case (I32V(v1), I32V(v2)) => I32V(if (v1 < v2) 1 else 0)
      case (I64V(v1), I64V(v2)) => I32V(if (v1 < v2) 1 else 0)
      case _                    => throw new Exception("Invalid types")
  case LtU(_) =>
    (lhs, rhs) match
      case (I32V(v1), I32V(v2)) =>
        I32V(if (Integer.compareUnsigned(v1, v2) < 0) 1 else 0)
      case (I64V(v1), I64V(v2)) =>
        I32V(if (java.lang.Long.compareUnsigned(v1, v2) < 0) 1 else 0)
      case _ => throw new Exception("Invalid types")
  case GtS(_) =>
    (lhs, rhs) match
      case (I32V(v1), I32V(v2)) => I32V(if (v1 > v2) 1 else 0)
      case (I64V(v1), I64V(v2)) => I32V(if (v1 > v2) 1 else 0)
      case _                    => throw new Exception("Invalid types")
  case GtU(_) =>
    (lhs, rhs) match
      case (I32V(v1), I32V(v2)) =>
        I32V(if (Integer.compareUnsigned(v1, v2) > 0) 1 else 0)
      case (I64V(v1), I64V(v2)) =>
        I32V(if (java.lang.Long.compareUnsigned(v1, v2) > 0) 1 else 0)
      case _ => throw new Exception("Invalid types")
  case LeS(_) =>
    (lhs, rhs) match
      case (I32V(v1), I32V(v2)) => I32V(if (v1 <= v2) 1 else 0)
      case (I64V(v1), I64V(v2)) => I32V(if (v1 <= v2) 1 else 0)
      case _                    => throw new Exception("Invalid types")
  case LeU(_) =>
    (lhs, rhs) match
      case (I32V(v1), I32V(v2)) =>
        I32V(if (Integer.compareUnsigned(v1, v2) <= 0) 1 else 0)
      case (I64V(v1), I64V(v2)) =>
        I32V(if (java.lang.Long.compareUnsigned(v1, v2) <= 0) 1 else 0)
      case _ => throw new Exception("Invalid types")
  case GeS(_) =>
    (lhs, rhs) match
      case (I32V(v1), I32V(v2)) => I32V(if (v1 >= v2) 1 else 0)
      case (I64V(v1), I64V(v2)) => I32V(if (v1 >= v2) 1 else 0)
      case _                    => throw new Exception("Invalid types")
  case GeU(_) =>
    (lhs, rhs) match
      case (I32V(v1), I32V(v2)) =>
        I32V(if (Integer.compareUnsigned(v1, v2) >= 0) 1 else 0)
      case (I64V(v1), I64V(v2)) =>
        I32V(if (java.lang.Long.compareUnsigned(v1, v2) >= 0) 1 else 0)
      case _ => throw new Exception("Invalid types")

def evalTestOp(op: TestOp, value: Value) = op match
  case Eqz(_) =>
    value match
      case I32V(v) => I32V(if (v == 0) 1 else 0)
      case I64V(v) => I32V(if (v == 0) 1 else 0)
      case _       => throw new Exception("Invalid types")

def memOutOfBound(module: ModuleInstance, memoryIndex: Int, offset: Int, size: Int) = {
  val memory = module.memory(memoryIndex)
  offset + size > memory.size
}

def zero(t: ValueType): Value = t match
  case NumType(kind) =>
    kind match
      case I32Type => I32V(0)
      case I64Type => I64V(0)
      case F32Type => F32V(0)
      case F64Type => F64V(0)
  case VecType(kind) => ???
  case RefType(kind) => RefNullV(kind)

def getFuncType(ty: BlockType): FuncType =
  ty match
    case VarBlockType(_, None) => ??? // TODO: fill this branch until we handle type index correctly
    case VarBlockType(_, Some(tipe)) => tipe
    case ValBlockType(Some(tipe))    => FuncType(List(), List(), List(tipe))
    case ValBlockType(None)          => FuncType(List(), List(), List())

def extractMainInstrs(module: ModuleInstance, main: Option[String]): List[Instr] =
  main match
    case Some(func_name) =>
      module.defs.flatMap({
        case Export(`func_name`, ExportFunc(fid)) =>
          System.err.println(s"Entering function $main")
          module.funcs(fid) match
            case FuncDef(_, FuncBodyDef(_, _, locals, body)) => body
            case _ => throw new Exception("Entry function has no concrete body")
        case _ => List()
      })
    case None =>
      module.defs.flatMap({
        case Start(id) =>
          System.err.println(s"Entering unnamed function $id")
          module.funcs(id) match
            case FuncDef(_, FuncBodyDef(_, _, locals, body)) => body
            case _ => throw new Exception("Entry function has no concrete body")
        case _ => List()
      })

def extractLocals(module: ModuleInstance, main: Option[String]): List[ValueType] =
  main match
    case Some(func_name) =>
      module.defs.flatMap({
        case Export(`func_name`, ExportFunc(fid)) =>
          System.err.println(s"Entering function $main")
          module.funcs(fid) match
            case FuncDef(_, FuncBodyDef(_, _, locals, _)) => locals
            case _ => throw new Exception("Entry function has no concrete body")
        case _ => List()
      })
    case None =>
      module.defs.flatMap({
        case Start(id) =>
          System.err.println(s"Entering unnamed function $id")
          module.funcs(id) match
            case FuncDef(_, FuncBodyDef(_, _, locals, body)) => locals
            case _ => throw new Exception("Entry function has no concrete body")
        case _ => List()
      })