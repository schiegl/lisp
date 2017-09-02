import Lang._
import VM._

import scala.collection.mutable.ArrayBuffer

/**
  * Compile a typed expression to bytecode for the VM (supports only simple arithmetic and logical expressions)
  */

object Compiler {

  def main(args: Array[String]): Unit = {

    val cp = new Compiler()

    val Right((ast,_)) = Parser.fromFile("src/main/resources/fac.lisp").flatMap(Typer(_))
    println(ast.pretty(noBreak = false))
    val (bc,fni) = cp.compile(ast)(Map(), Vector())
    println("\nByteCode:")
    println("=========")
    println(VM.pretty(bc :+ HALT))

    val vm = new VM(bc :+ HALT)
    val result = vm.eval()
    println(s"Result: $result")

  }

}

class Compiler {

  type ByteCodeMap = Map[String, Seq[ByteCode] => ByteCode]

  // arguments should look like: [(PUSH,5), (LOAD,1), ..]
  val stdEnv: ByteCodeMap = Map(
    "+"   -> { xs => binOp(xs, Seq(ADD)) },
    "-"   -> { xs => binOp(xs, Seq(SUB)) },
    "*"   -> { xs => binOp(xs, Seq(MUL)) },
    "<"   -> { xs => binOp(xs, Seq(LT))  }, // doesn't work for multiple values
    "="   -> { xs => binOp(xs, if (xs.length == 2) Seq(EQ) else Seq(EQ, PUSH, 1, EQ))  },
    "not" -> { xs => (xs.flatten :+ NOT).toArray },
  )

  /**
    * @param args have push/call instructions before each value
    */
  def binOp(args: Seq[ByteCode], inst: Seq[Instruction]): ByteCode = {
    val ops = ArrayBuffer.fill(args.length - 1)(inst).flatten
    (args.flatten ++ ops).toArray
  }

  // function -> start of function bytecode line number
  type FnI  = Map[String,Int]
  // a sequence of names. the index of the name represents the register
  type Registers = Vector[String]


  /**
    * Compile typed AST to ByteCode
    *
    * @param topExp Expression to compile
    * @param i      Current ByteCode index
    * @param fni    Map from function name to ByteCode index
    * @param regs   Registers
    * @return       ByteCode for VM (without HALT instruction)
    */

  def compile(topExp: Exp, i: Int = 0)(implicit fni: FnI, regs: Registers): (ByteCode, FnI) = topExp match {

    case Fn("defn",_,Seq(Sym(name,_), Fn("params",_,params: Seq[Sym]), exp)) =>

      // arguments are already on stack, store them in the register
      val argStores = Array.range(regs.length, regs.length + params.length).reverse.flatMap(Array(STORE,_))

      // compile function body, add function index to fni
      val defnI = i + 2 // +2 -> JMP, n
      val (bc, fniAfterDefn) = compile(exp, defnI + argStores.length)(fni + (name -> defnI), regs ++ params.map(_.name))
      val defnEndI = i + argStores.length + bc.length + 3 // +3 -> JMP, n, END

      (JMP +: defnEndI +: (argStores ++ bc) :+ END, fniAfterDefn)

    case Fn("def",_,Seq(Sym(name,_), exp)) =>
      val defI = i + 2 // +2 -> JMP, n
      val (bc, fniAfterDefn) = compile(exp, i)(fni + (name -> defI), regs :+ name)
      val defnEndI = i + bc.length + 3 // +3 -> JMP, n, END
      (JMP +: defnEndI +: bc :+ END, fniAfterDefn)

    case Fn("if", _, Seq(clause, conseq, other)) =>
      val (bcClause,_) = compile(clause, i)
      val otherI       = i + bcClause.length + 2 // JIF, conseq line
      val (bcOther,_)  = compile(other, otherI)
      val conseqI      = otherI + bcOther.length + 2 // JMP, n => end of conseq line
      val (bcConseq,_) = compile(conseq, i + bcClause.length)

      (bcClause ++
        Array(JIF, conseqI) ++
        bcOther ++
        Array(JMP, conseqI + bcConseq.length) ++
        bcConseq, fni)

    case Fn("do",_,exps) =>
      val (bc, iAfterDo, fniAfterDo) = exps.foldLeft((Array[Int](), i, fni)) {
        case ((accBC, accI, accFnI), exp) =>
          val (bc, fniAfterBC) = compile(exp, accI)(accFnI, regs)
          (accBC ++ bc, accI + bc.length, fniAfterBC)
      }
      (bc, fniAfterDo)

    case Fn(name, _, args) =>
      val compiledArgs = args.map(compile(_,i)._1)
      val bc = if (fni.contains(name)) {
        (compiledArgs.flatten :+ CALL :+ fni(name)).toArray
      } else {
        stdEnv(name)(compiledArgs)
      }
      (bc, fni)

    // if symbol has non primitive type -> Object load from heap space
    case Sym(name,typ) =>
      if (regs.contains(name)) {
        (Array(LOAD, regs.indexOf(name)), fni)
      } else if (fni.contains(name)) {
        (Array(CALL, fni(name)),fni)
      } else {
        (Array(),fni)
      }

    case Bool(value)  =>
      (Array(PUSH, if (value) 1 else 0), fni)
    case Int64(value) =>
      (Array(PUSH, value), fni)

  }

}
