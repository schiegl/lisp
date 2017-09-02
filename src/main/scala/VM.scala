import java.util

import VM.ByteCode

import scala.collection.mutable

/**
  * This is a simple stack machine with support for integer and boolean values
  * I constrained myself to using arrays and integers (pointers) and basic C constructs (for the most part) for the VM
  *
  * Next steps: floating point numbers, strings, heap allocation, garbage collection, ...
  */

object VM {

  def apply(byteCode: ByteCode): VM = new VM(byteCode)

  type ByteCode = Array[Int]
  type Instruction = Int

  val HALT  = 0  // stop VM
  val PUSH  = 1  // push value to stack
  val POP   = 2  // pop value from stack
  val SET   = 3  // e.g. SET, A, 5 -> register[A] = 5
  val JMP   = 4  // go to program[next instruction]
  val LT    = 5  // compare if lastlast value is lower than last value
  val EQ    = 6  // compare last two values for equality: 1 if equal otherwise zero
  val NOT   = 7  // flip last stack value
  val ADD   = 8  // add      top two stack values and push result to stack
  val SUB   = 9  // subtract top two stack values and push result to stack
  val MUL   = 10 // multiply top two stack values and push result to stack
  val CALL  = 11 // go to program[next instruction] and save next index on call stack
  val END   = 12 // go to program[stack.pop]
  val JIF   = 13 // jump to program[next instruction] if top value on stack equals 1 (also pop
  val LOAD  = 14 // next instruction is register index, put that register on the stack
  val STORE = 15 // put last stack value in register (next instruction = index)

  val instructions = Array(
    "HALT",
    "PUSH",
    "POP",
    "SET",
    "JMP",
    "LT",
    "EQ",
    "NOT",
    "ADD",
    "SUB",
    "MUL",
    "CALL",
    "END",
    "JIF",
    "LOAD",
    "SAVE",
  )

  // used to pretty print bytecode
  val instArgExpected = Map(
    HALT  -> 0,
    PUSH  -> 1,
    POP   -> 1,
    SET   -> 2,
    JMP   -> 1,
    LT    -> 0,
    EQ    -> 0,
    NOT   -> 0,
    ADD   -> 0,
    SUB   -> 0,
    MUL   -> 0,
    CALL  -> 1,
    END   -> 0,
    JIF   -> 1,
    LOAD  -> 1,
    STORE  -> 1,
  )

  def pretty(bc: ByteCode, i: Int = 0): String = {
    if (bc.nonEmpty) {
      val inst = bc(0)
      val argCount = instArgExpected(inst)
      (i + ":").padTo(5, ' ')  +
        instructions(inst).padTo(5, ' ') + " " + bc.tail.take(argCount).mkString(" ") + "\n" +
        pretty(bc.drop(argCount + 1), i + 1 + argCount)
    } else {
      ""
    }
  }

}

class VM(code: ByteCode) {

  import VM._

  var IP: Int = -1 // Instruction Pointer
  var SP: Int = -1 // Stack Pointer
  var CP: Int = -1 // Call Stack Pointer

  val STACK     = new Array[Int](Short.MaxValue)
  val CALLSTACK = new Array[Int](Short.MaxValue)
  val REGISTERS = new Array[Int](Short.MaxValue)

  /**
    * Evaluate the bytecode
    * @param debug print steps to STDOUT
    * @return      result of the evaluated bytecode
    */

  def eval(debug: Boolean = false): Int = {

    var running = true

    while (running && IP < code.length) {

      IP += 1

      if (debug) {
        println("STACK: " + STACK.take(14).mkString(", "))
        println("REG:   " + REGISTERS.take(7).mkString(", "))
        println()
        print("INST:  " + instructions(code(IP)) + " ")
      }

      code(IP) match {

        case PUSH =>
          SP += 1
          IP += 1
          STACK(SP) = code(IP)
          if (debug)
            print(code(IP))

        case POP =>
          SP -= 1

        case LOAD =>
          IP += 1
          SP += 1
          val regAddr = code(IP)
          STACK(SP) = REGISTERS(regAddr)
          if (debug)
            print(regAddr)

        case STORE =>
          IP += 1
          val regAddr = code(IP)
          REGISTERS(regAddr) = STACK(SP)
          SP -= 1
          if (debug)
            print(regAddr)

        case EQ =>
          val a = STACK(SP)
          SP -= 1
          STACK(SP) = if (a == STACK(SP)) 1 else 0

        case NOT =>
          STACK(SP) = if (1 == STACK(SP)) 0 else 1

        case LT =>
          val a = STACK(SP)
          SP -= 1
          STACK(SP) = if (STACK(SP) < a) 1 else 0

        case ADD =>
          val a = STACK(SP)
          SP -= 1
          STACK(SP) += a

        case SUB =>
          val a = STACK(SP)
          SP -= 1
          STACK(SP) -= a

        case MUL =>
          val a = STACK(SP)
          SP -= 1
          STACK(SP) *= a

        case JMP =>
          IP = code(IP + 1) - 1
          if (debug)
            print(code(IP + 1))

        case CALL =>
          if (debug)
            print(code(IP + 1))
          CP += 1
          CALLSTACK(CP) = IP + 2
          IP = code(IP + 1) - 1

        case END =>
          IP = CALLSTACK(CP) - 1
          CP -= 1

        case JIF =>
          IP = if (1 == STACK(SP))
            code(IP + 1) - 1
          else
            IP + 1
          SP -= 1
          if (debug)
            print(code(IP + 1))

        case HALT =>
          running = false

        case notImpl =>
          throw new Exception(s"Instruction not implemented: $notImpl")

      }

      if (debug)
        println()
    }

    STACK(SP)

  }

}

