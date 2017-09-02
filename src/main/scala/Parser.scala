import fastparse.all._
import scala.annotation.tailrec
import scala.util.matching.Regex
import Lang._

/**
  * Parse Lisp style source code
  */

object Parser {

  def apply(source: String): Either[String,Exp] = {
    exp.parse(source) match {
      case Parsed.Success(ast,_) => Right(ast)
      case err => Left(err.toString)
    }
  }

  def fromFile(path: String): Either[String,Exp] = {
    val source = io.Source.fromFile(path).mkString
    sourceFile.parse(source) match {
      case Parsed.Success(ast,_) => Right(ast)
      case err => Left(err.toString)
    }
  }

  val special = "(); \n\t\r\b"

  val sym: P[Sym] = P(CharsWhile(!special.contains(_))).!
    .map(Sym(_))

  val typ1: P[Typ1] = P(CharIn('A' to 'Z') ~ CharsWhile(!special.contains(_))).!
    .map(Typ1)

  val bool: P[Bool] = P("true" | "false").!.map {
    case "true" => Bool(true)
    case "false" => Bool(false)
  }
  val digit = CharIn('0' to '9')
  val int: P[Int64] = P("-".? ~ digit.rep(min = 1)).!
    .map(s => Int64(s.toInt))
  val float: P[Float64] = P("-".? ~ digit.rep(min = 1) ~ "." ~ digit.rep(min = 0)).!
    .map(s => Float64(s.toDouble))

  val text: P[Text] = P("\"" ~ CharsWhile(_ != '"').?.! ~ "\"").map(Text)
  val comment: P[Unit] = P(";;" ~ CharsWhile(_ != '\n'))

  val atom: P[Exp] = P(float | int | text | bool | typ1 | sym)


  val s: P[Unit] = P(CharsWhileIn(" \n\t\r\b").?)

  // TODO: comments don't work properly
  val exp: P[Exp] = P(s ~ comment.rep(sep = "\n").? ~ s ~ (fn | atom) ~  s)
  val fn: P[Fn] = P("(" ~ s ~ sym ~ s ~ exp.rep ~ s ~ ")")
    .map {
      case (symbol,args) => Fn(symbol.name, TypInfer, args.toVector)
    }

  val sourceFile: P[Fn] = P(exp.rep ~ End)
    .map(Fn("do", TypInfer, _))



  // Manual simple parsing (old code)

  def tokenizeSimple(source: String): List[String] =
    source.replaceAll("[)(]", " $0 ").trim.split("\\s+").toList

  @tailrec
  def parseSimple(tokens: List[String], expr: Fn = Fn("do"), stack: List[Fn] = List()): Either[String,Fn] =
    tokens match {
      case "(" :: name :: rest =>
        parseSimple(rest, Fn(name), expr :: stack)
      case ")" :: rest =>
        if (stack.isEmpty) Left("Unexpected )")
        else               parseSimple(rest, stack.head :+ expr, stack.tail)
      case symbol :: rest =>
        parseSimple(rest, expr :+ toAtom(symbol), stack)
      case _ =>
        if (stack.isEmpty) Right(expr)
        else               Left("Unexpected EOF")
    }

  def toAtom(id: String): Exp = id match {
    case reText(str)  => Text(str)
    case reInt(str)   => Int64(str.toInt)
    case reFloat(str) => Float64(str.toDouble)
    case reBool(str)  => Bool(str.toBoolean)
    case reType(str)  => Typ1(str)
    case _            => Sym(id)
  }

  val reBool: Regex = "(true|false)".r
  val reText: Regex = "\"(.+)\"".r
  val reFloat: Regex = "([0-9]+\\.[0-9]*)".r
  val reInt: Regex = "([0-9]+)".r
  val reType: Regex = "([A-Z].*)".r

}

