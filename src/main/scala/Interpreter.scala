import Typer.EnvT

/**
  * Interpret an already typed expression
  */

object Interpreter {

  import Lang._

  def apply(source: String, env: Env = stdEnv, envT: EnvT = Typer.stdEnvTyp) = for {
    ast <- Parser(source)
    res <- Typer(ast, envT)
    (astTyped,envTRes) = res
    (exp,envRes) = eval(astTyped)(env)
  } yield (exp,envRes,astTyped,envTRes)

  def fromFile(path: String) = for {
    ast <- Parser.fromFile(path)
    res <- Typer(ast)
    (astTyped,envT) = res
    (exp,env) = eval(astTyped)
  } yield (exp,env,astTyped,envT)


  type Env = Map[String, Seq[Exp] => Exp]

  // A bunch of unsafe casting happening here but the AST is type checked so it should be fine
  val stdEnv: Env = Map(
    "+" -> {
      case xs: Seq[Exp] if xs.forall(_.isInstanceOf[Int64]) =>
        Int64(xs.map(_.asInstanceOf[Int64].value).sum)
      case xs: Seq[Exp] =>
        Float64(xs.map {
          case Float64(x) => x
          case Int64(x) => x
        }.sum)
    },
    "-" -> {
      case xs: Seq[Exp] if xs.forall(_.isInstanceOf[Int64]) =>
        Int64(xs.map(_.asInstanceOf[Int64].value).reduce(_ - _))
      case xs: Seq[Exp] =>
        Float64(xs.map {
          case Float64(x) => x
          case Int64(x) => x
        }.reduce(_ - _))
    },
    "*" -> {
      case xs: Seq[Exp] if xs.forall(_.isInstanceOf[Int64]) =>
        Int64(xs.map(_.asInstanceOf[Int64].value).product)
      case xs: Seq[Exp] =>
        Float64(xs.map {
          case Float64(x) => x
          case Int64(x) => x
        }.product)
    },
    "/" -> {
      case xs: Seq[Exp] if xs.forall(_.isInstanceOf[Int64]) =>
        Int64(xs.map(_.asInstanceOf[Int64].value).reduce(_ / _))
      case xs: Seq[Exp] =>
        Float64(xs.map {
          case Float64(x) => x
          case Int64(x) => x
        }.reduce(_ / _))
    },
    ">" -> {
      case xs =>
        val nums = xs.map {
          case Int64(x)   => x
          case Float64(x) => x
        }
        Bool(nums.sliding(2).forall { case Seq(x, y) => x > y })
    },
    "<" -> {
      case xs =>
        val nums = xs.map {
          case Int64(x)   => x
          case Float64(x) => x
        }
        Bool(nums.sliding(2).forall { case Seq(x, y) => x < y })
    },
    "=" -> { xs => Bool(xs.forall(_ == xs.head)) },
    "!=" -> { xs => Bool(xs.forall(_ != xs.head)) },
    "not" -> {
      case Seq(Bool(value)) => Bool(!value)
    },
    "and" -> {
      case xs: Seq[Exp] => Bool(xs.asInstanceOf[Seq[Bool]].forall(_.value))
    },
    "pi" -> { _ => Float64(Math.PI) },
    "println" -> { xs => {
      println(xs.map(_.pretty()).mkString(" "))
      Nil
    }},
    "type" -> {
      case Seq(x) => x.typ
    }
  )


  /**
    * Interpret the statically typed AST
    * @param topExp Expression to evaluate
    * @param env    Symbol table
    * @return       Evaluated expression
    */

  def eval(topExp: Exp)(implicit env: Env = stdEnv): (Exp,Env) = topExp match {

    case Sym(name,_) =>
      (env(name)(Seq()), env)

    case Fn("if", _, Seq(clause, conseq, other)) =>
      val clauseVal = eval(clause)._1.asInstanceOf[Bool].value
      if (clauseVal) {
        eval(conseq)
      } else {
        eval(other)
      }

    case Fn("do",_,exps) =>
      val (expEval,_) = evalAccum(exps)
      (expEval.last, env)

    case Fn("def",_,Seq(Sym(name,_), exp)) =>
      val (expEval,_) = eval(exp)
      (Nil, env + (name -> (_ => expEval)))

    case Fn("defn",_,Seq(Sym(name,_), Fn("params",_,params: Seq[Sym]), exp)) =>
      // needs lazy for recursion
      lazy val defnFn: Seq[Exp] => Exp = args => {
        // set param symbols with arguments
        val envArgsInit = params.zip(args).foldLeft(env) {
          case (accEnv, (param, arg)) => accEnv + (param.name -> (_ => arg))
        } + (name -> defnFn) // for recursive calls

        eval(exp)(envArgsInit)._1
      }
      (Nil, env + (name -> defnFn))

    case Fn(name, _, args) =>
      val (argsEval,_) = evalAccum(args)
      (env(name)(argsEval), env)

    case exp => (exp,env)

  }

  /**
    * Evaluate multiple expressions while accumulating environment changes
    * @param exps Sequence of expressions
    * @param env  Symbol table / Environment
    * @return     (Evaluated expressions, environment with changes)
    */

  def evalAccum(exps: Seq[Exp])(implicit env: Env): (Seq[Exp],Env) = {
    exps.foldLeft((Seq[Exp](),env)) {
      case ((accTypedExp,accEnv), exp) =>
        val (expTyped, newAccEnv) = eval(exp)(accEnv)
        (accTypedExp :+ expTyped, newAccEnv)
    }
  }


}
