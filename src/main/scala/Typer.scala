import Lang._

import scala.annotation.tailrec

/**
  * Type an AST if possible otherwise leave expression as TypInfer
  */

object Typer {

  def apply(ast: Exp, envT: EnvT = stdEnvTyp): Either[String, (Exp,EnvT)] = typeAST(ast)(envT)

  /**
    * Symbols and their types. A type may be a type function or a atomic type
    */
  type EnvT = Map[String, Typ]

  val stdEnvTyp = Map(
    "pi" -> TypFloat,
    "+" -> Fn("Fn", TypNum, Stream.continually(TypNum)),
    "-" -> Fn("Fn", TypNum, Stream.continually(TypNum)),
    "*" -> Fn("Fn", TypNum, Stream.continually(TypNum)),
    "/" -> Fn("Fn", TypNum, Stream.continually(TypNum)),
    ">" -> Fn("Fn", TypBool, Stream.continually(TypNum)),
    "<" -> Fn("Fn", TypBool, Stream.continually(TypNum)),
    "=" -> Fn("Fn", TypBool, Stream.continually(TypTop)),
    "!=" -> Fn("Fn", TypBool, Stream.continually(TypTop)),
    "not" -> Fn("Fn", TypBool, Seq(TypBool)),
    "and" -> Fn("Fn", TypBool, Stream.continually(TypTop)),
    "println" -> Fn("Fn", TypNil, Stream.continually(TypTop)),
    "type" -> Fn("Fn", TypTyp, Seq(TypTop))
  )

  /**
    * Types an AST recursively. TypMe annotations will be inferred
    * @param topExp parent expression
    * @param env    type environment
    * @return       (typed topExp, updated env)
    */
  def typeAST(topExp: Exp)(implicit env: EnvT = stdEnvTyp): Either[String, (Exp,EnvT)] = topExp match {

    // resolve symbol
    case sym@Sym(name,TypSym) => env.get(name) match {
      case Some(typ) => Right((Sym(name,typ), env))
      case None      => Left(s"Failed to resolve $name")
    }

    case ifFn@Fn("if", _, Seq(clause, conseq, other)) =>
      typeAccum(Seq(clause, conseq, other)) match {
        case Right((Seq(clauseTyped, conseqEval, otherEval), accEnv)) =>
          clauseTyped.typ match {
            case TypBool =>

              // TODO: Duplication...
              val conseqTyped = conseqEval match {
                case fn@Fn(_,TypInfer,_) => fn.copy(typ = accEnv.getOrElse(fn.name,TypInfer))
                case sym@Sym(_,TypInfer) => sym.copy(typ = accEnv.getOrElse(sym.name,TypInfer))
                case exp => exp
              }
              val otherTyped = otherEval match {
                case fn@Fn(_,TypInfer,_) => fn.copy(typ = accEnv.getOrElse(fn.name,TypInfer))
                case sym@Sym(_,TypInfer) => sym.copy(typ = accEnv.getOrElse(sym.name,TypInfer))
                case exp => exp
              }

              val ifUnion = UnionTyp(conseqTyped.typ, otherTyped.typ)
              val ifFn = Fn("if", ifUnion, Seq(clauseTyped, conseqTyped, otherTyped))
              Right((ifFn, accEnv))
            case _ =>
              Left(s"${clause.pretty()} is not of type Bool")
          }
        case Left(errs) => Left(errs)
      }


    case Fn("do", _, args) => for {
      res <- typeAccum(args)
      (expsTyped, accEnv) = res
    } yield (Fn("do", expsTyped.last.typ, expsTyped), accEnv)

    // TODO: proper error handling
    // TODO: parse return type
    case Fn("defn", _, Seq(Sym(name,TypSym), Fn("params",_,params: Seq[Exp]), exp)) =>
      typeDefn(name, params, exp)

    case Fn("def", _, Seq(Sym(name,TypSym), exp)) =>
      typeDef(name, exp)

    case fn@Fn(_, retTypeActual, _) => for {

      _ <- if (env.contains(fn.name)) Right() else Left(s"Failed to resolve ${fn.name}")

      res <- typeAccum(fn.args)
      (argsEvaled, envAfterArgEval) = res

      fnTyp@Fn(_,retTypeEnv,paramTyps: Seq[Typ]) = envAfterArgEval(fn.name)

      argCountErr <- if (paramTyps.hasDefiniteSize && argsEvaled.length != paramTyps.length)
        Left(s"${fn.name} expects ${paramTyps.length} arguments, not ${argsEvaled.length} in ${fn.pretty()}") else Right()

      (argsInf, paramTypsInf, envInf) = infer(argsEvaled, paramTyps)(envAfterArgEval)

      maybeTypeMismatch = argsInf.zip(paramTypsInf).find {
        case (arg,paramTyp) => !arg.typ.isSubTypeOf(paramTyp)
      }

      res2 <- maybeTypeMismatch match {
        case Some((actualArg, expectedTyp)) =>
          Left(s"${fn.name} expects ${expectedTyp.pretty()} instead of ${actualArg.typ.pretty()} in ${fn.pretty()}")
        case _ =>
          val retType = if (retTypeActual != TypInfer) retTypeActual else retTypeEnv
          Right((fn.copy(typ = retType, args = argsInf), envInf + (fn.name -> fnTyp.copy(typ = retType, args = paramTypsInf))))
      }

    } yield res2

    case exp =>
      Right((exp,env))
  }

  /**
    * Infer the type of the arguments or parameters (if the argument gets typed in the scope somewhere else)
    * of a function
    *
    * @param args      Sequence of expressions
    * @param paramTyps Parameter types (in respective order of args)
    * @param env       Type environment
    * @return          (Maybe typed args, Maybe typed parameter types, updated type env)
    */
  def infer(args: Seq[Exp], paramTyps: Seq[Typ])(implicit env: EnvT): (Seq[Exp],Seq[Typ],EnvT) = {

    args.zip(paramTyps).foldLeft((Seq[Exp](), Seq[Typ](), env)) {

      case ((accArgs, accParamTyps, accEnv), tuple) =>

        val (argInf, paramTypInf, envUpdated) = tuple match {

          case (Sym(name, TypInfer), paramTyp) =>
            if (paramTyp != TypInfer) {
              (Sym(name, paramTyp), paramTyp, updateIfNoneOrToInfer(accEnv, name, paramTyp))
            } else {
              // both types unknown maybe env knows it
              val downUnderTyp = accEnv.getOrElse(name, TypInfer)
              (Sym(name, downUnderTyp), downUnderTyp, accEnv)
            }
          case (Fn(name, TypInfer, args), paramTyp) =>
            // TODO: Make type more specific with knowledge from down under?
            if (paramTyp != TypInfer)
              (Fn(name, paramTyp, args), paramTyp, updateIfNoneOrToInfer(accEnv, name, paramTyp))
            else {
              // both types unknown maybe env knows it
              val downUnderTyp = accEnv.getOrElse(name, TypInfer)
              (Fn(name, downUnderTyp, args), downUnderTyp, accEnv)
            }
          case (arg, TypInfer) =>
            (arg, arg.typ, accEnv)

          case (arg, paramTyp) =>
            (arg, paramTyp, accEnv)

        }

        (accArgs :+ argInf, accParamTyps :+ paramTypInf, envUpdated)
    }
  }

  def updateIfNoneOrToInfer(env: EnvT, name: String, typ: Typ): EnvT = {
    env.get(name) match {
      case Some(TypInfer) | None =>
        env + (name -> typ)
      case _ => env
    }
  }


  /**
    * Type defn declaration and add it to environment.
    * If not all parameters are typed it will try to infer them.
    * @param name   name of the new function
    * @param params params expression
    * @param exp    actual function or value
    * @param env    type environment
    * @return       (typed defn, env + (fun -> exp))
    */
  def typeDefn(name: String, params: Seq[Exp], exp: Exp)(implicit env: EnvT): Either[String,(Exp,EnvT)] = for {

    _ <- if (env.contains(name)) Left(s"$name already defined") else Right()
    paramSyms <- parseParams(params)

    // add all parameters + the function itself to defn scope
    envParamsUntyped = paramSyms.foldLeft(env) {
      case (accEnv, param) => accEnv + (param.name -> param.typ)
    } + (name -> Fn("Fn", TypInfer, paramSyms.map(_.typ)))

    // get typs for params
    res <- typeAST(exp)(envParamsUntyped)
    (expTyped, envParamsTyped) = res
    // take typed symbols from exp scope and apply to this scope
    paramsTyped  = paramSyms.map(p => p.copy(typ = envParamsTyped(p.name)))
    defnTyped    = Fn("defn", TypNil, Seq(Sym(name,TypSym), Fn("params", TypNil, paramsTyped), expTyped))
    envAfterDefn = env + (name -> Fn("Fn", expTyped.typ, paramsTyped.map(_.typ)))

  } yield (defnTyped, envAfterDefn)


  /**
    * Type a def expression and add it to the environment
    * @param name name of expression
    * @param exp  expression that name will resolve to
    * @param env  type environment
    * @return     (typed def expression, env + (name -> exp))
    */

  def typeDef(name: String, exp: Exp)(implicit env: EnvT): Either[String, (Exp,EnvT)] = for {
    _ <- if (env.contains(name)) Left(s"$name already defined") else Right()
    res <- typeAST(exp)
    (typedExp, envUpdated) = res
    typedDef = Fn("def", TypNil, Seq(Sym(name, TypSym), typedExp))
    envUpdatedWithDef = envUpdated + (name -> typedExp.typ)
  } yield (typedDef, envUpdatedWithDef)


  /**
    * Type multiple expressions and accumulate environment updates
    * @param exps sequence of expressions to type
    * @param env  environment that will be updated
    * @return     (typed expressions, updated environment)
    */
  def typeAccum(exps: Seq[Exp])(implicit env: EnvT): Either[String,(Seq[Exp],EnvT)] = {
    exps.foldLeft(Right((Seq[Exp](),env)): Either[String,(Seq[Exp],EnvT)]) {
      case (Right((accTypedExp,accEnv)), exp) => for {
        res <- typeAST(exp)(accEnv)
        (expTyped, newAccEnv) = res
      } yield (accTypedExp :+ expTyped, newAccEnv)
      // in case of error the rest of the list will still be traversed
      case (Left(err),next) =>
        Left(err)
    }
  }


  /**
    * Parse defn arguments. Arguments without type annotation will be assigned TypMe
    * @param args   defn parameters
    * @param parsed result accumulation
    * @return       seq of typed symbols/TypMe symbols
    */

  @tailrec
  def parseParams(args: Seq[Exp], parsed: Seq[Sym] = Vector()): Either[String,Seq[Sym]] = args match {
    case Sym(name,_) +: Typ1(typName) +: rest =>
      parseParams(rest, parsed :+ Sym(name, Typ1(typName)))
    case Sym(name,_) +: Fn(typFn, _, typArgs) +: rest =>
      parseParams(rest, parsed :+ Sym(name, Fn(typFn, TypTyp, typArgs)))

    case typMe @ Sym(name,_) +: Sym(nextName,nextTyp) +: rest =>
      parseParams(Sym(nextName,nextTyp) +: rest, parsed :+ Sym(name, TypInfer))
    case typMe @ Sym(name,_) +: rest =>
      parseParams(rest, parsed :+ Sym(name,TypInfer))

    case Seq() =>
      val maybeDuplicate = parsed.groupBy(_.name).find {
        case (name, duplicateArgs) => duplicateArgs.size != 1
      }
      maybeDuplicate match {
        case Some(dup) =>
          Left(s"Failed to parse arguments: ${dup._1} used more than once")
        case _ =>
          Right(parsed)
      }

    case _ =>
      Left(s"Failed to parse parameters: $args")
  }

}
