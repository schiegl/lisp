import Interpreter.Env
import Lang.Exp
import Typer.EnvT

object REPL {

  def run(source: String = "",
          history: List[Exp] = List(),
          env: Env = Interpreter.stdEnv,
          envT: EnvT = Typer.stdEnvTyp,
          showAST: Boolean = false
         ): Unit =

    source match {

      case ":q" =>
        println("Bye...")
      case ":ast" =>
        println(if (!showAST) "Showing AST" else "Hiding AST")
        run(history = history, env = env, envT = envT, showAST = !showAST)
      case ""   =>
        run(io.StdIn.readLine("> "), history, env, envT, showAST)
      case _ =>
        Interpreter(source, env = env, envT = envT) match {
          case Right((result, envRes, astTyped, envTRes)) =>
            if (showAST)
              println(astTyped.pretty(noBreak = false) + "\n")
            println(result.pretty())
            run(io.StdIn.readLine("> "), result :: history, envRes, envTRes, showAST)

          case Left(err) =>
            println(err)
            run(io.StdIn.readLine("> "), history, env, envT)
        }

  }

}
