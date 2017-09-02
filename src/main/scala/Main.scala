object Main {

  def main(args: Array[String]): Unit = {
    // REPL.run()
    someScripts()
  }

  def someScripts(): Unit = {

    val paths = List(
      "src/main/resources/fac.lisp",
      "src/main/resources/fib.lisp",
      "src/main/resources/logic.lisp",
      "src/main/resources/arg-type-err.lisp",
      "src/main/resources/param-count-err.lisp"
    )

    for (path <- paths) {
      println(path.split("/").last)
      // println(Parser.fromFile(path))
      Interpreter.fromFile(path) match {
        case Right((result, env, astTyped, envT)) =>
          println(astTyped.pretty(noBreak = false))
          println(result.pretty())
        case Left(err) =>
          println(err)

      }
      println()
    }

  }

}
