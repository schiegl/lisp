object Lang {

  sealed trait Exp {

    val typ: Typ

    def pretty(indent: Int = 2, noBreak: Boolean = true): String = this match {
      case Fn(name, typ, args) =>
        "(" + name + "::" + typ.pretty(indent + 2, noBreak = true) +
          (if (noBreak) " " else "\n" + (" " * indent)) +
          args.map(_.pretty(indent + 2, noBreak)).mkString(if (noBreak) " " else "\n" + (" " * indent)) +
          ")"
      case Typ1(name)     => name
      case Sym(name, typ) => ":" + name + "::" + typ.pretty()
      case Int64(value)   => value.toString
      case Float64(value) => value.toString
      case Text(value)    => "\"" + value.toString + "\""
      case Bool(value)    => value.toString + "::Bool"
      case _              => this.toString
    }

  }

  trait Typ extends Exp {

    /**
      * Get all super types of this type
      * @return starting with this in ascending order up to top type
      */
    def superTypes: List[Typ] =
      this :: superTypesMap.getOrElse(this, Set()).toList.flatMap(_.superTypes)

    def isSubTypeOf(otherTyp: Typ): Boolean = this match {
      case Fn("Union", _, args: Seq[Typ]) => args.forall(otherTyp.isSubTypeOf)
      case _ => this.superTypes.contains(otherTyp)
    }

  }



  /**
    * @param name of this function
    * @param typ  current return type
    * @param args arguments of the function
    */
  case class Fn(name: String, typ: Typ = TypInfer, args: Seq[Exp] = Seq()) extends Exp with Typ {
    def :+(exp: Exp): Fn = this.copy(args = args :+ exp)
  }

  object UnionTyp {
    def apply(typ1: Typ, typ2: Typ, typs: Typ*): Typ = {

      val simplifiedTyps = (typ2 +: typs).foldLeft(Vector(typ1)) {
        case (upperTyps, typ) =>
          upperTyps.span(ut => !(ut.isSubTypeOf(typ) || typ.isSubTypeOf(ut))) match {
            case (distinctTyps, similarTyp +: rest) =>
              (distinctTyps :+ (if (similarTyp.isSubTypeOf(typ)) typ else similarTyp)) ++ rest
            case (distinctTyps, _) =>
              distinctTyps :+ typ
          }

      }

      if (simplifiedTyps.length == 1)
        simplifiedTyps.head
      else
        new UnionTyp(simplifiedTyps)
    }
  }

  class UnionTyp(val typs: Seq[Typ]) extends Fn("Union", TypTyp, typs) {
    override def isSubTypeOf(otherTyp: Typ): Boolean = typs.forall(_.isSubTypeOf(otherTyp))
  }


  // a shorthand for Fn... a function that contains only types
  // these are essentially Fns but they have no input parameters therefore they will be abbreviated
  // () -> Value
  case class Typ1(name: String) extends Exp with Typ {
    val typ = TypTyp
  }
  case class Sym(name: String, typ: Typ = TypSym) extends Exp

  case class Bool(value: Boolean) extends Exp {
    val typ = TypBool
  }
  case class Float64(value: Double) extends Exp {
    val typ = TypFloat
  }
  case class Int64(value: Int) extends Exp {
    val typ = TypInt
  }
  case class Text(value: String) extends Exp {
    val typ = TypText
  }
  case object Nil extends Exp {
    val typ = TypNil
  }

  val TypTyp   = Typ1("Typ")
  val TypTop   = Typ1("Top")
  val TypNum   = Typ1("Num")
  val TypFloat = Typ1("Float")
  val TypInt   = Typ1("Int")
  val TypText  = Typ1("Text")
  val TypBool  = Typ1("Bool")
  val TypNil   = Typ1("Nil")
  val TypSym   = Typ1("Sym")
  val TypFn    = Typ1("Fn")
  val TypInfer = Typ1("?")

  val superTypesMap: Map[Typ, Set[Typ]] = Map(
    TypTop   -> Set(),
    TypTyp   -> Set(TypTop),
    TypNum   -> Set(TypTop),
    TypText  -> Set(TypTop),
    TypFloat -> Set(TypNum),
    TypInt   -> Set(TypFloat),
    TypBool  -> Set(TypTop),
    TypNil   -> Set(TypTop),
    TypSym   -> Set(TypTop)
  )

}
