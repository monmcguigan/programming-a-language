enum Expr:
  case Num(i: Int)
  case Bool(b: Boolean)
  case Add(lhs: Expr, rhs: Expr)
  case Sub(lhs: Expr, rhs: Expr)
  case Cond(pred: Expr, thenBranch: Expr, elseBranch: Expr)

enum Value:
  case Num(i: Int)

  case Bool(b: Boolean)

object Value:
  def pprint(in: Value): String =
    in match
      case Value.Num(i)  => i.toString
      case Value.Bool(b) => b.toString

object Expr:
  def pprint(in: Expr): String =
    in match
      case Expr.Num(i)        => i.toString
      case Expr.Bool(b)       => b.toString
      case Expr.Add(lhs, rhs) => s"${pprint(lhs)} + ${pprint(rhs)}"
      case Expr.Sub(lhs, rhs) => s"${pprint(lhs)} - ${pprint(rhs)}"
      case Expr.Cond(pred, thenBranch, elseBranch) =>
        s"if ${pprint(pred)} then ${pprint(thenBranch)} else ${pprint(elseBranch)}"

  def interpret(in: Expr): Value =
    in match
      case Expr.Num(i)  => Value.Num(i)
      case Expr.Bool(b) => Value.Bool(b)
      case Expr.Add(lhs, rhs) =>
        (interpret(lhs), interpret(rhs)) match
          case (Value.Num(l), Value.Num(r)) => Value.Num(l + r)
          case _                            => sys.error(s"type error in add, expected Num & Num, got $lhs, $rhs")
      case Expr.Sub(lhs, rhs) =>
        (interpret(lhs), interpret(rhs)) match
          case (Value.Num(l), Value.Num(r)) => Value.Num(l - r)
          case _                            => sys.error(s"type error in sub, expected Num & Num, got $lhs, $rhs")
      case Expr.Cond(pred, thenBranch, elseBranch) =>
        interpret(pred) match
          case Value.Bool(true)  => interpret(thenBranch)
          case Value.Bool(false) => interpret(elseBranch)
          case _                 => sys.error(s"Expected pred to be Bool got ${pprint(pred)}")
