enum Expr:
  case Num(i: Int)
  case Bool(b: Boolean)
  case Add(lhs: Expr, rhs: Expr)
  case Sub(lhs: Expr, rhs: Expr)
  case Cond(pred: Expr, thenBranch: Expr, elseBranch: Expr)
  case Let(name: String, value: Expr, body: Expr)
  case Ref(name: String)
  case Lambda(param: String, body: Expr)
  case Apply(func: Expr, arg: Expr)
  case Eq(lhs: Expr, rhs: Expr)

//  case LetRec()

case class Env(values: Map[String, Value]):
  def bind(name: String, value: Value): Env = copy(values.updated(name, value))
  def lookup(name: String): Value = values.get(name) match
    case Some(value) => value
    case None        => sys.error(s"binding not found: $name")

object Env:
  def empty: Env = Env(Map.empty[String, Value])
enum Value:
  case Num(i: Int)

  case Bool(b: Boolean)

  case Lambda(param: String, body: Expr, env: Env)

object Value:
  def pprint(in: Value): String =
    in match
      case Value.Num(i)          => i.toString
      case Value.Bool(b)         => b.toString
      case Lambda(name, body, _) => s"fun $name => ${Expr.pprint(body)}"

object Expr:
  def pprint(in: Expr): String =
    in match
      case Expr.Num(i)        => i.toString
      case Expr.Bool(b)       => b.toString
      case Expr.Add(lhs, rhs) => s"${pprint(lhs)} + ${pprint(rhs)}"
      case Expr.Sub(lhs, rhs) => s"${pprint(lhs)} - ${pprint(rhs)}"
      case Expr.Cond(pred, thenBranch, elseBranch) =>
        s"if ${pprint(pred)} then ${pprint(thenBranch)} else ${pprint(elseBranch)}"
      case Expr.Let(name, value, body) => s"let $name = ${pprint(value)} in ${pprint(body)}"
      case Expr.Ref(name)              => name
      case Lambda(param, body)         => s"fun $param => ${pprint(body)}"
      case Apply(func, arg)            => s"${pprint(func)} (${pprint(arg)})"
      case Eq(lhs, rhs)                => s"${pprint(lhs)} == ${pprint(rhs)}"

      /*
      let y = 1 in
        let f = fun x => x + y in
          let y = 2 in
            f 3
       */

  def interpret(in: Expr, env: Env): Value =
    in match
      case Expr.Num(i)  => Value.Num(i)
      case Expr.Bool(b) => Value.Bool(b)
      case Expr.Add(lhs, rhs) =>
        (interpret(lhs, env), interpret(rhs, env)) match
          case (Value.Num(l), Value.Num(r)) => Value.Num(l + r)
          case _                            => sys.error(s"type error in add, expected Num & Num, got $lhs, $rhs")
      case Expr.Sub(lhs, rhs) =>
        (interpret(lhs, env), interpret(rhs, env)) match
          case (Value.Num(l), Value.Num(r)) => Value.Num(l - r)
          case _                            => sys.error(s"type error in sub, expected Num & Num, got $lhs, $rhs")
      case Expr.Cond(pred, thenBranch, elseBranch) =>
        interpret(pred, env) match
          case Value.Bool(true)  => interpret(thenBranch, env)
          case Value.Bool(false) => interpret(elseBranch, env)
          case _                 => sys.error(s"Expected pred to be Bool got ${pprint(pred)}")
      case Expr.Let(name, value, body) =>
        val newEnv = env.bind(name, interpret(value, env))
        interpret(body, newEnv)
      case Expr.Ref(name)          => env.lookup(name)
      case Expr.Lambda(name, body) => Value.Lambda(name, body, env)
      case Expr.Apply(func, arg) =>
        interpret(func, env) match
          case Value.Lambda(param, body, env2) =>
            val newEnv = env.bind(param, interpret(arg, env))
            interpret(body, newEnv)
          case other => sys.error(s"${Value.pprint(other)} is not a function")

      case Expr.Eq(lhs, rhs) =>
        (interpret(lhs, env), interpret(rhs, env)) match
          case (Value.Num(a), Value.Num(b)) => Value.Bool(a == b)
          case _                            => sys.error(s"Invalid types to compare ${pprint(lhs)} and ${pprint(rhs)}")

// (if pred then fun x => x + 1 else fun x => x + 2)(3)
