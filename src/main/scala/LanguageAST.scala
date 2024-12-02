import scala.collection.mutable

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
  case LetRec(name: String, value: Expr, body: Expr)

case class Env(values: mutable.Map[String, Value]):
  def bind(name: String, value: Value): Env = copy(values.updated(name, value))

  def set(name: String, value: Value): Unit = values.update(name, value)
  
  def lookup(name: String): Value = values.get(name) match
    case Some(value) => value
    case None        => sys.error(s"binding not found: $name")

object Env:
  def empty: Env = Env(mutable.Map.empty[String, Value])
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
      case LetRec(name, value, body)   => s"let rec $name = ${pprint(value)} in ${pprint(body)}"
      /*
      let y = 1 in
        let f = fun x => x + y in
          let y = 2 in
            f 3
       */

  def interpret(in: Expr, env: Env): Value =
    in match
      case Expr.Num(i)                             => Value.Num(i)
      case Expr.Bool(b)                            => Value.Bool(b)
      case Expr.Lambda(name, body)                 => Value.Lambda(name, body, env)
      case Expr.Add(lhs, rhs)                      => add(lhs, rhs, env)
      case Expr.Sub(lhs, rhs)                      => sub(lhs, rhs, env)
      case Expr.Cond(pred, thenBranch, elseBranch) => cond(pred, thenBranch, elseBranch, env)
      case Expr.Eq(lhs, rhs)                       => eq(lhs, rhs, env)
      case Expr.Ref(name)                          => env.lookup(name)
      case Expr.Let(name, value, body)             => let(name, value, body, env)
      case Expr.Apply(func, arg)                   => apply(func, arg, env)
      case Expr.LetRec(name, value, body)          => letRec(name, value, body, env)

  def let(name: String, value: Expr, body: Expr, env: Env): Value =
    val v      = interpret(value, env)
    val newEnv = env.bind(name, v)
    interpret(body, newEnv)

  def letRec(name: String, value: Expr, body: Expr, env: Env): Value =
    val newEnv = env.bind(name, null)
    val v      = interpret(value, newEnv)
    newEnv.set(name, v)
    interpret(body, newEnv)

  def apply(function: Expr, arg: Expr, env: Env): Value =
    interpret(function, env) match
      case Value.Lambda(param, body, env2) =>
        val a      = interpret(arg, env)
        val newEnv = env2.bind(param, a)
        interpret(body, newEnv)
      case other => sys.error(s"${Value.pprint(other)} is not a function")

  def eq(lhs: Expr, rhs: Expr, env: Env): Value =
    (interpret(lhs, env), interpret(rhs, env)) match
      case (Value.Num(a), Value.Num(b)) => Value.Bool(a == b)
      case _                            => sys.error(s"Invalid types to compare ${pprint(lhs)} and ${pprint(rhs)}")

  def cond(pred: Expr, thenBranch: Expr, elseBranch: Expr, env: Env): Value =
    interpret(pred, env) match
      case Value.Bool(true)  => interpret(thenBranch, env)
      case Value.Bool(false) => interpret(elseBranch, env)
      case _                 => sys.error(s"Expected pred to be Bool got ${pprint(pred)}")

  def sub(lhs: Expr, rhs: Expr, env: Env): Value =
    (interpret(lhs, env), interpret(rhs, env)) match
      case (Value.Num(l), Value.Num(r)) => Value.Num(l - r)
      case _                            => sys.error(s"type error in sub, expected Num & Num, got $lhs, $rhs")

  def add(lhs: Expr, rhs: Expr, env: Env): Value =
    (interpret(lhs, env), interpret(rhs, env)) match
      case (Value.Num(l), Value.Num(r)) => Value.Num(l + r)
      case _                            => sys.error(s"type error in add, expected Num & Num, got $lhs, $rhs")
