import Expr.*

object Main extends App {
  /*
   * x = 5
   * x + {
   *   let y = 7
   *     y + 9
   * }
   * */

  val expr     = Add(Num(1), Add(Num(3), Num(4)))
  val exprBool = Cond(Bool(true), expr, Num(0))
  val let      = Let("x", Num(2), Expr.Add(Ref("x"), Num(2)))
  val fun      = Expr.Lambda("x", Add(Expr.Ref("x"), Num(1)))
  val letFun   = Let("f", fun, Ref("f"))

  val env = Env.empty
  println(pprint(letFun))
  println(Value.pprint(interpret(letFun, env)))
}
