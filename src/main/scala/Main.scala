import Expr.*

object Main extends App {
  /*
   * x = 5
   * x + {
   *   let y = 7
   *     y + 9
   * }
   * */

  /*
  f => fun x = if (x = 10) x  else f(x + 1)
   */
  val expr     = Add(Num(1), Add(Num(3), Num(4)))
  val exprBool = Cond(Bool(true), expr, Num(0))
  val let      = Let("x", Num(2), Expr.Add(Ref("x"), Num(2)))
  val fun      = Expr.Lambda("x", Add(Expr.Ref("x"), Num(1)))
  val letFun   = Let("f", fun, Apply(Expr.Ref("f"), Num(2)))
  // f = fun x => if(f(x + 1) == 10) x + 1 else x
  val fun1 = Lambda("x", Cond(Eq(Ref("x"), Num(10)), Ref("x"), Expr.Apply(Ref("f"), Add(Expr.Ref("x"), Expr.Num(1)))))
  val letFunRec = Let("f", fun1, Apply(Ref("f"), Expr.Num(0)))
  val env       = Env.empty
  println(pprint(letFunRec))
  println(Value.pprint(interpret(letFunRec, env)))
}
