import Expr._

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
  println(pprint(exprBool))
  println(Value.pprint(interpret(exprBool)))
}
