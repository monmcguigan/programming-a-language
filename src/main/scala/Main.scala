import Expr._

object Main extends App {

  /* let y = 1 in
      let f = fun x => x + y in
        let z = 2 in
          f(z) // env => y = 1, x = 1

   *
   * */
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

//  let sum = fun lower
//  => fun upper
//  =>
//  if lower > upper then 0
//  else lower + sum(lower + 1)(upper)
//  in sum (1)(10)

// trying to do recursive functions using our let statements
// think we have a problem with shadowing and trying to apply a function in a different
// env to what it was introduced in
  val expr     = Add(Num(1), Add(Num(3), Num(4)))
  val exprBool = Cond(Bool(true), expr, Num(0))
  val let      = Let("x", Num(2), Expr.Add(Ref("x"), Num(2)))
  val fun      = Expr.Lambda("x", Add(Expr.Ref("x"), Num(1)))
  val letFun   = Let("f", fun, Apply(Expr.Ref("f"), Num(2)))
  // f = fun x => if(f(x + 1) == 10) x + 1 else x
  val fun1 = Lambda("x", Cond(Eq(Ref("x"), Num(10)), Ref("x"), Expr.Apply(Ref("f"), Add(Expr.Ref("x"), Expr.Num(1)))))
  val letFunRec = Let("f", fun1, Apply(Ref("f"), Expr.Num(0)))
  val env       = Env.empty

  val thing = LetRec(
    name = "sum",
    value = Expr.Lambda(
      param = "lower",
      body = Expr.Lambda(
        param = "upper",
        body = Expr.Cond(
          pred = Eq(Ref("lower"), Ref("upper")),
          thenBranch = Expr.Ref("upper"),
          elseBranch = Add(
            lhs = Ref("lower"),
            rhs = Expr.Apply(
              func = Apply(Ref("sum"), Expr.Add(Ref("lower"), Expr.Num(1))),
              arg = Ref("upper")
            )
          )
        )
      )
    ),
    body = Expr.Apply(Apply(Expr.Ref("sum"), Expr.Num(1)), Expr.Num(10))
  )

  val envCheck =
    Let(
      "y",
      Num(1),
      Let(
        "f",
        Expr.Lambda("x", Add(Expr.Ref("x"), Expr.Ref("y"))),
        Let("z", Num(2), Expr.Apply(Expr.Ref("f"), Expr.Ref("z")))
      )
    )

  

  println(pprint(envCheck))
  println(Value.pprint(interpret(envCheck, env)))
}

// case for needing stack env rather than map env
// let rec sum = lower -> upper -> let foo = 1 in
//  if lower > upper then 0
//  else                  lower + (sum (lower + 1) upper)
//  in sum 1 10
