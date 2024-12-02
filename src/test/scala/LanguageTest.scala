import org.scalatest.freespec.AnyFreeSpec
import Expr._
class LanguageTest extends AnyFreeSpec {
  "static scoping" in {
    // let x = 1 in
    //    let f = a => a + x in
    //       let x = 2 in
    //         f(1) // expected = 2
    val expr =
      Expr.Let("x", Num(1),
        Let("f", Lambda("a", Add(Ref("a"), Ref("x"))),
          Let("x", Num(2),
            Apply(Ref("f"), Num(1))
          )
        )
      )
    val expected = Value.Num(2)
    val interpreted = interpret(expr, Env.empty)
    assert(interpreted == expected)

  }
  "static scoping 2" in {
    // let x = 1 in
    //    let f = a => a + x in
    //       let x = 2 in
    //          let z = f(1) in
    //            z // expected = 2
    val expr =
      Expr.Let("x", Num(1),
        Let("f", Lambda("a", Add(Ref("a"), Ref("x"))),
          Let("x", Num(2),
            Let("z", Apply(Ref("f"), Num(1)),
              Ref("z")
            )
          )
        )
      )
    val expected = Value.Num(2)
    val interpreted = interpret(expr, Env.empty)
    assert(interpreted == expected)

  }
  "let rec" in {
    val brokenLetRec = LetRec(
      name = "sum",
      value = Expr.Lambda(
        param = "lower",
        body = Expr.Lambda(
          param = "upper",
          body = Let("foo", Num(1), Expr.Cond(
            pred = Eq(Ref("lower"), Ref("upper")),
            thenBranch = Expr.Ref("upper"),
            elseBranch = Add(
              lhs = Ref("lower"),
              rhs = Expr.Apply(
                func = Apply(Ref("sum"), Expr.Add(Ref("lower"), Ref("foo"))),
                arg = Ref("upper")
              )
            )
          ))
        )
      ),
      body = Expr.Apply(Apply(Expr.Ref("sum"), Expr.Num(1)), Expr.Num(10))
    )
    val interpreted = interpret(brokenLetRec, Env.empty)
    println(interpreted)
    val expected = Value.Num(55)
    assert(interpreted == expected)
    // let rec sum = lower -> upper -> let foo = sum in
    //  if lower > upper then 0
    //  else                  lower + (sum (lower + 1) upper)
    //  in sum 1 10
  }
  "testing env" in {
    val env = Env.empty
    val newEnv = env.bind("x", null)
    println(newEnv)
    val newEnv2 = newEnv.bind("y", Value.Num(3))
    newEnv.set("x", Value.Num(2))
    println(newEnv)
    println(newEnv2)
  }

}
