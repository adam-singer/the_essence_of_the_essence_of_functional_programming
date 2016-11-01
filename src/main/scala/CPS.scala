/**
 * The following code corresponds to Fig. 4 (Interpreter in CPS). We had
 * to gather all the missing features. One detail is that the Value type
 * also had to be updated so that the Fun constructor is typed in CPS as
 * well.
 *
 * This is a non-monadic, CBV and CPS interpreter.
 */

object CPS {
  type Name = String
  type Environment = List[(Name, Value)]

  sealed trait Term
  case class Var(name: Name) extends Term
  case class Con(integer: Int) extends Term
  case class Add(term1: Term, term2: Term) extends Term
  case class Lam(name: Name, term: Term) extends Term
  case class App(term1: Term, term2: Term) extends Term

  type Answer = Value

  sealed trait Value
  case object Wrong extends Value
  case class Num(number: Int) extends Value
  // Fun (Value -> (Value -> Answer) -> Answer)
  case class Fun(function: Value => (Value => Answer) => Answer) extends Value


  implicit def show(value: Value): String = {
    value match {
      case Num(number) => number.toString
      case Fun(_) => "<function>"
      case Wrong => "<wrong>"
    }
  }

  // TODO(adams): could type alias `(Value, Value => Answer) => Answer`
  //  type Continuation = (Value => Answer) => Answer //  suspended computation: a function with general type (a -> r) -> r
  //  type FunctionContinuation = Value => Continuation

  def interp(term: Term, environment: Environment): (Value => Answer) => Answer = {
    term match {
      case Var(name) => (c: Value => Answer) => lookup(name, environment)(c)
      case Con(number) => (c: Value => Answer) => c(Num(number))
      case Add(term1, term2) => (c: Value => Answer) =>
        interp(term1, environment)(
          (a) => interp(term2, environment)(
            (b) => add(a, b)(c)
          )
        )
      case Lam(name, term1) => (c: Value => Answer) =>
        c(Fun(
          (a) => interp(term1, (name, a) :: environment)
        ))
      case App(term1, term2) => (c: Value => Answer) =>
        interp(term1, environment)((f) => interp(term2, environment)((a) => apply(f, a)(c)))
    }
  }

  def lookup(name: Name, environment: Environment): (Value => Answer) => Answer = {
    environment match {
      case Nil => (c: Value => Answer) => c(Wrong)
      case ((envName: Name, value: Value) :: env) =>
        (c: Value => Answer) => if (name == envName) c(value) else lookup(name, env)(c)
    }
  }

  def add(value1: Value, value2: Value): (Value => Answer) => Answer  = {
    (value1, value2) match {
      case (Num(num1), Num(num2)) => (c: Value => Answer) => c(Num(num1 + num2)) // returnM(Num(num1 + num2))
      case _ => (c: Value => Answer) => c(Wrong)
    }
  }

  def apply(value1: Value, value2: Value): (Value => Answer) => Answer = {
    value1 match {
      case Fun(fun) => (c: Value => Answer) => fun(value2)(c)
      case _ => (c: Value => Answer) => c(Wrong)
    }
  }

  def main(args: Array[String]): Unit = {
    val term42 = App(
      Lam("x",
        Add(
          Var("x"),
          Var("x")
        )
      ),
      Add(
        Con(10),
        Con(11)
      )
    )

    val answerFunction: (Value => Answer) => Answer = interp(term42, List())
    val answer = answerFunction(identity)
    println(answer: String)
  }
}