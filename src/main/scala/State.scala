/**
 * 2.5 Variation three: State
 */

object StateM {
  type S[A] = State => (A, State)
  type State = Int
  val state0 = 0
  def unit[A](a: A): S[A] = (s0) => (a, s0)
  def bind[A, B](m: S[A], f: A => S[B]): S[B] = {
    (s0) => {
      val (a, s1) = m(s0)
      f(a)(s1)
    }
  }
  def tick: S[Unit] = (s) => (Unit, s+1)
  def fetch: S[State] = (s) => (s, s)
  // infix `>>=`
  implicit class BindState[A](val a: S[A]) extends AnyVal {
    def >>=[B](f: A => S[B]): S[B] = bind(a, f)
  }
}

object State {
  import StateM._

  type Name = String
  type Environment = List[(Name, Value)]
  type M[A] = S[A]

  sealed trait Term
  case class Var(name: Name) extends Term
  case class Con(integer: Int) extends Term
  case class Add(term1: Term, term2: Term) extends Term
  case class Lam(name: Name, term: Term) extends Term
  case class App(term1: Term, term2: Term) extends Term
  case object Count extends Term

  sealed trait Value
  case object Wrong extends Value
  case class Num(number: Int) extends Value
  case class Fun(function: Value => M[Value]) extends Value

  implicit def show(value: Value): String = {
    value match {
      case Num(number) => number.toString
      case Fun(_) => "<function>"
      case Wrong => "<wrong>"
    }
  }

  implicit def show(state: S[Value]): String = {
    val (a, s1) = state(state0)
    s"Value: ${a: String}; Count: $s1"
  }

  def interp(term: Term, environment: Environment): M[Value] = {
    term match {
      case Var(name) => lookup(name, environment)
      case Con(number) => unit(Num(number))
      case Add(term1, term2) =>
        interp(term1, environment) >>= {
          (a: Value) => interp(term2, environment) >>= {
            (b: Value) => add(a, b)
          }
        }
      case Lam(name, term1) =>
        unit(Fun((value) => interp(term1, (name, value) :: environment)))
      case App(term1, term2) =>
        interp(term1, environment) >>= {
          (f: Value) =>
            interp(term2, environment) >>= {
              (a: Value) => apply(f, a)
            }
        }
      case Count =>
        fetch >>= {
          (number) => unit(Num(number))
        }
    }
  }

  def lookup(name: Name, environment: Environment): M[Value] = {
    environment match {
      case Nil => unit(Wrong)
      case ((envName, value) :: env) => if (name == envName)
        unit(value)
      else
        lookup(name, env)
    }
  }

  def add(value1: Value, value2: Value): M[Value] = {
    (value1, value2) match {
      case (Num(num1), Num(num2)) =>
        tick >>= { _ => unit(Num(num1 + num2)) }
      case _ => unit(Wrong)
    }
  }

  def apply(value1: Value, value2: Value): M[Value] = {
    value1 match {
      case Fun(fun) => tick >>= { _ => fun(value2) }
      case _ => unit(Wrong)
    }
  }

  def test(term: Term): String = {
    show(interp(term, List()))
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

    val termS = Add(
      Add(
        Con(1),
        Con(2)
      ),
      Count
    )

    println(test(term42))
    println(test(termS))
  }
}
