/**
 * The following code provides execution counts for the interpreter by
 * means of using the state monad's type in the answer type of a
 * CPS-style interpreter. The underlying paper discusses this option in
 * Section 3.3.
 *
 * This is a variation on State.hs which leverages the continuation monad
 * and uses the Answer type to plug state into the interpreter.
 */
object StateCPS {
  type N[A] = StateM.S[A]
  // type Answer = N Value
  type Answer = N[Value]

  // type M a = K a
  type M[A] = CallccMonad.K[A]

  object CallccMonad {
    type K[A] = (A => Answer) => Answer

    def unit[A](a: A): K[A] = (c) => c(a)
    def bind[A, B](m: K[A], f: A => K[B]): K[B] = (c) => m((a) => f(a)(c))
    def callcc[A, B](h: (A => K[B]) => K[A] ): K[A] = (c) => {
      h((a) => (_) => c(a))(c)
    }

    def promote[A](m: N[A]): K[A] = {
      (c) => // promoteK m = \c -> m `bindN` c
        StateM.bind(m, c)
    }

    // infix `>>=`
    implicit class BindState[A](val a: K[A]) extends AnyVal {
      def >>=[B](f: A => K[B]): K[B] = bind(a, f)
    }
  }

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

  import CallccMonad.BindState
  def tick = CallccMonad.promote(StateM.tick)
  def fetch = CallccMonad.promote(StateM.fetch)

  type Name = String
  type Environment = List[(Name, Value)]

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

  import scala.reflect.Manifest
  implicit def show(value: M[Value])(implicit m: Manifest[M[Value]]): String = {
    value(StateM.unit)
  }

  implicit def show(value: Value): String = {
    value match {
      case Num(number) => number.toString
      case Fun(_) => "<function>"
      case Wrong => "<wrong>"
    }
  }

  implicit def show(state: StateM.S[Value]): String = {
    val (a, s1) = state(StateM.state0)
    s"Value: ${a: String}; Count: $s1"
  }

  def interp(term: Term, environment: Environment): M[Value] = {
    term match {
      case Var(name) => lookup(name, environment)
      case Con(number) => CallccMonad.unit(Num(number))
      case Add(term1, term2) =>
        interp(term1, environment) >>= {
          (a: Value) => interp(term2, environment) >>= {
            (b: Value) => add(a, b)
          }
        }
      case Lam(name, term1) =>
        CallccMonad.unit(Fun((value) => interp(term1, (name, value) :: environment)))
      case App(term1, term2) =>
        interp(term1, environment) >>= {
          (f: Value) =>
            interp(term2, environment) >>= {
              (a: Value) => apply(f, a)
            }
        }
      case Count =>
        fetch >>= {
          (number) => CallccMonad.unit(Num(number))
        }
    }
  }

  def lookup(name: Name, environment: Environment): M[Value] = {
    environment match {
      case Nil => CallccMonad.unit(Wrong)
      case ((envName, value) :: env) => if (name == envName)
        CallccMonad.unit(value)
      else
        lookup(name, env)
    }
  }

  def add(value1: Value, value2: Value): M[Value] = {
    (value1, value2) match {
      case (Num(num1), Num(num2)) =>
         tick >>= { _ => CallccMonad.unit(Num(num1 + num2)) }
      case _ => CallccMonad.unit(Wrong)
    }
  }

  def apply(value1: Value, value2: Value): M[Value] = {
    value1 match {
      case Fun(fun) => tick >>= { _ => fun(value2) }
      case _ => CallccMonad.unit(Wrong)
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
