/**
 * The following code provides error messages for the interpreter by
 * means of using the error monad's type in the answer type of a
 * CPS-style interpreter. The underlying paper discusses this option in
 * Section 3.3.
 *
 * This is a variation on Errors.hs which leverages the continuation
 * monad and uses the Answer type to plug errors into the interpreter.
 */
object ErrorsCPS {
  type Answer = ErrorMonad[Value]

  object CallccMonad {
    type K[A] = (A => Answer) => Answer

    def unit[A](a: A): K[A] = (c) => c(a)
    def bind[A, B](m: K[A], f: A => K[B]): K[B] = (c) => m((a) => f(a)(c))
    def callcc[A, B](h: (A => K[B]) => K[A] ): K[A] = (c) => {
      h((a) => (_) => c(a))(c)
    }

    def promote[A](m: ErrorMonad[A]): K[A] = {
      (c) => // promoteK m = \c -> m `bindN` c
        ErrorMonad.bind(m, c)
    }

    // infix `>>=`
    implicit class BindState[A](val a: K[A]) extends AnyVal {
      def >>=[B](f: A => K[B]): K[B] = bind(a, f)
    }
  }

  object ErrorMonad {
    def unit[A](a: A): ErrorMonad[A] = Success(a)
    def bind[A, B](a: ErrorMonad[A], f: A => ErrorMonad[B]): ErrorMonad[B] = a match {
      case Success(value) => f(value)
      case Error(message) => Error(message)
    }
    def fail[A](a: String): ErrorMonad[A] = Error(a)
  }

  type M[A] = CallccMonad.K[A]
  def fail[A](s: String): M[A] = CallccMonad.promote(ErrorMonad.fail(s))

  sealed trait ErrorMonad[+A]
  case class Success[A](a: A) extends ErrorMonad[A]
  case class Error(message: String) extends ErrorMonad[Nothing]

  type Name = String
  type Environment = List[(Name, Value)]

  sealed trait Term
  case class Var(name: Name) extends Term
  case class Con(integer: Int) extends Term
  case class Add(term1: Term, term2: Term) extends Term
  case class Lam(name: Name, term: Term) extends Term
  case class App(term1: Term, term2: Term) extends Term

  sealed trait Value
  case object Wrong extends Value
  case class Num(number: Int) extends Value
  case class Fun(function: Value => M[Value]) extends Value

  implicit def show(value: M[Value]): String = {
    value(ErrorMonad.unit)
  }

  implicit def show(value: Value): String = {
    value match {
      case Num(number) => number.toString
      case Fun(_) => "<function>"
      case Wrong => "<wrong>"
    }
  }

  implicit def show(value: ErrorMonad[Value]): String = {
    value match {
      case Success(v) => v
      case Error(message) => s"<error: $message>"
    }
  }


  def interp(term: Term, environment: Environment): M[Value] = {
    term match {
      case Var(name) => lookup(name, environment)
      case Con(number) => CallccMonad.unit(Num(number))
      case Add(term1, term2) =>
        val am = interp(term1, environment)
        val bm = interp(term2, environment)
        CallccMonad.bind(am, (a: Value) => CallccMonad.bind(bm, (b: Value) => add(a, b)))
      case Lam(name, term1) => CallccMonad.unit(Fun((value) => interp(term1,  (name, value) :: environment)))
      case App(term1, term2) =>
        val fm = interp(term1, environment)
        val am = interp(term2, environment)
        CallccMonad.bind(fm, (f: Value) => CallccMonad.bind(am, (a: Value) => apply(f, a)))
    }
  }

  def lookup(name: Name, environment: Environment): M[Value] = {
    environment match {
      case Nil => fail(s"unbound variable: $name")
      case ((envName: Name, value: Value) :: env) => if (name == envName) CallccMonad.unit(value) else lookup(name, env)
    }
  }

  def add(value1: Value, value2: Value): M[Value] = {
    (value1, value2) match {
      case (Num(num1), Num(num2)) => CallccMonad.unit(Num(num1 + num2))
      case _ => fail(s"should be numbers: ${show(value1)}, ${show(value2)}")
    }
  }

  def apply(value1: Value, value2: Value): M[Value] = {
    value1 match {
      case Fun(fun) => fun(value2)
      case _ => fail(s"should be function: ${show(value1)}")
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

    val termE = App(
      Con(1),
      Con(2)
    )

    println(show(interp(term42, List())))
    println(show(interp(termE, List())))
  }
}