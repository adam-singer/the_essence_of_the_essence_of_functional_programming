// 2.3 Variation One: Error Messages

object Errors {
//  return :: a -> M a
//  (>>=) :: M a -> (a -> M b) -> M b
//  fail :: String -> M a
//  trait Monad[A] {
//    def unit[A](a: A): Monad[A]
//    def bind[A, B](a: Monad[A], f: A => Monad[B]): Monad[B]
//    def fail(a: String): Monad[A]
//  }

  object ErrorMonad {
    def unit[A](a: A): ErrorMonad[A] = Success(a)
    def bind[A, B](a: ErrorMonad[A], f: A => ErrorMonad[B]): ErrorMonad[B] = a match {
      case Success(value) => f(value)
      case Error(message) => Error(message)
    }
    def fail[A](a: String): ErrorMonad[A] = Error(a)
  }

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
  case class Fun(function: Value => ErrorMonad[Value]) extends Value

  implicit def show(value: Value): String = {
    value match {
      case Num(number) => number.toString
      case Fun(_) => "<function>"
      case Wrong => "<wrong>"
    }
  }

  implicit def showError(value: ErrorMonad[Value]): String = {
    value match {
      case Success(value) => value
      case Error(message) => s"<error: $message>"
    }
  }

  def interp(term: Term, environment: Environment): ErrorMonad[Value] = {
    term match {
      case Var(name) => lookup(name, environment)
      case Con(number) => ErrorMonad.unit(Num(number))
      case Add(term1, term2) =>
        val am = interp(term1, environment)
        val bm = interp(term2, environment)
        ErrorMonad.bind(am, (a: Value) => ErrorMonad.bind(bm, (b: Value) => add(a, b)))
      case Lam(name, term1) => ErrorMonad.unit(Fun((value) => interp(term1,  (name, value) :: environment)))
      case App(term1, term2) =>
        val fm = interp(term1, environment)
        val am = interp(term2, environment)
        ErrorMonad.bind(fm, (f: Value) => ErrorMonad.bind(am, (a: Value) => apply(f, a)))
    }
  }

  def lookup(name: Name, environment: Environment): ErrorMonad[Value] = {
    environment match {
      case Nil => ErrorMonad.fail(s"unbound variable: $name")
      case ((envName: Name, value: Value) :: env) => if (name == envName) ErrorMonad.unit(value) else lookup(name, env)
    }
  }

  def add(value1: Value, value2: Value): ErrorMonad[Value] = {
    (value1, value2) match {
      case (Num(num1), Num(num2)) => ErrorMonad.unit(Num(num1 + num2))
      case _ => ErrorMonad.fail(s"should be numbers: ${show(value1)}, ${show(value2)}")
    }
  }

  def apply(value1: Value, value2: Value): ErrorMonad[Value] = {
    value1 match {
      case Fun(fun) => fun(value2)
      case _ => ErrorMonad.fail(s"should be function: ${show(value1)}")
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

    println(showError(interp(term42, List())))
    println(showError(interp(termE, List())))
  }

}