/**
 * The following code exercises Section 3.2 of the underlying article.
 * That section is concerned with call with current continuation.
 *
 * This is a monadic-style interpreter (CBV) that uses the
 * continuation monad to provide "call with current continuation"
 * (callcc). We perform a data extension to provide callcc as a
 * language construct within the interpreted language.
 */

object CallccM {
  type K[A] = (A => Answer) => Answer

  def unit[A](a: A): K[A] = (c) => c(a)
  def bind[A, B](m: K[A], f: A => K[B]): K[B] = (c) => m((a) => f(a)(c))
  def callcc[A, B](h: (A => K[B]) => K[A] ): K[A] = (c) => {
    h((a) => (_) => c(a))(c)
  }
  // infix `>>=`
  implicit class BindState[A](val a: K[A]) extends AnyVal {
    def >>=[B](f: A => K[B]): K[B] = bind(a, f)
  }

  type Answer = Value

  type Name = String
  type Environment = List[(Name, Value)]
  type M[A] = K[A]

  sealed trait Term
  case class Var(name: Name) extends Term
  case class Con(integer: Int) extends Term
  case class Add(term1: Term, term2: Term) extends Term
  case class Lam(name: Name, term: Term) extends Term
  case class App(term1: Term, term2: Term) extends Term
  case class Callcc(name: Name, term: Term) extends Term

  sealed trait Value
  case object Wrong extends Value
  case class Num(number: Int) extends Value
  case class Fun(function: Value => M[Value]) extends Value

  implicit def show(value: M[Value]): String = {
    value(identity)
  }

  implicit def show(value: Value): String = {
    value match {
      case Num(number) => number.toString
      case Fun(_) => "<function>"
      case Wrong => "<wrong>"
    }
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
      case Callcc(name, term1) =>
        callcc(
          (k: Value => M[Value]) =>
            interp(term1, (name, Fun(k)) :: environment)
        )
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
      case (Num(num1), Num(num2)) => unit(Num(num1 + num2))
      case _ => unit(Wrong)
    }
  }

  def apply(value1: Value, value2: Value): M[Value] = {
    value1 match {
      case Fun(fun) => fun(value2)
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

    val termK = Add(
      Con(1),
      Callcc(
        "k",
        Add(
          Con(2),
          App(
            Var("k"),
            Con(4)
          )
        )
      )
    )

    println(test(term42))
    println(test(termK))
  }
}
