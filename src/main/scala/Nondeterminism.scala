/**
 * 2.7 Variation five: Non-deterministic choice
 */

object NondeterminismM {
  type L[A] = List[A]
  def unit[A](a: A): L[A] = List(a)
  def bind[A, B](m: L[A], f: A => L[B]): L[B] = {
    for {
      a <- m
      b <- f(a)
    } yield {
      b
    }
  }
  def mzero[A]: L[A] = List.empty[A]
  def mplus[A](l: L[A], m: L[A]): L[A] = l ++ m

  // infix `>>=`
  implicit class BindNondeterminism[A](val a: L[A]) extends AnyVal {
    def >>=[B](f: A => L[B]): L[B] = bind(a, f)
  }
}


object Nondeterminism {
  import NondeterminismM._
  type Name = String
  type Environment = List[(Name, Value)]
  type M[A] = L[A]

  sealed trait Term
  case class Var(name: Name) extends Term
  case class Con(integer: Int) extends Term
  case class Add(term1: Term, term2: Term) extends Term
  case class Lam(name: Name, term: Term) extends Term
  case class App(term1: Term, term2: Term) extends Term
  case object Fail extends Term
  case class Amb(term1: Term, term2: Term) extends Term

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

  implicit def show(value: M[Value]): String = {
    "[" + bind(value, (x: Value) => unit(show(x))).mkString(",") + "]"
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
      case Fail => mzero
      case Amb(term1, term2) =>
        mplus(interp(term1, environment), interp(term2, environment))
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

    val termL = App(
      Lam("x",
        Add(
          Var("x"),
          Var("x")
        )
      ),
      Amb(
        Con(2),
        Con(1)
      )
    )

    println(test(term42))
    println(test(Fail))
    println(test(termL))
  }
}