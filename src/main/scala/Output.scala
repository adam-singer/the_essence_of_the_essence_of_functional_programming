/**
 * 2.6 Variation four: Output
 */

object OutputM {

  type O[A] = (A, String)
  def unit[A](a: A): O[A] = (a, "")
  def bind[A, B](m: O[A], f: A => O[B]): O[B] = {
    val (a, r) = m
    val (b, s) = f(a)
    (b, r ++ s)
  }

  // infix `>>=`
  implicit class BindOutput[A](val a: O[A]) extends AnyVal {
    def >>=[B](f: A => O[B]): O[B] = bind(a, f)
  }
}

object Output {
  import OutputM._
  type Name = String
  type Environment = List[(Name, Value)]
  type M[A] = O[A]

  sealed trait Term
  case class Var(name: Name) extends Term
  case class Con(integer: Int) extends Term
  case class Add(term1: Term, term2: Term) extends Term
  case class Lam(name: Name, term: Term) extends Term
  case class App(term1: Term, term2: Term) extends Term
  case class Out(term: Term) extends Term

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

  implicit def show(value: O[Value]): String = {
    val (a, o) = value
    s"Value: ${a: String}; Output: $o"
  }

  def tell(a: Value) = (Unit, s"${a: String}; ")

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
      case Out(term1) =>
        interp(term1, environment) >>= {
          (a) => {
            tell(a) >>= {
              _ => unit(a)
            }
          }
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

    val termO = Add(
      Out(Con(41)),
      Out(Con(1))
    )

    println(test(term42))
    println(test(termO))
  }
}
