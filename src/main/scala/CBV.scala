object CBV {
  type Name = String
  type Environment = List[(Name, Value)]

  sealed trait Term
  case class Var(name: Name) extends Term
  case class Con(integer: Int) extends Term
  case class Add(term1: Term, term2: Term) extends Term
  case class Lam(name: Name, term: Term) extends Term
  case class App(term1: Term, term2: Term) extends Term

  case class M[A](a: A)
  def returnM[A](a: A): M[A] = M(a)
  def bindM[A, B](a: M[A], f: A => M[B]): M[B] = f(a.a)

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

  def interp(term: Term, environment: Environment): M[Value] = {
    term match {
      case Var(name) => lookup(name, environment)
      case Con(number) => returnM(Num(number))
      case Add(term1, term2) =>
        val am = interp(term1, environment)
        val bm = interp(term2, environment)
        bindM(am, (a: Value) => bindM(bm, (b: Value) => add(a, b)))
      case Lam(name, term1) => returnM(Fun((value) => interp(term1,  (name, value) :: environment)))
      case App(term1, term2) =>
        val fm = interp(term1, environment)
        val am = interp(term2, environment)
        bindM(fm, (f: Value) => bindM(am, (a: Value) => apply(f, a)))
    }
  }

  def lookup(name: Name, environment: Environment): M[Value] = {
    environment match {
      case Nil => returnM(Wrong)
      case ((envName: Name, value: Value) :: env) => if (name == envName) returnM(value) else lookup(name, env)
    }
  }

  def add(value1: Value, value2: Value): M[Value] = {
    (value1, value2) match {
      case (Num(num1), Num(num2)) => returnM(Num(num1 + num2))
      case _ => returnM(Wrong)
    }
  }

  def apply(value1: Value, value2: Value): M[Value] = {
    value1 match {
      case Fun(fun) => fun(value2)
      case _ => returnM(Wrong)
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

    println(interp(term42, List()).a: String)
  }
}