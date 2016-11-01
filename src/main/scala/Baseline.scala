object Baseline {
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
  case class Fun(function: Value => Value) extends Value

  implicit def show(value: Value): String = {
    value match {
      case Num(number) => number.toString
      case Fun(_) => "<function>"
      case Wrong => "<wrong>"
    }
  }

  def interp(term: Term, environment: Environment): Value = {
    term match {
      case Var(name) => lookup(name, environment)
      case Con(number) => Num(number)
      case Add(term1, term2) => add(interp(term1, environment), interp(term2, environment))
      case Lam(name, term1) => Fun((value) => interp(term1,  (name, value) :: environment))
      case App(term1, term2) => apply(interp(term1, environment), interp(term2, environment))
    }
  }

  def lookup(name: Name, environment: Environment): Value = {
    environment match {
      case Nil => Wrong
      case ((envName: Name, value: Value) :: env) => if (name == envName) value else lookup(name, env)
    }
  }

  def add(value1: Value, value2: Value): Value = {
    (value1, value2) match {
      case (Num(num1), Num(num2)) => Num(num1 + num2)
      case _ => Wrong
    }
  }

  def apply(value1: Value, value2: Value): Value = {
    value1 match {
      case Fun(fun) => fun(value2)
      case _ => Wrong
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

    println(interp(term42, List()): String)
  }
}
