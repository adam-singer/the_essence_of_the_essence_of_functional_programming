/**
 * 2.4 Variation two: Error messages with position
 */

object ErrorM {
  type E[A] = ErrorMonad[A]
  def unit[A](a: A): E[A] = Success(a)
  def bind[A, B](a: E[A], f: A => E[B]): E[B] = a match {
    case Success(value) => f(value)
    case Error(message) => Error(message)
  }
  def fail[A](a: String): E[A] = Error(a)
  sealed trait ErrorMonad[+A]
  case class Success[A](a: A) extends ErrorMonad[A]
  case class Error(message: String) extends ErrorMonad[Nothing]

  // infix `>>=`
  implicit class BindError[A](val a: E[A]) extends AnyVal {
    def >>=[B](f: A => E[B]): E[B] = bind(a, f)
  }
}

object PositionM {
  import ErrorM.{ErrorMonad, bind => bindM, unit => unitM, fail => failM}
  type Position = Int
  val position0: Position = 0
  type P[A] = Position => ErrorMonad[A]
  def unit[A](a: A): P[A] = (p) => unitM(a)

  def bind[A, B](a: P[A], f: A => P[B]): P[B] = (p: Position) => bindM(a(p), (x: A) => f(x)(p))

  def fail[A](a: String): P[A] = (p) => failM(s"$p : $a")

  def reset[A](position: Position, m: P[A]): P[A] = (p) => m(position)

  // infix `>>=`
  implicit class BindPosition[A](val a: P[A]) extends AnyVal {
    def >>=[B](f: A => P[B]): P[B] = bind(a, f)
  }
}

object Position {
  import ErrorM.{ErrorMonad, Success, Error}
  import PositionM.{Position, P, position0, unit, fail, reset, BindPosition}

  type Name = String
  type Environment = List[(Name, Value)]
  type M[A] = P[A]

  sealed trait Term
  case class Var(name: Name) extends Term
  case class Con(integer: Int) extends Term
  case class Add(term1: Term, term2: Term) extends Term
  case class Lam(name: Name, term: Term) extends Term
  case class App(term1: Term, term2: Term) extends Term
  case class At(position: Position, term: Term) extends Term

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

  implicit def show(value: ErrorMonad[Value]): String = {
    value match {
      case Success(v) => v
      case Error(message) => s"<error: $message>"
    }
  }

  implicit def show[A](position: M[Value]): String = {
    position(position0)
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
      case At(p, t) =>
        reset(p, interp(t, environment))
    }
  }

  def lookup(name: Name, environment: Environment): M[Value] = {
    environment match {
      case Nil => fail(s"unbound variable: $name")
      case ((envName, value) :: env) => if (name == envName)
        unit(value)
      else
        lookup(name, env)
    }
  }

  def add(value1: Value, value2: Value): M[Value] = {
    (value1, value2) match {
      case (Num(num1), Num(num2)) => unit(Num(num1 + num2))
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

    val termP = Add(
      Con(1),
      At(
        42,
        App(
          Con(2),
          Con(3)
        )
      )
    )

    println(interp(term42, List()): String)
    println(interp(termE, List()): String)
    println(interp(termP, List()): String)

    // [localhost ~]$ runhaskell ~/Downloads/developers-code-2710/repository/ralfs-channel9-lectures/code/monads/Haskell/origin/cache/Positions.hs
    //  42
    //  <error: 0: should be function: 1>
    //  <error: 42: should be function: 2>
  }
}

//
//
//object Position {
//  type Name = String
//  type Environment = List[(Name, Value)]
//  type Position = Int
//
//  /*
//  type P a = Position -> E a
//
//returnP :: a -> P a
//returnP a = \p -> returnE a
//
//bindP :: P a -> (a -> P b) -> P b
//m `bindP` k = \p -> m p `bindE` \x -> k x p
//
//instance Show (P Value) where
//  show f = show (f pos0)
//
//failP :: String -> P a
//failP s = \p -> failE (show p ++ ": " ++ s)
//
//resetP :: Position -> P x -> P x
//resetP q m = \p -> m q
//
//type Position = Int
//
//pos0 :: Position
//pos0 = 0
//
//type M a = P a
//return = returnP
//(>>=)  = bindP
//fail   = failP
//   */
//
//
//
//  object PositionMonad {
//    type P[A] = Position => ErrorMonad[A]
//    def unit[A](a: A): P[A] = (p) => ErrorMonad.unit(a)
//
//    // : ErrorMonad[A] =
//    //??? //Success(a)
//    def bind[A, B](a: P[A], f: A => P[B]): P[B] = (p: Position) => ErrorMonad.bind(a(p), (x: A) => f(x)(p))
//
//    //failP :: String -> P a
//    //failP s = \p -> failE (show p ++ ": " ++ s)
//    def fail[A](a: String): P[A] = (p) => ErrorMonad.fail(s"$p : $a")
//
//    //resetP :: Position -> P x -> P x
////    resetP q m = \p -> m q
//    def reset[A](position: Position, m: P[A]): P[A] = (p) => m(position)
//
//
//    // TODO(adams): move to a different example.
//    // this would allow for `pvalue >>= function` style application
//    //http://stackoverflow.com/questions/23890507/creating-infix-operators-in-scala
//    implicit class Bind[A](val a: P[A]) extends AnyVal {
//      def >>=[B](f: A => P[B]): P[B] = (p: Position) => ErrorMonad.bind(a(p), (x: A) => f(x)(p))
//    }
//  }
//
//  object ErrorMonad {
//    type E[A] = ErrorMonad[A]
//    def unit[A](a: A): E[A] = Success(a)
//    def bind[A, B](a: E[A], f: A => E[B]): E[B] = a match {
//      case Success(value) => f(value)
//      case Error(message) => Error(message)
//    }
//    def fail[A](a: String): E[A] = Error(a)
//  }
//
//  type M[A] = PositionMonad.P[A]
//  val position0: Position = 0
//
//  sealed trait ErrorMonad[+A]
//  case class Success[A](a: A) extends ErrorMonad[A]
//  case class Error(message: String) extends ErrorMonad[Nothing]
//
//  sealed trait Term
//  case class Var(name: Name) extends Term
//  case class Con(integer: Int) extends Term
//  case class Add(term1: Term, term2: Term) extends Term
//  case class Lam(name: Name, term: Term) extends Term
//  case class App(term1: Term, term2: Term) extends Term
//  case class At(position: Position, term: Term) extends Term
//
//  sealed trait Value
//  case object Wrong extends Value
//  case class Num(number: Int) extends Value
//  case class Fun(function: Value => M[Value]) extends Value
//
//  implicit def show(value: Value): String = {
//    value match {
//      case Num(number) => number.toString
//      case Fun(_) => "<function>"
//      case Wrong => "<wrong>"
//    }
//  }
//
//  implicit def show(value: ErrorMonad[Value]): String = {
//    value match {
//      case Success(v) => v
//      case Error(message) => s"<error: $message>"
//    }
//  }
//
//  implicit def show(position: M[Value]): String = {
//    position(position0)
//  }
//
//  def interp(term: Term, environment: Environment): M[Value] = {
//    term match {
//      case Var(name) => lookup(name, environment)
//      case Con(number) => PositionMonad.unit(Num(number))
//      case Add(term1, term2) =>
//        val am = interp(term1, environment)
//        val bm = interp(term2, environment)
////        am >>= ((a: Value) => bm >>= ((b: Value) => add(a,b)))
////        am >>= {
////          (a: Value) => bm >>= {
////            (b: Value) => add(a, b)
////          }
////        }
//        PositionMonad.bind(am, (a: Value) => PositionMonad.bind(bm, (b: Value) => add(a, b)))
//      case Lam(name, term1) =>
//        PositionMonad.unit(Fun((value) => interp(term1, (name, value) :: environment)))
//      case App(term1, term2) =>
//        val fm = interp(term1, environment)
//        val am = interp(term2, environment)
//        // interp (App t u) e = interp t e >>= \f -> interp u e >>= \a -> apply f a
//        // ErrorMonad.bind(fm, (f: Value) => ErrorMonad.bind(am, (a: Value) => apply(f, a)))
//        PositionMonad.bind(fm, (f: Value) => PositionMonad.bind(am, (a: Value) => apply(f, a)))
//
//      case At(p, t) =>
//        PositionMonad.reset(p, interp(t, environment))
//      //     TODO: interp (At p t) e
//      //    = resetP p (interp t e)
//    }
//  }
//
//  def lookup(name: Name, environment: Environment): M[Value] = {
//    environment match {
//      case Nil => PositionMonad.fail(s"unbound variable: $name")
//      case ((envName: Name, value) :: env) => if (name == envName)
//        PositionMonad.unit(value)
//      else
//        lookup(name, env)
//    }
//  }
//
//  def add(value1: Value, value2: Value): M[Value] = {
//    (value1, value2) match {
//      case (Num(num1), Num(num2)) => PositionMonad.unit(Num(num1 + num2))
//      case _ => PositionMonad.fail(s"should be numbers: ${show(value1)}, ${show(value2)}")
//    }
//  }
//
//  def apply(value1: Value, value2: Value): M[Value] = {
//    value1 match {
//      case Fun(fun) => fun(value2)
//      case _ => PositionMonad.fail(s"should be function: ${show(value1)}")
//    }
//  }
//
//  def main(args: Array[String]): Unit = {
//    val term42 = App(
//      Lam("x",
//        Add(
//          Var("x"),
//          Var("x")
//        )
//      ),
//      Add(
//        Con(10),
//        Con(11)
//      )
//    )
//
//    val termE = App(
//      Con(1),
//      Con(2)
//    )
//
//    val termP = Add(
//      Con(1),
//      At(
//        42,
//        App(
//          Con(2),
//          Con(3)
//        )
//      )
//    )
//
//    println(show(interp(term42, List())))
//    println(show(interp(termE, List())))
//    println(show(interp(termP, List())))
//
//    // [localhost ~]$ runhaskell ~/Downloads/developers-code-2710/repository/ralfs-channel9-lectures/code/monads/Haskell/origin/cache/Positions.hs
//    //  42
//    //  <error: 0: should be function: 1>
//    //  <error: 42: should be function: 2>
//  }
//}
