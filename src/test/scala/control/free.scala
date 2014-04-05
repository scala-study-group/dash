package control

import org.scalatest._

class FreeTests extends FunSuite {

  def runO[A](x: Option[Free[Option,A]]): Option[A] =
    x match {
      case Some(f) => run(f)
      case None    => None
    }

  def run[A](f: Free[Option,A]): Option[A] =
    f match {
      case Return(x)     => Some(x)
      case Suspend(s)    => runO(s)
      case FlatMap(s, f) => runO(run(s).map(f))
    }

  test("Free#map") {
    val optionFree = new FreeMonad[Option]
    val double = { x: Int => x * 2 }
    val doubleM: Int => Free[Option,Int] = { x: Int => Return(x * 2) }

    val r21: Free[Option,Int] = Return(21)
    val r42: Free[Option,Int] = Return(42)

    assert(Some(42) === run(optionFree.map(r21)(double)))
    
    val s21: Free[Option,Int] = Suspend(Option(r21))
    val s42: Free[Option,Int] = Suspend(Option(r42))

    assert(Some(42) === run(optionFree.map(s21)(double)))

    assert(Some(42) === run(optionFree.flatMap(s21)(doubleM)))

  }

}
