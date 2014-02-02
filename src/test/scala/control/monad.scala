package control

import org.scalatest._
import data.Reader

class FPiSMonadTests extends FunSuite {

  import Monad._

  val doubleM = { x: Int => Some(x * 2) }
  val toStringM = { x: Int => Some(x.toString) }

  test("exercise 11.1") {
    // EXERCISE 11.1: Write monad instances for Par, Parser, Option, Stream, and
    // List
    assert(optionM.unit(42) === Some(42))
    assert(optionM.flatMap(Some(42))(x => Some(x * 2)) === Some(84))

    assert(streamM.unit(42) === Stream(42))
    assert(streamM.flatMap(Stream(42))(x => Stream(x * 2)) === Stream(84))

    assert(listM.unit(42) === List(42))
    assert(listM.flatMap(List(42))(x => List(x * 2)) === List(84))
  }

  test("exercise 11.2") {
    // EXERCISE 11.2 (optional, hard): State looks like it would be a monad too,
    // but it takes two type arguments and you need a type constructor of one
    // argument to implement M.

    assert(stateM.unit(42)(List("forty-two")) === (42,List("forty-two")))

    assert(stateM.
      map { l: List[String] => (21, "twenty-one" :: l) }
      (x => x * 2)(Nil) ===
      (42, List("twenty-one"))) 

    assert(stateM.
      flatMap { l: List[String] => (21, "twenty-one" :: l) }
      (x => { l: List[String] => (x * 2, "times two" :: l) } )(Nil) ===
      (42, List("times two", "twenty-one"))) 
  }

  test("exercise 11.3") {
    // EXERCISE 11.3: The sequence and traverse combinators should be pretty
    // familiar to you by now, and your implementations of them from various
    // prior chapters are probably all very similar. Implement them once and
    // for all on Monad[F]

    assert(optionM.sequence(List(Some(1), Some(2), Some(3))) ===
      Some(List(1,2,3)))

    assert(optionM.sequence(List(Some(1), None, Some(3))) === None)
  }

  test("exercise 11.4") {
    // EXERCISE 11.4: Implement replicateM

    assert(optionM.replicateM(4, optionM.unit(42)) ===
      Some(List(42,42,42,42)))
  }

  test("exercise 11.6") {
    // EXERCISE 11.6 (hard): Here's an example of a function we haven't seen
    // before. Implement the function filterM.

    //filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]]

    assert(optionM.filterM(List(1,2,3))(x => Some(x % 2 == 1)) ===
      Some(List(1,3)))
  }

  test("exercise 11.9") {
    // EXERCISE 11.9: Implement [compose].
    
    assert(optionM.compose(doubleM, toStringM)(21) === Some("42"))
  }

  test("exercise 11.10") {
    // EXERCISE 11.10 (hard): Implement flatMap in terms of compose.

    def flatMap[A,B](x: Option[A])(f: A => Option[B]): Option[B] =
      optionM.compose({ _: Unit => x }, f)(())

    assert(flatMap(Some(2))(x => Some(x * 21)) === Some(42))
  }

  test("exercise 11.13") {
    // EXERCISE 11.13: There is a third minimal set of monadic combinators: map,
    // unit, and join. Implement join.

    assert(optionM.join(Some(Some(42))) === Some(42))
  }

  test("exercise 11.14") {
    // EXERCISE 11.14: Implement either flatMap or compose in terms of join.
    def flatMap[A,B](x: Option[A])(f: A => Option[B]): Option[B] =
      optionM.join(optionM.map(x)(f))

    assert(flatMap(Some(2))(x => Some(x * 21)) === Some(42))

    def compose[A,B,C](f: A => Option[B], g: B => Option[C]): A => Option[C] =
      { a => optionM.join(optionM.map(f(a))(g)) }

    assert(compose(doubleM, toStringM)(21) === Some("42"))
  }

  test("exercise 11.18") {
    // EXERCISE 11.18: Implement map and flatMap as methods on [Id], and give an
    // implementation for Monad[Id].

    import data.Id

    assert(idM.unit(42) === Id(42))
    assert(idM.map(Id(2))(x => x * 21) === Id(42))
    assert(idM.flatMap(Id(2))(x => Id(x * 21)) === Id(42))
  }

  test("exercise 11.21") {
    // EXERCISE 11.21: To cement your understanding of monads, give a monad
    // instance for the following type, and explain what it means.

    val plus  = { x: Int => Reader({ y: Int => x + y }) }
    val times = { x: Int => Reader({ y: Int => x * y }) }

    assert(readerM.unit(42).run(123) === 42)
    assert(readerM.flatMap(plus(19))(times).run(2) === 42)
  }

}
