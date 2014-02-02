package control

import org.scalatest._
import data.Reader

class FPiSApplicativeTests extends FunSuite {

  import Applicative._

  val doubleM = { x: Int => Some(x * 2) }
  val toStringM = { x: Int => Some(x.toString) }

  test("exercise 11.1") {
    // EXERCISE 11.1: Write monad instances for Par, Parser, Option, Stream, and
    // List
    assert(optionA.unit(42) === Some(42))

    assert(streamA.unit(42) === Stream(42))

    assert(listA.unit(42) === List(42))
  }

  test("exercise 11.2") {
    // EXERCISE 11.2 (optional, hard): State looks like it would be a monad too,
    // but it takes two type arguments and you need a type constructor of one
    // argument to implement A.

    assert(stateA.unit(42)(List("forty-two")) === (42,List("forty-two")))

    assert(stateA.
      map { l: List[String] => (21, "twenty-one" :: l) }
      (x => x * 2)(Nil) ===
      (42, List("twenty-one"))) 
  }

  test("exercise 11.3") {
    // EXERCISE 11.3: The sequence and traverse combinators should be pretty
    // familiar to you by now, and your implementations of them from various
    // prior chapters are probably all very similar. Implement them once and
    // for all on Applicative[F]

    assert(optionA.sequence(List(Some(1), Some(2), Some(3))) ===
      Some(List(1,2,3)))

    assert(optionA.sequence(List(Some(1), None, Some(3))) === None)
  }

  test("exercise 11.4") {
    // EXERCISE 11.4: Implement replicateM

    assert(optionA.replicateM(4, optionA.unit(42)) ===
      Some(List(42,42,42,42)))
  }

  test("exercise 11.18") {
    // EXERCISE 11.18: Implement map and flatMap as methods on [Id], and give an
    // implementation for Applicative[Id].

    import data.Id

    assert(idA.unit(42) === Id(42))
    assert(idA.map(Id(2))(x => x * 21) === Id(42))
  }

  test("exercise 11.21") {
    // EXERCISE 11.21: To cement your understanding of monads, give a monad
    // instance for the following type, and explain what it means.

    val plus  = { x: Int => Reader({ y: Int => x + y }) }
    val times = { x: Int => Reader({ y: Int => x * y }) }

    assert(readerA.unit(42).run(123) === 42)
  }

}
