package data.monoid

import org.scalatest._

class MonoidTests extends FunSuite {

  test("list monoid") {

    val m = new ListMonoid[Int]
    import m._

    assert(List(1,2,3) === unit ⋅ List(1,2,3))
    assert(List(1,2,3) === List(1,2,3) ⋅ unit)
    assert(List(1,2,3,4,5,6) === List(1,2,3) ⋅ List(4,5,6))
    assert(List(4,5,6,1,2,3) === List(4,5,6) ⋅ List(1,2,3))

  }

  test("option monoid") {

    val m = new OptionMonoid[Int]
    import m._

    assert(None === unit)
    assert(Some(42) === unit ⋅ Some(42))
    assert(Some(42) === Some(42) ⋅ unit)
    assert(Some(42) === unit ⋅ Some(42) ⋅ unit)
    assert(Some(42) === Some(42) ⋅ Some(21) ⋅ Some(2))

  }

  test("endo monoid") {

    val timesTwo: Int => Int = _ * 2
    val plusOne: Int => Int = _ + 1

    val m = new EndoMonoid[Int]
    import m._

    assert(42 === (unit)(42))
    assert(42 === (unit ⋅ timesTwo)(21))
    assert(42 === (plusOne ⋅ unit ⋅ timesTwo)(20))
    assert(42 === (timesTwo ⋅ plusOne ⋅ plusOne)(20))

  }

  test("function monoid") {

    val timesTwo: Int => Int = _ * 2
    val plusOne: Int => Int = _ + 1

    object IntAdditionMonoid extends Monoid[Int] {
      def append(a1: Int, a2: Int): Int = a1 + a2
      val unit: Int = 0
    }

    val m = new FunctionMonoid[Int,Int](IntAdditionMonoid)
    import m._

    assert(42 === (plusOne ⋅ plusOne ⋅ timesTwo)(10))
    assert(42 === (plusOne ⋅ unit ⋅ plusOne ⋅ timesTwo)(10))
    assert(42 === (unit ⋅ plusOne ⋅ plusOne ⋅ timesTwo)(10))
  }

}
