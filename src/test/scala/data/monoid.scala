package data

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object ListMonoidSpec extends Properties("list monoid") {

  val m = new ListMonoid[Int]
  import m._

  property("left identity")  = forAll { l: List[Int] => l == unit ⋅ l }
  property("right identity") = forAll { l: List[Int] => l == l ⋅ unit }
  property("associativity")  = forAll { (l1: List[Int], l2: List[Int], l3: List[Int]) => l1 ⋅ (l2 ⋅ l3) == (l1 ⋅ l2) ⋅ l3 }

}

object OptionMonoidSpec extends Properties("option monoid") {

  val m = new OptionMonoid[Int]
  import m._

  property("left identity")  = forAll { o: Option[Int] => o == unit ⋅ o }
  property("right identity") = forAll { o: Option[Int] => o == o ⋅ unit }
  property("associativity")  = forAll { (l1: Option[Int], l2: Option[Int], l3: Option[Int]) => l1 ⋅ (l2 ⋅ l3) == (l1 ⋅ l2) ⋅ l3 }

}

import org.scalatest._

class MonoidTests extends FunSuite {

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
