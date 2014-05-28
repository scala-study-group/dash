package control

import org.scalatest._

class FPiSProcessTests extends FunSuite {

  def vowels: Process[Char,Char] = {
    def f(c: Char) = if (Set('a','e','i','o','u')(c)) Seq(c) else Seq.empty
    Await({ c => Emit(f(c), vowels) })
  }

  def sayAgain[A]: Process[A,A] =
    Await({ a => Emit(Seq(a,a), sayAgain) })

  test("Halt#apply(empty)") {
    assert("" === Halt()(Stream.empty).mkString)
  }

  test("Halt#apply(stream)") {
    assert("" === Halt()("hello, world".toStream).mkString)
  }

  test("Emit#apply(empty)") {
    assert("foo" === Emit("foo".toSeq)(Stream.empty).mkString)
  }

  test("Emit#apply(stream)") {
    assert("foo" === Emit("foo".toSeq)("hello, world".toStream).mkString)
  }

  test("Await#apply(empty)") {
    assert("" === Await[Char,Char]({ c => Emit(Seq(c)) })(Stream.empty).mkString)
  }

  test("Await#apply(stream)") {
    assert("h" === Await[Char,Char]({ c => Emit(Seq(c)) })("hello, world".toStream).mkString)
  }

  test("vowels#apply(empty)") {
    assert("" === vowels(Stream.empty).mkString)
  }

  test("vowels#apply(stream)") {
    assert("eoo" === vowels("hello, world".toStream).mkString)
  }

  test("sayAgain#apply(empty)") {
    assert("" === sayAgain(Stream.empty).mkString)
  }

  test("sayAgain#apply(stream)") {
    assert("ffoooo" === sayAgain("foo".toStream).mkString)
  }

  test("vowels#feed(empty)#apply(empty)") {
    assert("" === (vowels.feed(Seq.empty))(Stream.empty).mkString)
  }

  test("vowels#feed(seq)#apply(empty)") {
    assert("eoo" === (vowels.feed("hello, world".toSeq))(Stream.empty).mkString)
  }

  test("vowels#feed(empty)#apply(stream)") {
    assert("o" === (vowels.feed(Seq.empty))(", world".toStream).mkString)
  }

  test("vowels#feed(seq)#apply(stream)") {
    assert("eoo" === (vowels.feed("hello".toSeq))(", world".toStream).mkString)
  }

  test("sayAgain#feed(empty)#apply(empty)") {
    assert("" === (sayAgain.feed(Seq.empty))(Stream.empty).mkString)
  }

  test("sayAgain#feed(seq)#apply(empty)") {
    assert("ffoooo" === (sayAgain.feed("foo".toSeq))(Stream.empty).mkString)
  }

  test("sayAgain#feed(empty)#apply(stream)") {
    assert("bbaarr" === (sayAgain.feed(Seq.empty))("bar".toStream).mkString)
  }

  test("sayAgain#feed(seq)#apply(stream)") {
    assert("ffoooobbaarr" === (sayAgain.feed("foo".toSeq))("bar".toStream).mkString)
  }

  // Exercise 1: Implement |>
  test("|>") {
    assert("eeoooo" === (vowels |> sayAgain)("hello, world".toStream).mkString)
  }

  // Exercise 2: Implement take, drop, takeWhile, dropWhile
  test("take") {
    assert("hello" === Process.take(5)("hello, world".toStream).mkString)
  }

  test("drop") {
    assert("world" === Process.drop(7)("hello, world".toStream).mkString)
  }

  test("takeWhile") {
    val notW: Char => Boolean = _ != 'w'
    assert("hello, " === Process.takeWhile(notW)("hello, world".toStream).mkString)
  }

  test("dropWhile") {
    val notW: Char => Boolean = _ != 'w'
    assert("www" === Process.dropWhile(notW)("hello, wwworld".toStream).mkString)
  }

  // Exercise 3: count
  test("count") {
    assert("01234" === Process.count("hello".toStream).mkString)
  }

}
