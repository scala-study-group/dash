package data.monoid

trait Monoid[A] extends data.semigroup.Semigroup[A] {
  def unit: A
}

class ListMonoid[A] extends Monoid[List[A]] {
  def append(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
  def unit: List[A] = List.empty
}

class OptionMonoid[A] extends Monoid[Option[A]] {
  def append(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
  val unit: Option[A] = None
}

class EndoMonoid[A] extends Monoid[A => A] {
  def append(a1: A => A, a2: A => A): A => A = a1 andThen a2
  def unit: A => A = identity
}

class FunctionMonoid[A,B](m: Monoid[B]) extends Monoid[A => B] {
  def append(a1: A => B, a2: A => B) = { a => m.append(a1(a), a2(a)) }
  def unit: A => B = _ => m.unit
}
