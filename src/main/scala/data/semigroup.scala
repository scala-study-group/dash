package data

trait Semigroup[A] {
  def append(a1: A, a2: A): A
  class InfixSemigroup(a1: A) {
    def ⋅(a2: A): A = Semigroup.this.append(a1, a2)
  }
  implicit def infix(a1: A) = new InfixSemigroup(a1)
}
