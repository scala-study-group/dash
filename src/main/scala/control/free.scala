package control

sealed trait Free[F[_],A]
case class Return[F[_],A](a: A) extends Free[F,A]
case class Suspend[F[_],A](s: F[Free[F,A]]) extends Free[F,A]
case class FlatMap[F[_],A,B](s: Free[F,A], f: A => Free[F,B]) extends Free[F,B]

class FreeMonad[F[_]] extends Monad[({type M[A] = Free[F,A]})#M] {

  def unit[A](a: => A): Free[F,A] = Return(a)

  def flatMap[A,B](ma: Free[F,A])(f: A => Free[F,B]): Free[F,B] =
    ma match {
      case FlatMap(s,f2) => FlatMap(s, { a: Any => FlatMap(f2(a), f) })
      case x             => FlatMap(x, f)
    }

}
