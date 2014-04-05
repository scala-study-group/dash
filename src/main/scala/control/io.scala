package control

sealed trait IO[F[_], +A]

case class Pure[F[_], +A](get: A) extends IO[F,A]
case class Request[F[_], I, +A](
    expr: F[I],
    receive: I => IO[F,A]) extends IO[F,A]

trait Run[F[_]] {
  def apply[A](expr: F[A]): (A, Run[F])
}

object IO {

  @annotation.tailrec
  def run[F[_],A](R: Run[F])(io: IO[F,A]): A = io match {
    case Pure(a) => a
    case Request(expr,recv) =>
      R(expr) match { case (e,r2) => run(r2)(recv(e)) }
  }

  def monad[F[_]] = new IOMonad[F]
  class IOMonad[F[_]] extends Monad[({ type f[a] = IO[F,a]})#f] {

    def unit[A](a: => A): IO[F,A] = Pure(a)

    override def map2[A,B,C](fa: IO[F,A], fb: IO[F,B])(f: (A, B) => C): IO[F,C] =
      fa match {
        case Pure(a) =>
          fb match {
            case Pure(b) => Pure(f(a,b))
            case Request(eb,rb)  => ??? // Request(eb, { i => flatMap(rb(i))({ b => unit(f(a,b)) }) } )
          }
        case Request(ea,ra) =>
          fb match {
            case Pure(b)         => ???
            case Request(eb,rb)  => ???
          }
      }

    def flatMap[A,B](ma: IO[F,A])(f: A => IO[F,B]): IO[F,B] =  ???
  }

}
