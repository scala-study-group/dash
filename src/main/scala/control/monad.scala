package control

import Applicative._
import data.Functor
import data.Reader

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms.reverse.foldLeft(unit(List.empty[A]))((fla,a) => flatMap(f(a))(x => if (x) map(fla)(la => a :: la) else fla))

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    { a => flatMap(f(a))(g) }

  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(fa => fa)
}

object Monad {

  type State[S,+A] = S => (A,S)

  val optionM = new OptionMonad
  class OptionMonad extends OptionApplicative with Monad[Option] {
    def flatMap[A,B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma flatMap f
  }

  val streamM = new StreamMonad
  class StreamMonad extends StreamApplicative with Monad[Stream] {
    def flatMap[A,B](ma: Stream[A])(f: A => Stream[B]): Stream[B] =
      ma flatMap f
  }

  val listM = new ListMonad
  class ListMonad extends ListApplicative with Monad[List] {
    def flatMap[A,B](ma: List[A])(f: A => List[B]): List[B] =
      ma flatMap f
  }

  def stateM[S] = new StateMonad[S]
  class StateMonad[S] extends StateApplicative[S] with Monad[({type λ[α] = State[S,α]})#λ] {
    def flatMap[A,B](ma: State[S,A])(f: A => State[S,B]): State[S,B] =
      { s =>
        val (a, s2) = ma(s)
        f(a)(s2)
      }
  }

  val idM = new IdMonad
  class IdMonad extends IdApplicative with Monad[data.Id] {
    import data.Id
    def flatMap[A,B](ma: Id[A])(f: A => Id[B]): Id[B] =
      ma flatMap f
  }

  def readerM[R] = new ReaderMonad[R]
  class ReaderMonad[R] extends ReaderApplicative[R] with Monad[({type λ[α] = Reader[R,α]})#λ] {
    def flatMap[B,C](r: Reader[R,B])(f: B => Reader[R,C]): Reader[R,C] =
      Reader(a => f(r.run(a)).run(a))
  }

  def eitherM[E]: Monad[({type f[x] = Either[E, x]})#f] = new EitherMonad[E]
  class EitherMonad[E] extends EitherApplicative[E] with Monad[({type f[x] = Either[E, x]})#f] {
    def flatMap[A,B](ma: Either[E,A])(f: A => Either[E,B]): Either[E,B] =
      ma match {
        case Left(l) => Left(l)
        case Right(a) => f(a)
      }

  }
}
