package control.monad

import data.functor.Functor

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

  def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    traverse(lma)(identity)

  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.reverse.foldLeft(unit(List.empty[B]))((flb,a) => flatMap(f(a))(b => map(flb)(lb => b :: lb)))

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence((1 to n).map(_ => ma).toList)

  def product[A,B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms.reverse.foldLeft(unit(List.empty[A]))((fla,a) => flatMap(f(a))(x => if (x) map(fla)(la => a :: la) else fla))

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    { a => flatMap(f(a))(g) }

  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(fa => fa)
}

object Monad {

  type State[S,+A] = S => (A,S)

/*
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }
*/

//  val parMonad = new Monad[Par]
//  val parserMonad = new Monad[Parser]

  val optionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Option(a)
    def flatMap[A,B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma flatMap f
  }

  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)
    def flatMap[A,B](ma: Stream[A])(f: A => Stream[B]): Stream[B] =
      ma flatMap f
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)
    def flatMap[A,B](ma: List[A])(f: A => List[B]): List[B] =
      ma flatMap f
  }

  def stateMonad[S] = new Monad[({type λ[α] = State[S,α]})#λ] {
    def unit[A](a: => A): State[S,A] = { s => (a,s) }
    def flatMap[A,B](ma: State[S,A])(f: A => State[S,B]): State[S,B] =
      { s =>
        val (a, s2) = ma(s)
        f(a)(s2)
      }
  }

  def idMonad[A] = {
    import data.Id
    new Monad[Id] {
      def unit[A](a: => A): Id[A] = Id(a)
      def flatMap[A,B](ma: Id[A])(f: A => Id[B]): Id[B] =
        ma flatMap f
    }
  }

}
