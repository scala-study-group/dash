package control

trait Applicative[F[_]] extends data.Functor[F] {

  // primitive combinators
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  def unit[A](a: => A): F[A]

  // derived combinators
  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fa, fab)((a, ab) => ab(a))
  def map[A,B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    traverse(lma)(identity)

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence((1 to n).map(_ => ma).toList)

  def product[A,B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    {
      val f2: (A,B) => C => D = (a,b) => f.curried(a)(b)
      val f3: F[C => D] = map2(fa,fb)(f2)
      apply(f3)(fc)
    }

  def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])
                     (f: (A, B, C, D) => E): F[E] =
    {
      val f2: (A,B,C) => D => E = (a,b,c) => f.curried(a)(b)(c)
      val f3: F[D => E] = map3(fa,fb,fc)(f2)
      apply(f3)(fd)
    }

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] =
    ofa.foldRight(unit(Map.empty[K,V]))((kv, fmkv) => map2(kv._2, fmkv)((v, mkv) => mkv + (kv._1 -> v)))
}

object Applicative {

  type State[S,+A] = S => (A,S)

  val optionA = new OptionApplicative
  class OptionApplicative extends Applicative[Option] {
    def unit[A](a: => A): Option[A] = Option(a)
    def map2[A,B,C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] =
      fa match {
        case None    => None
        case Some(a) => fb map { b => f(a,b) }
      }
  }

  val streamA = new StreamApplicative
  class StreamApplicative extends Applicative[Stream] {
    import Stream._
    def unit[A](a: => A): Stream[A] = Stream(a)
    def map2[A,B,C](fa: Stream[A], fb: Stream[B])(f: (A, B) => C): Stream[C] =
      fa match {
        case Empty    => Empty
        case a #:: at =>
          fb match {
            case Empty    => Empty
            case b #:: bt => f(a,b) #:: map2(at,bt)(f)
          }
      }
  }

  val listA = new ListApplicative
  class ListApplicative extends Applicative[List] {
    def zipWith[A,B,C](f: (A,B) => C)(as: List[A])(bs: List[B]): List[C] =
      as match {
        case Nil     => Nil
        case a :: at => 
          bs match {
            case Nil     => Nil
            case b :: bt => f(a,b) :: zipWith(f)(at)(bt)
          }
      }
    def unit[A](a: => A): List[A] = List(a)
    def map2[A,B,C](fa: List[A], fb: List[B])(f: (A, B) => C): List[C] =
      zipWith(f)(fa)(fb)
  }

  def stateA[S] = new StateApplicative[S]
  class StateApplicative[S] extends Applicative[({type λ[α] = State[S,α]})#λ] {
    def unit[A](a: => A): State[S,A] = { s => (a,s) }
    def map2[A,B,C](fa: State[S,A], fb: State[S,B])(f: (A, B) => C): State[S,C] =
      { s =>
        val (a,sa) = fa(s)
        val (b,sb) = fb(sa)
        (f(a,b),sb)
      }
    
  }

  def idA[A] = new IdApplicative[A]
  class IdApplicative[A] extends Applicative[data.Id] {
    import data.Id
    def unit[A](a: => A): Id[A] = Id(a)
    def map2[A,B,C](fa: Id[A], fb: Id[B])(f: (A, B) => C): Id[C] =
      Id(f(fa.value,fb.value))
  }

  def readerA[R] = new ReaderApplicative[R]
  class ReaderApplicative[R] extends Applicative[({type λ[α] = data.Reader[R,α]})#λ] {
    import data.Reader
    def unit[B](b: => B): Reader[R,B] = Reader(a => b)
    def map2[A,B,C](fa: Reader[R,A], fb: Reader[R,B])(f: (A, B) => C): Reader[R,C] =
      Reader { r =>
        val a = fa.run(r)
        val b = fb.run(r)
        f(a,b)
      }
  }

  def eitherA[E]: Applicative[({type f[x] = Either[E, x]})#f] = new EitherApplicative[E]
  class EitherApplicative[E] extends Applicative[({type f[x] = Either[E, x]})#f] {
    def map2[A,B,C](fa: Either[E,A], fb: Either[E,B])(f: (A, B) => C): Either[E,C] =
      fa match {
        case Left(l) => Left(l)
        case Right(a) =>
          fb match {
            case Left(l) => Left(l)
            case Right(b) => Right(f(a,b))
          }
      }
    def unit[A](a: => A): Either[E,A] =
      Right(a)
  }

//case class Failure[E](head: E, tail: Vector[E]) extends Validation[E, Nothing]
//case class Success[A](a: A) extends Validation[Nothing, A]

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] = new ValidationApplicative[E]
  class ValidationApplicative[E] extends Applicative[({type f[x] = Validation[E, x]})#f] {
    def map2[A,B,C](fa: Validation[E,A], fb: Validation[E,B])(f: (A, B) => C): Validation[E,C] =
      fa match {
        case Failure(ah, at) =>
          fb match {
            case Failure(bh, bt) => Failure(ah, bt ++ at :+ bh)
            case Success(_)      => Failure(ah, at)
          }
        case Success(a) =>
          fb match {
            case Failure(bh, bt) => Failure(bh, bt)
            case Success(b)      => Success(f(a,b))
          }
      }
    def unit[A](a: => A): Validation[E,A] =
      Success(a)
  }

}
