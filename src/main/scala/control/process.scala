package control

trait Process[I,O] {

  def apply(s: Stream[I]): Stream[O] =
    this match {
      case Halt() => Stream()
      case Await(recv,fb) =>
        s match {
          case h #:: t => recv(h)(t)
          case _ => fb(s) // stream is empty
        }
      case Emit(h,t) => h.toStream append t(s)
    }

  def map[O2](f: O => O2): Process[I,O2] =
    this match {
      case Halt() => Halt()
      case Emit(h,t) => Emit(h map f, t map f)
      case Await(recv,fb) => Await(recv andThen (_ map f), fb map f)
    }

  def ++(p: => Process[I,O]): Process[I,O] =
    this match {
      case Halt() => p
      case Emit(h,t) => Process.emitAll(h,t ++ p)
      case Await(recv,fb) => Await(recv andThen (_ ++ p), fb ++ p)
    }

  def flatMap[O2](f: O => Process[I,O2]): Process[I,O2] =
    this match {
      case Halt() => Halt()
      case Emit(h,t) =>
        if (h.isEmpty) t flatMap f
        else f(h.head) ++ Process.emitAll(h.tail,t).flatMap(f)
      case Await(recv,fb) => Await(recv andThen(_ flatMap f), fb flatMap f)
    }

  def unit[O](o: => O): Process[I,O] = Process.emit(o)

  def |>[O2](p2: Process[O,O2]): Process[I,O2] =
    this match {
      case Halt() =>
        p2 match {
          case Emit(h2,t2) => Emit(h2, this |> t2)
          case _ => Halt()
        }
      case Emit(h,t) => t |> p2.feed(h)
      case Await(recv,fb) => Await({ i => recv(i) |> p2 }, fb |> p2)
    }

  def feed(in: Seq[I]): Process[I,O] =
    this match {
      case Halt() => Halt()
      case Emit(h,t) => Emit(h,t feed in)
      case Await(recv,fb) =>
        if (in.isEmpty) this
        else (recv(in.head) feed in.tail) ++ this
    }

}

object Process {

  def emitAll[I,O](head: Seq[O], tail: Process[I,O] = Halt[I,O]()): Process[I,O] =
    tail match {
      case Emit(h2,t1) => Emit(head ++ h2, t1)
      case _ => Emit(head, tail)
    }

  def emit[I,O](head: O, tail: Process[I,O] = Halt[I,O]()): Process[I,O] =
    emitAll(Stream(head), tail)

  def take[I](n: Int): Process[I,I] =
    if (n == 0) Halt() else Await({ i => emit(i, take(n-1)) })

  def drop[I](n: Int): Process[I,I] =
    if (n == 0) Await({ i => emit(i, drop(0)) })
    else Await({ i => emitAll(Nil, drop(n - 1)) })

  def takeWhile[I](f: I => Boolean): Process[I,I] =
    Await({ i => if (f(i)) emit(i, takeWhile(f)) else Halt() })

  def dropWhile[I](f: I => Boolean): Process[I,I] =
    Await({ i => if (f(i)) emitAll(Nil, dropWhile(f))
                 else emit(i, dropWhile(f)) })

  def count[I]: Process[I,Int] = {
    def _count[I](c: Int): Process[I,Int] = Await({ i => emit(c, _count(c+1)) })
    _count(0)
  }
}

case class Emit[I,O](head: Seq[O], tail: Process[I,O] = Halt[I,O]())
  extends Process[I,O]

case class Await[I,O](recv: I => Process[I,O], fallback: Process[I,O] = Halt[I,O]())
  extends Process[I,O]

case class Halt[I,O]() extends Process[I,O]

