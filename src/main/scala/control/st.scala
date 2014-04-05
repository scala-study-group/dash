package control

sealed trait ST[S,A] { self =>
  protected def run(s: S): (A,S)
  def map[B](f: A => B): ST[S,B] = new ST[S,B] {
    def run(s: S) = {
      val (a,s1) = self.run(s)
      (f(a), s1)
    }
  }
  def flatMap[B](f: A => ST[S,B]): ST[S,B] = new ST[S,B] {
    def run(s:S) = {
      val (a,s1) = self.run(s)
      f(a).run(s1)
    }
  }
}

object ST {
  def apply[S,A](a: => A) = {
    lazy val memo = a
    new ST[S,A] {
      def run(s: S) = (memo, s)
    }
  }
  def runST[A](st: RunnableST[A]): A =
    st.apply[Null].run(null)._1
}

sealed trait STRef[S,A] {
  protected var cell: A
  def read: ST[S,A] = ST(cell)
  def write(a: A): ST[S,Unit] = new ST[S,Unit] {
    def run(s: S) = {
      cell = a
      ((), s)
    }
  }
}

object STRef {
  def apply[S,A](a: A): ST[S,STRef[S,A]] = ST(new STRef[S,A] {
    var cell = a
  })
}

trait RunnableST[A] {
  def apply[S]: ST[S,A]
}

sealed abstract class STArray[S,A](implicit manifest: Manifest[A]) {

  protected def value: Array[A]
  def size: ST[S,Int] = ST(value.size)

  def write(i: Int, a: A): ST[S,Unit] = new ST[S,Unit] {
    def run(s: S) = {
      value(i) = a
      ((), s)
    }
  }

  def read(i: Int): ST[S,A] = ST(value(i))

  def freeze: ST[S,List[A]] = ST(value.toList)

  def fill(xs: Map[Int,A]): ST[S,Unit] =
    xs.foldRight(ST[S,Unit](())) {
      case ((i, a), st) => st flatMap (_ => write(i, a))
    }

  def swap(i: Int, j: Int): ST[S,Unit] =
    for {
      x <- read(i)
      y <- read(j)
      _ <- write(i,y)
      _ <- write(j,x)
    } yield ()

}

object STArray {

  def apply[S,A:Manifest](sz: Int, v: A): ST[S,STArray[S,A]] =
    ST(new STArray[S,A] {
      lazy val value = Array.fill(sz)(v)
    })

  def fromList[S,A:Manifest](xs: List[A]): ST[S,STArray[S,A]] =
    ST(new STArray[S,A] {
      lazy val value = xs.toArray
    })

  def partition[S](a: STArray[S,Int], l: Int, r: Int, pivot: Int): ST[S,Int] =
    for {
      vp <- a.read(pivot)
      _  <- a.swap(pivot, r)
      j  <- STRef(l)
      _  <- (l until r).foldLeft(ST[S,Unit](()))((s, i) =>
              for {
                _  <- s
                vi <- a.read(i)
                _  <- if (vi < vp)
                        for {
                          vj <- j.read
                          _  <- a.swap(i, vj)
                          _  <- j.write(vj + 1)
                        } yield () 
                      else ST[S,Unit](())
              } yield ())
      x  <- j.read
      _  <- a.swap(x, r)
    } yield x

  def qs[S](a: STArray[S,Int], l: Int, r: Int): ST[S,Unit] =
    if (l < r)
      for {
        pi <- partition(a, l, r, l + (r - l) / 2)
        _ <- qs(a, l, pi - 1)
        _ <- qs(a, pi + 1, r)
      } yield ()
    else ST[S,Unit](())

  def quicksort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) xs else ST.runST(new RunnableST[List[Int]] {
      def apply[S] = for {
        arr <- STArray.fromList(xs)
        size <- arr.size
        _ <- qs(arr, 0, size - 1)
        sorted <- arr.freeze
      } yield sorted
  })
}

sealed trait STMap[S,K,V] {

  protected def value: scala.collection.mutable.HashMap[K,V]
  def size: ST[S,Int] = ST(value.size)

  def write(k: K, v: V): ST[S,Unit] = new ST[S,Unit] {
    def run(s: S) = {
      value(k) = v
      ((), s)
    }
  }

  def read(k: K): ST[S,V] = ST(value(k))

  def freeze: ST[S,Map[K,V]] = ST(value.toMap)

  def fill(xs: Map[K,V]): ST[S,Unit] =
    xs.foldRight(ST[S,Unit](())) {
      case ((k, v), st) => st flatMap (_ => write(k, v))
    }

}

object STMap {

  def apply[S,K,V]: ST[S,STMap[S,K,V]] =
    ST(new STMap[S,K,V] {
      lazy val value = new scala.collection.mutable.HashMap[K,V]
    })

  def fromMap[S,K,V](xs: Map[K,V]): ST[S,STMap[S,K,V]] =
    ST(new STMap[S,K,V] {
      lazy val value = scala.collection.mutable.HashMap(xs.toSeq:_*)
    })

}
