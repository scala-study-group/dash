package control

class STTests extends org.scalatest.FunSuite {

  test("STRef") {
    val p = new RunnableST[(Int, Int)] {
      def apply[S] =
        for {
          r1 <- STRef(1)
          r2 <- STRef(2)
          x  <- r1.read
          y  <- r2.read
          _  <- r1.write(y+1)
          _  <- r2.write(x+1)
          a  <- r1.read
          b  <- r2.read
        } yield (a,b)
    }
    assert((3,2) === ST.runST(p))
  }

  test("STArray apply") {
    val p = new RunnableST[(String, String)] {
      def apply[S] =
        for {
          a <- STArray(10, "foo")
          x <- a.read(2)
          _ <- a.write(2, "bar")
          y <- a.read(2)
        } yield (x,y)
    }
    assert(("foo","bar") === ST.runST(p))
    
  }

  test("STArray fill") {
    val p = new RunnableST[(String, String)] {
      def apply[S] =
        for {
          a <- STArray(10, "foo")
          _ <- a.fill(Map(0 -> "a", 2 -> "b"))
          x <- a.read(0)
          y <- a.read(2)
        } yield (x,y)
    }
    assert(("a","b") === ST.runST(p))
    
  }

  test("STArray fromList") {
    val p = new RunnableST[(Int, Int, Int)] {
      def apply[S] =
        for {
          a <- STArray.fromList(List(1,2,3,4,5))
          _ <- a.fill(Map(0 -> 42, 2 -> 42))
          x <- a.read(0)
          y <- a.read(1)
          z <- a.read(2)
        } yield (x,y,z)
    }
    assert((42,2,42) === ST.runST(p))
    
  }

  test("STArray swap") {
    val p = new RunnableST[(Int, Int)] {
      def apply[S] =
        for {
          a <- STArray.fromList(List(1,2,3,4,5))
          _ <- a.swap(0,2)
          x <- a.read(0)
          y <- a.read(2)
        } yield (x,y)
    }
    assert((3,1) === ST.runST(p))
    
  }

  test("STArray qs") {
    assert(List(0,1,2,3,4,5,6) === STArray.quicksort(List(5,2,6,4,1,3,0)))
  }

  test("STMap apply") {
    val p = new RunnableST[(String, String)] {
      def apply[S] =
        for {
          a <- STMap[S,Int,String]
          _ <- a.write(2, "foo")
          x <- a.read(2)
          _ <- a.write(2, "bar")
          y <- a.read(2)
        } yield (x,y)
    }
    assert(("foo","bar") === ST.runST(p))
    
  }

  test("STMap fill") {
    val p = new RunnableST[(String, String)] {
      def apply[S] =
        for {
          a <- STMap[S,Int,String]
          _ <- a.fill(Map(0 -> "a", 2 -> "b"))
          x <- a.read(0)
          y <- a.read(2)
        } yield (x,y)
    }
    assert(("a","b") === ST.runST(p))
    
  }

  test("STMap fromMap") {
    val p = new RunnableST[(Int, Int, Int)] {
      def apply[S] =
        for {
          a <- STMap.fromMap(Map(0 -> 42, 1 -> 2, 2 -> 42))
          x <- a.read(0)
          y <- a.read(1)
          z <- a.read(2)
        } yield (x,y,z)
    }
    assert((42,2,42) === ST.runST(p))
    
  }

}
