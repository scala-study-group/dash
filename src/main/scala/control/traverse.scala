package control

trait Traverse[F[_]] extends data.Functor[F] {

  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_]:Applicative,A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  def map[A,B](fa: F[A])(f: A => B): F[B] = {
    implicit val idA = Applicative.idA[A]
    traverse(fa)({ a => idA.unit(f(a)) }).value
  }

}

case class Tree[+A](head: A, tail: List[Tree[A]])

object Traverse {

  val listT: Traverse[List] =
    new Traverse[List] {
      override def traverse[G[_]:Applicative,A,B](fa: List[A])
        (f: A => G[B]): G[List[B]] = {
          fa match {
            case Nil => implicitly[Applicative[G]].unit(Nil)
            case a :: as =>
              val app = implicitly[Applicative[G]]
              val cons = { (a: B, b: List[B]) => a :: b }
              app.map2(f(a),traverse(as)(f))(cons)
          }
        }
    }

  val optionT: Traverse[Option] =
    new Traverse[Option] {
      override def traverse[G[_]:Applicative,A,B](fa: Option[A])
        (f: A => G[B]): G[Option[B]] =
          fa match {
            case None => implicitly[Applicative[G]].unit(None)
            case Some(a) => implicitly[Applicative[G]].map(f(a))(Option.apply)
          }
    }

  val treeT: Traverse[Tree] =
    new Traverse[Tree] {
      override def traverse[G[_]:Applicative,A,B](fa: Tree[A])
        (f: A => G[B]): G[Tree[B]] =
          {
            val app = implicitly[Applicative[G]]
            val gbs: List[Tree[G[B]]] = fa.tail map { t => map(t)(f) }
            implicitly[Applicative[G]].map(f(fa.head))(Tree(_, Nil))
          }
    }

case class Tree[+A](head: A, tail: List[Tree[A]])
}
