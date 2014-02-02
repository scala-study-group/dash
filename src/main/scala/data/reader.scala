package data

case class Reader[A,B](run: A => B)
