package parscala

import scala.language.higherKinds

import scalaz.{Traverse, Applicative}

object Scalaz {
  val listTraverseInst : Traverse[List] = new Traverse[List] {
    override def traverseImpl[G[_], A, B](fa : List[A])(f : A => G[B])(implicit arg0 : Applicative[G]) : G[List[B]] =
      fa match {
        case a :: as => arg0.apply2(f(a), traverseImpl(as)(f)(arg0))((_ : B) :: (_ : List[B]))
        case List()  => arg0.pure(List())
      }
  }
}
