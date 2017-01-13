package parscala

import scala.language.higherKinds

import scalaz.Monad

object Control {
  def until[A](p : A => Boolean, f : A => A, x : A) : A = {
    if (p(x)) x else until(p, f, f(x))
  }

  def foldM[M[_], A, B](f : (A, B) => M[A], e : A, l : List[B])(implicit evidence : Monad[M]) : M[A] = {
    import scalaz.syntax.bind._
    l match {
      case List() =>
        evidence.pure(e)
      case x :: xs => 
        f(e, x) >>= (e2 => foldM(f, e2, xs))
    }
  }

  def foldM_[M[_], A, B](f : (A, B) => M[A], e : A, l : List[B])(implicit evidence : Monad[M]) : M[Unit] = 
    evidence.void(foldM(f, e, l))

  def mapM[M[_], A, B](f : A => M[B], l : List[A])(implicit evidence : Monad[M]) : M[List[B]] =
    evidence.sequence(l.map(f))(scalaz.std.list.listInstance)
}
