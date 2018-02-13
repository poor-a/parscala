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

  def mapM_[M[_], A, B](f : A => M[B], l : List[A])(implicit evidence : Monad[M]) : M[Unit] =
    evidence.void(evidence.sequence(l.map(f))(scalaz.std.list.listInstance))

  def forM[M[_], A, B](l : List[A])(f : A => M[B])(implicit evidence : Monad[M]) : M[List[B]] =
    mapM(f, l)

  def forM_[M[_], A, B](l : List[A])(f : A => M[B])(implicit evidence : Monad[M]) : M[Unit] =
    mapM_(f, l)

  def zipWithM[M[_], A, B, C](f : (A, B) => M[C])(as : List[A], bs : List[B])(implicit evidence : Monad[M]) : M[List[C]] =
    (as, bs) match {
      case (a :: as_, b :: bs_) =>
        evidence.apply2(f(a, b), zipWithM(f)(as_, bs_))(_ :: _)
      case (_, _) =>
        evidence.pure(List())
    }

  def catSomes[A](xs : List[Option[A]]) : List[A] =
    xs.foldRight(List[A]())(
        (mx, acc) => mx match {
            case Some(x) => x :: acc
            case None    => acc
          }
      )
}
