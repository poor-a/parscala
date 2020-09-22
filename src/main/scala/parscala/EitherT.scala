package parscala

import scala.language.higherKinds

import scalaz.{\/-, MonadState}

object EitherT {
  implicit def eitherTStateTInstance[M[_], E, S](implicit mSt : MonadState[M, S]) : MonadState[({type λ[A] = scalaz.EitherT[E, M, A]})#λ, S] =
    new MonadState[({type λ[A] = scalaz.EitherT[E, M, A]})#λ, S] {
      override def bind[A, B](fa : scalaz.EitherT[E, M, A])(f : A => scalaz.EitherT[E, M, B]) : scalaz.EitherT[E, M, B] =
        scalaz.EitherT.eitherTHoist[E](mSt).bind(fa)(f)

      override def get : scalaz.EitherT[E, M, S] = new scalaz.EitherT(mSt.map(mSt.get)(\/-(_)))

      override def init : scalaz.EitherT[E, M, S] = new scalaz.EitherT(mSt.map(mSt.init)(\/-(_)))

      override def point[A](a : => A) : scalaz.EitherT[E, M, A] = new scalaz.EitherT(mSt.map(mSt.point(a))(\/-(_)))

      override def put(s : S) : scalaz.EitherT[E, M, Unit] = new scalaz.EitherT(mSt.map(mSt.put(s))(\/-(_)))
    }
}
