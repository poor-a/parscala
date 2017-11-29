package parscala

import scala.language.higherKinds

import scalaz.{\/-, MonadState}

object EitherT {
  implicit def eitherTStateTInstance[M[_], S, E](implicit mSt : MonadState[M, S]) : MonadState[({type λ[A] = scalaz.EitherT[M, E, A]})#λ, S] =
    new MonadState[({type λ[A] = scalaz.EitherT[M, E, A]})#λ, S] {
      override def bind[A, B](fa : scalaz.EitherT[M, E, A])(f : A => scalaz.EitherT[M, E, B]) : scalaz.EitherT[M, E, B] = 
        scalaz.EitherT.eitherTMonad.bind(fa)(f)

      override def get : scalaz.EitherT[M, E, S] = new scalaz.EitherT(mSt.map(mSt.get)(\/-(_)))

      override def init : scalaz.EitherT[M, E, S] = new scalaz.EitherT(mSt.map(mSt.init)(\/-(_)))

      override def point[A](a : => A) : scalaz.EitherT[M, E, A] = new scalaz.EitherT(mSt.map(mSt.point(a))(\/-(_)))

      override def put(s : S) : scalaz.EitherT[M, E, Unit] = new scalaz.EitherT(mSt.map(mSt.put(s))(\/-(_)))
    }
}
