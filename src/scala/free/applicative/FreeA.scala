package free.applicative

import scalaz._
import syntax.applicative._

import FreeA._

object FreeA extends FreeAInstances with FreeAFunctions {

  case class Pure[F[+ _], +A](a: A) extends FreeA[F, A]

  case class Ap[F[+ _], A, +B](fa: F[A], apf: FreeA[F, A => B]) extends FreeA[F, B]

}

sealed abstract class FreeA[F[+ _], +A] {

  def map[B](f: A => B): FreeA[F, B] = this match {
    case Pure(a) => Pure(f(a))
    case Ap(fa: F[A], apf) => Ap(fa, apf map (f compose _))
  }

  def <*>[B](freeA: FreeA[F, A => B]): FreeA[F, B] = this match {
    case Pure(a) => freeA map (_(a))
    case Ap(fa: F[A], apf) => {
      val fAB: FreeA[F, (Any => A) => (Any => B)] = freeA map (x => x.compose _)
      Ap(fa, apf <*> fAB)
    }
  }

  def run[B, G[+ _] : Applicative](u: F[B] => G[B]): G[A] =
    this match {
      case Pure(x) => Applicative[G].pure(x)
      case Ap(f: F[B], x: FreeA[F, B => A]) => u(f) <*> x.run(u)
    }

  def hoistAp[F[+ _] : Functor, G[+ _] : Functor, B](f: F[B] => G[B]): FreeA[G, A] =
    this match {
      case Pure(a) => Pure(a)
      case Ap(x: F[B], y) => Ap(f(x), y.hoistAp(f))
    }

}

trait FreeAInstances {

  implicit def freeApplicative[F[+ _] : Functor]: Applicative[({type λ[α] = FreeA[F, α]})#λ] =
    new Applicative[({type λ[α] = FreeA[F, α]})#λ] {
      def point[A](a: => A) = FreeA.pure(a)

      override def map[A, B](fa: FreeA[F, A])(f: A => B): FreeA[F, B] = fa map f

      def ap[A, B](fa: => FreeA[F, A])(f: => FreeA[F, A => B]): FreeA[F, B] = fa <*> f
    }
}

trait FreeAFunctions {
  def liftFreeA[F[+ _] : Functor, A](x: F[A]): FreeA[F, A] = Ap[F, A, A](x, Pure(identity _))

  def pure[F[+ _] : Functor, A](a: => A) = Pure[F, A](a)
}