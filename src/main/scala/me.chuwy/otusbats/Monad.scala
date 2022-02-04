package me.chuwy.otusbats

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


trait Monad[F[_]] extends Functor[F] { self =>
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def point[A](a: A): F[A]

  def flatten[A](fa: F[F[A]]): F[A] = flatMap(fa)(f => f[A])

  def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => point(f(a)))
}

object Monad {
  implicit val monadOption: Monad[Option] = {
    new Monad[Option] {
      override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

      override def point[A](a: A): Option[A] = Option(a)

      override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.flatMap(a => point(f(a)))
    }
  }
  implicit val listMonad: Monad[List]  = new Monad[List] {
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)

    override def point[A](a: A): List[A] = List(a)

    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  implicit val futureMonad: Monad[Future] = new Monad[Future] {
    override def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa.flatMap(f)

    override def point[A](a: A): Future[A] = Future(a)

    override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
  }

  def apply [F[_]](implicit ev: Monad[F]): Monad[F] = ev

}
