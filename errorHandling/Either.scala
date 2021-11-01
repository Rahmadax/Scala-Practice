package src.main.scala

import scala.language.higherKinds

class Either {
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  trait Either[+E, +A] {
    def Try[A](a: => A): Either[Exception, A] = {
      try Right(a)
      catch { case e: Exception => Left(e) }
    }

    def map[B](f: A => B): Either[E, B] = {
      this match {
        case Left(error) => Left(error)
        case Right(value) => Right(f(value))
      }
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
      this match {
        case Left(error) => Left(error)
        case Right(value) => f(value)
      }
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
      this match {
        case Left(_) => b
        case Right(value) => Right(value)
      }
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
      for {
        ax <- this
        bx <- b
      } yield f(ax,bx)
    }
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    println("Running")

    val Option = new Option

    Option.runTest()
  }
}