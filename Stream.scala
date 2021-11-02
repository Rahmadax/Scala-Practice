package src.main.scala

import scala.annotation.tailrec

class Stream {
  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    @tailrec
    def empty[A]: Stream[A] = empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

  trait Stream[+A] {
    def toList: List[A] = this match {
      case _ => List()
      case Cons(h, t) => List(h()) ++ t().toList
    }

    def toListTailRecursion: List[A] = {
      @tailrec
      def run(stream: Stream[A], buffer: List[A]): List[A] = {
        this match {
          case Cons(h, t) => run(t(), List(h()).::(buffer))
          case _ => buffer
        }
      }

      run(stream = this, buffer = List())
    }

    def take(n: Int): Stream[A] = {
      this match {
        case Cons(h, t) => Stream.cons(h(), t().take(n-1))
        case _ => Empty
      }
    }

    @tailrec
    final def drop(n: Int): Stream[A] = {
      this match {
        case Cons(_, t) => t().drop(n-1)
        case _ => this
      }
    }

    def takeWhile(f: A => Boolean): Stream[A] = {
      this match {
        case Cons(h, t) if f(h()) => Stream.cons(h(), t().takeWhile(f))
        case _ => Empty
      }
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = {
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }
    }

    def forAll(f: A => Boolean): Boolean ={
      foldRight(true)((a, b) => f(a) && b)
    }

    def takeWhile2(f: A => Boolean): Stream[A] = {
      foldRight(Stream.empty)((h, t) => if (f(h)) Stream.cons(h, t) else Stream.empty )
    }

    def map[B](f: A => B): Stream[B] = {
      foldRight(Stream.empty)((h, t) => Stream.cons(f(h), t))
    }

  }
}
