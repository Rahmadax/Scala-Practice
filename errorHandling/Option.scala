package src.errorHandling

class Option {
  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  // ex 4.1
  trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(value) => Some(f(value))
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(value) => f(value)
    }

    def flatMap2[B](f: A => Option[B]): Option[B] = {
      map(f).getOrElse(None)
    }


    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(value) => value
    }


    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case None => ob
      case Some(_) => this
    }

    def orElse2[B >: A](ob: => Option[B]): Option[B] = {
      map(Some(_)).getOrElse(ob)
    }


    def filter(f: A => Boolean): Option[A] = this match {
      case Some(value) if f(value) => Some(value)
      case _ => None
    }
  }
}
