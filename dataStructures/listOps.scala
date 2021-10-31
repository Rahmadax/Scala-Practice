package src.dataStructures

import scala.annotation.tailrec

class listOps {
  // x 3.2
  def tail[A](l: A): List[A] ={
    l match {
      case (_, b) => b
      case Nil => Nil
    }
  }

  // ex 3.3
  def setHead[A](newHead: A, l: A): List[A] ={
    l match {
      case (_, b) => List(newHead, b)
      case Nil => Nil
    }
  }

  // ex 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) {
      return l
    }

    l match {
      case ::(_, tl) => drop(tl, n-1)
      case Nil => Nil
    }
  }

  // ex 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case ::(head, tl) if f(head) => dropWhile(tl, f)
      case _ => l
    }
  }

  // ex 3.6
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("init of empty list")
      case ::(_,Nil) => Nil
      case ::(head, tl) => List(head, init(tl))
    }
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case ::(head, tl) => f(head, foldRight(tl, z)(f))
    }
  }

  // ex 3.9
  def countFoldRight[A](as: List[A]): Int = {
    foldRight(as, 0)(_ + 1)
  }

  // ex 3.10
  @tailrec
  final def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B ={
    as match {
      case Nil => z
      case ::(head, tl) => foldLeft(tl, f(z, head))(f)
    }
  }

  // ex 3.11
  def productFoldLeft(ns: List[Double]): Double ={
    foldLeft(ns, 1.0)(_*_)
  }

  def sumFoldLeft(ns: List[Int]): Int = {
    foldLeft(ns, 0)(_+_)
  }

  def countFoldLeft[A](ns: List[A]): Int = {
    foldLeft(ns, 0)((a,_) => a + 1)
  }

  // ex 3.12
  def reverseList[A](ns: List[A]): List[A] ={
    foldLeft(ns, List[A]())((z,head) => List(head) ++ z)
  }

  // ex 3.13
  def tailRecFoldRight[A,B](l: List[A], z: B, f: (A,B) => B): B = {
    foldLeft(reverseList(l), z)((b,a) => f(a,b))
  }

  // ex 3.14
  def appendFoldLeft[A](listA: List[A], listB: List[A]): List[A] = {
    foldLeft(listB, listA)((z, listBEntry) => z ++ List(listBEntry))
  }

  // ex 3.15
  def flatten[A](list: List[List[A]]): List[A] = {
    foldLeft(list, List[A]())((z, head) => z ++ head)
  }
}
