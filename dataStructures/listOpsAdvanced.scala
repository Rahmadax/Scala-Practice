package src.dataStructures

import scala.annotation.tailrec

class listOpsAdvanced {

  @tailrec
  final def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B ={
    as match {
      case Nil => z
      case ::(head, tl) => foldLeft(tl, f(z, head))(f)
    }
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case ::(head, tl) => f(head, foldRight(tl, z)(f))
    }
  }

  def reverseList[A](ns: List[A]): List[A] ={
    foldLeft(ns, List[A]())((z,head) => List(head) ++ z)
  }

  def tailRecFoldRight[A,B](l: List[A], z: B, f: (A,B) => B): B = {
    foldLeft(reverseList(l), z)((b,a) => f(a,b))
  }

  // ex 3.16
  def arrayIncrementor(list: List[Int]): List[Int] ={
    list match {
      case ::(head, tl) => List(head + 1) ++ arrayIncrementor(tl)
      case Nil => Nil
    }
  }

  // ex 3.17
  def doubleListToString(list: List[Double]): List[String] = {
    list match {
      case ::(head, tl) => List(head.toString) ++  doubleListToString(tl)
      case Nil => Nil
    }
  }

  // ex 3.18
  def map[A, B](list: List[A])(f: A => B): List[B] ={
    list match {
      case ::(head, tl) => List(f(head)) ++ map(tl)(f)
      case Nil => Nil
    }
  }

  // ex 3.19
  def filter[A](list: List[A])(f: A => Boolean): List[A] = {
    list match {
      case Nil => Nil
      case ::(head, tl) if f(head) => List(head) ++ filter(tl)(f)
      case ::(_, tl) => filter(tl)(f)
    }
  }

  // ex 3.20
  def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = {
    list match {
      case Nil => Nil
      case ::(head, tl) => f(head) ++ flatMap(tl)(f)
    }
  }

  def flatten[A](list: List[List[A]]): List[A] = {
    foldLeft(list, List[A]())((z, head) => z ++ head)
  }

  def flatMap2[A, B](list: List[A])(f: A => List[B]): List[B] = {
    flatten(map(list)(f))
  }

  // ex 3.21
  def filterViaFlatMap[A](list: List[A])(f: A => Boolean): List[A] = {
    flatMap(list)(a => if (f(a)) List(a) else Nil)
  }

  // ex 3.22
  def multiListAdder(listA: List[Int], listB: List[Int]): List[Int] = {
    (listA, listB) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (a, b) => List(a.head + b.head) ++ multiListAdder(a.tail, b.tail)
    }
  }

  // ex 3.23
  def zipWith[A, B, C](listA: List[A], listB: List[B])(f: (A, B) => List[C]): List[C] ={
    (listA, listB) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (a, b) => f(a.head, b.head) ++ zipWith(a.tail, b.tail)(f)
    }
  }
}
