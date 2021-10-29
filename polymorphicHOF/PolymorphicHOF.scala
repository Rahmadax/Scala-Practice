package src.polymorphicHOF

import scala.annotation.tailrec

object PolymorphicHOF {
  def YmodXEqualsZero: (Int, Int) => Boolean = (x: Int, y: Int) => y % x == 0
  def XGreaterThanY: (Int, Int) => Boolean = (x: Int, y: Int) => x > y
  def XLessThanY: (Int, Int) => Boolean = (x: Int, y: Int) => x < y
  def allTheSame: (Int, Int) => Boolean = (x: Int, y: Int) => x == y

  def main(args: Array[String]): Unit = {
    val res = isSorted(Array(2, 2, 2, 2), allTheSame)
    println(res)
  }

  @tailrec
  final def isSorted[A](array: Array[A], f: (A, A) => Boolean): Boolean = {
    if (array.length == 1) {
      return true

    } else if (!f(array(0), array(1))){
      return false
    }

    isSorted(array.drop(1), f)
  }
}
