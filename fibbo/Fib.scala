package src.main.scala

import scala.annotation.tailrec

class Fib {
  def findAndPrintNFibboValues(n: Int): Array[Int] = {
    getNextFibbo(n, 0, 1, Array())
  }

  @tailrec
  final def getNextFibbo(remainingFibs: Int, firstNum: Int, secondNum: Int, existingArray: Array[Int]): Array[Int] = {
    val nextFib = firstNum + secondNum
    val newRemainingFibs = remainingFibs - 1

    val newArray = existingArray ++ Array(firstNum)

    if (newRemainingFibs == 0) {
      newArray

    } else {
      getNextFibbo(newRemainingFibs, secondNum, nextFib, newArray)
    }
  }

  // Not Tail Rec
  final def getNextFibbo(remainingFibs: Int, firstNum: Int, secondNum: Int): Array[Int] ={
    val newRemainingFibs = remainingFibs - 1

    if (newRemainingFibs == 0) {
      Array(firstNum)

    } else {
      Array (firstNum) ++ getNextFibbo(newRemainingFibs, secondNum, firstNum + secondNum)
    }
  }
}
