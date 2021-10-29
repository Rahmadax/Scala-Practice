package src.main.scala

class test {
  val fibbo = new Fib

  assert(fibbo.findAndPrintNFibboValues(1) sameElements Array(0))
  assert(fibbo.findAndPrintNFibboValues(3) sameElements Array(0, 1, 1))
  assert(fibbo.findAndPrintNFibboValues(5) sameElements Array(0, 1, 1, 2, 3))
}
