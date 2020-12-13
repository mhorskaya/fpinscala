package chapter02

import chapter02.Exercise01.fib
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Exercise01Spec extends AnyFlatSpec with Matchers {
  "fib" should "return the correct Fibonacci numbers" in {
    fib(0) should be(0)
    fib(1) should be(1)
    fib(2) should be(1)
    fib(3) should be(2)
    fib(4) should be(3)
    fib(5) should be(5)
    fib(6) should be(8)
    fib(7) should be(13)
    fib(8) should be(21)
    fib(9) should be(34)
    fib(10) should be(55)
    fib(11) should be(89)
    fib(12) should be(144)
  }
}
