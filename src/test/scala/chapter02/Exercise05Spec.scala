package chapter02

import chapter02.Exercise05.compose
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Exercise05Spec extends AnyFlatSpec with Matchers {
  "compose" should "double and then increment" in {
    val composedFunc = compose((x: Int) => x + 1, (y: Int) => 2 * y)
    composedFunc(2) should be(5)
  }
}
