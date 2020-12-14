package chapter02

import chapter02.Exercise04.uncurry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Exercise04Spec extends AnyFlatSpec with Matchers {
  "uncurry" should "multiply" in {
    val uncurryFunc = uncurry((x: Int) => (y: Int) => x * y)
    uncurryFunc(2, 3) should be(6)
  }
}
