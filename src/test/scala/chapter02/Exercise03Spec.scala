package chapter02

import chapter02.Exercise03.curry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Exercise03Spec extends AnyFlatSpec with Matchers {
  "curry" should "multiply" in {
    val curryFunc = Exercise03.curry((x: Int, y: Int) => x * y)
    curryFunc(2)(3) should be(6)
  }
}
