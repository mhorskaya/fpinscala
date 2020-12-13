package chapter02

import chapter02.Exercise02.isSorted
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Exercise02Spec extends AnyFlatSpec with Matchers {
  val gt: (Int, Int) => Boolean = (x: Int, y: Int) => y > x

  "isSorted" should "detect that an array is sorted" in {
    isSorted(Array(1, 2, 3, 4, 5), gt) should be(true)
  }

  it should "detect that an array is not sorted" in {
    isSorted(Array(1, 3, 2, 5, 4), gt) should be(false)
  }

  it should "detect that an array with one element is sorted" in {
    isSorted(Array(1), gt) should be(true)
  }
}
