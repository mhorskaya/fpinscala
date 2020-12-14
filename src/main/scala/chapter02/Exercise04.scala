package chapter02

import scala.annotation.tailrec

object Exercise04 {
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }
}
