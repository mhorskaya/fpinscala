package chapter02

import scala.annotation.tailrec

object Exercise05 {
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
}
