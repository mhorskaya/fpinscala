package chapter02

import scala.annotation.tailrec

object Exercise01 {
  def fib(n: Int): Int = {
    @tailrec
    def go(x: Int, y: Int, z: Int): Int = {
      if (x == 0) y
      else go(x - 1, z, y + z)
    }

    go(n, 0, 1)
  }
}
