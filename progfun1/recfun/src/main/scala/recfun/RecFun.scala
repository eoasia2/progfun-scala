package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    @tailrec
    def loop(c0: Int, r0: Int, pred: Array[Int], cur: Array[Int]): Int = {
      cur(c0) = (if (c0 > 0) pred(c0 - 1) else 0) + (if (c0 < r0) pred(c0) else 0)

      if ((c0 == c) && (r0 == r)) cur(c0)
      else if (c0 < r0) loop(c0 + 1, r0, pred, cur)
      else loop(0, r0 + 1, cur, new Array(_length = r0 + 2))
    }

    if ((c == 0) && (r == 0)) 1
    else loop(0, 1, Array(1), Array(0, 0))
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def balanceHelper(chars: List[Char], open: Int): Boolean = {
      if (open < 0) false
      else if(chars.isEmpty) open == 0
      else if (chars.head == '(') balanceHelper(chars.tail, open + 1)
      else if (chars.head == ')') balanceHelper(chars.tail, open - 1)
      else balanceHelper(chars.tail, open)
    }
    balanceHelper(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)


}
