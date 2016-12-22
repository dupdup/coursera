package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Long, r: Long): Long = {
    if (r < 2 || c == 0 || r == c) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def tb(c: Int, cs: List[Char]): Boolean = {
      if (cs.isEmpty) c == 0
      else if (cs.head == '(') tb(c + 1, cs.tail)
      else if (cs.head == ')' && c > 0) tb(c - 1, cs.tail)
      else if (cs.head == ')' && c == 0) false
      else tb(c, cs.tail)
    }
    tb(0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
      1
    else if (money > 0 && !coins.isEmpty)
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else
      0

  }
}
