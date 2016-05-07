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
  // @tailrec
  def pascal(c: Int, r: Int): Int = {
    if (r == 0 || c == 0 || c == r)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def innerBalance(chars: List[Char], counter: Int): Boolean = {
      if(chars.isEmpty || counter < 0) {
        (counter == 0)
      }
      else {
        chars.head match {
          case '(' => innerBalance(chars.tail, counter + 1)
          case ')' => innerBalance(chars.tail, counter - 1)
          case _ => innerBalance(chars.tail, counter)
        }
      }
    }
    innerBalance(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {


  }

}
