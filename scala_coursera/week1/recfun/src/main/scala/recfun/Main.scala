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
    def pascal(c: Int, r: Int): Int = if (c == 0 || c == r) 1 else pascal(c-1, r-1) + pascal(c, r-1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      if ( chars.isEmpty ) return true
  
      var stack = 0
      def dispatch(head: Char): Unit = {
        head match {
          case '(' => stack += 1
          case ')' => stack -= 1
          case _ => stack
        }
      }
      def inner(x: List[Char]): Unit = {
        if(x.nonEmpty) {
          dispatch(x.head)
          if(stack < 0)
            return
          inner(x.tail)
        }
      }
      
      inner(chars)
      if(stack != 0) false else true
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(money < 0) 0
      else if(money == 0) 1
      else if(coins.isEmpty && money>0) 0
      else countChange(money-coins.head, coins) + countChange(money, coins.tail)
    }
  }
