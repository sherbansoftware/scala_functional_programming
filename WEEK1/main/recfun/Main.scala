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
    * For this example I'have inspired from this link:
    * http://codereview.stackexchange.com/questions/74262/calculating-a-specific-entry-in-a-pascal-s-triangle-recursively
    *
    * public int getPascalValue(int row, int column) {
    * if (column == 0 || column == row) {
    * return 1;
    * }
    * return getPascalValue(row - 1, column - 1) + getPascalValue(row - 1, column);
    * }
    *
    * Then I've adapted the Java Code to Scala
    */
  def pascal(c: Int, r: Int): Int = if (c > r) 0 else if (c == 0 || c == r) 1 else {
    pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    * ==Pseudocode==
    * FUNCTION isBalanced(String input, String stack) : boolean
    * IF isEmpty(input)
    * RETURN isEmpty(stack)
    * ELSE IF isOpen(firstChar(input))
    * RETURN isBalanced(allButFirst(input), stack + firstChar(input))
    * ELSE IF isClose(firstChar(input))
    * RETURN NOT isEmpty(stack) AND isMatching(firstChar(input), lastChar(stack))
    * AND isBalanced(allButFirst(input), allButLast(stack))
    * ELSE
    * ERROR "Invalid character"
    * ==Pseudocode==
    *
    */
  def balance(chars: List[Char]): Boolean = {

    @scala.annotation.tailrec
    def containsParenthesisRecursive(listOfChars: List[Char], numberOfOperations: Int = 0): Boolean = {
      if (listOfChars.isEmpty && numberOfOperations == 0) true
      else if (listOfChars.isEmpty) false
      else if (listOfChars.head == '(') containsParenthesisRecursive(listOfChars.tail, numberOfOperations + 1); //posiitve number for )
      else if (listOfChars.head == ')') numberOfOperations > 0 && containsParenthesisRecursive(listOfChars.tail, numberOfOperations - 1); //negative number for (
      else containsParenthesisRecursive(listOfChars.tail, numberOfOperations)
    }

    containsParenthesisRecursive(chars)
  }

  /**
    * Exercise 3
    * Inspired from pseudocode:
    * === Pseudocode ===
    * def count( n, m ):
    * if n < 0 or m <= 0: #m < 0 for zero indexed programming languages
    * return 0
    * if n == 0: # needs be checked after n & m, as if n = 0 and m < 0 then it would return 1, which should not be the case.
    * return 1
    * *
    * return count( n, m - 1 ) + count( n - S[m], m )
    */
  def countChange(amount: Int, uniqueDenominaitonsCoins: List[Int]): Int = {
    if (amount < 0 || uniqueDenominaitonsCoins.isEmpty) 0
    else if (amount == 0) 1
    else countChange(amount, uniqueDenominaitonsCoins.tail) + countChange(amount - uniqueDenominaitonsCoins.head, uniqueDenominaitonsCoins)
  }
}
