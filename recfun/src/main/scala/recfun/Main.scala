package recfun
import common._

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
  def pascal(c: Int, r: Int): Int = (c, r) match {
    case (_, 0) => return 1
    case (_, 1) => return 1
    case (0, _) => return 1
    case (x, y) => if (x == y) {
      return 1
    } else {
      return pascal(x-1,y-1) + pascal(x,y-1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance_r(parens: List[Char], chars: List[Char]): Boolean = {
      chars.isEmpty match {
        case true => parens.isEmpty
        case false => {
          val head = chars.head
          val tail = chars.tail
          head match {
            case '(' => {
                  balance_r('(' +: parens, tail)
            }
            case ')' => {
              if (parens.isEmpty) return false
              parens.head match {
                case '(' => 
                    balance_r(parens.tail, tail)
                case _ => 
                  false
              }
            }
            case _ => {
              balance_r(parens, tail)
            }
          }
        }
      }
    }
    balance_r(List(), chars)
  }
        

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    0
  }
}
