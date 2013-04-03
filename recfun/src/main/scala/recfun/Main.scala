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
    case (_, 0) => 1
    case (_, 1) => 1
    case (0, _) => 1
    case (x, y) => if (x == y) {
      1
    } else {
      pascal(x-1,y-1) + pascal(x,y-1)
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
              parens.isEmpty match {
                case true => false
                case false => {
                  parens.head match {
                    case '(' => 
                        balance_r(parens.tail, tail)
                    case _ => 
                      false
                  }
                }
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
    def countChange_r(money: Int, coins: List[Int]): Int = {
      money match {
        case 0 => 1
        case _ => {
          coins match {
            case List() => 0
            case x :: xs if money < x => countChange_r(money, xs)
            case x :: xs if money == x => 1
            case x :: xs => {
                countChange_r(money, xs) +
                countChange_r(money - x, x +: xs)
            }
          }
        }
      }
    }
    countChange_r(money, coins.sortBy(_.toInt))
  }
}
