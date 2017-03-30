package pp201701.hw2
import pp201701.hw2.Data._
import scala.annotation.tailrec
/*
 * ** The submitted code should be runnable. Before upload, you MUST check it
      whether Test.scala runs successfully. Otherwise you may get 0 points for
      all problems.

 * ** Compress the 'src' folder only. Don't put .sh files in the zip file.
      You can put the .iml file together, if you feel difficulty for some reasons.

 * ** Use only the features taught in class. Especially, don't use imperative
      features such as while, for, return, and so on. You may get deduction
      in your points.
 */

/*
 Implement below functions, which is currently blank. (???)
 */

object Main {
  /*
   Exercise 1: IList Map
   Write a map function that applies the given function to all elements of the given IList.
   */
  def map(xs: IList)(f: Int => Int): IList = {
    @tailrec
    def nestedMap(xs: IList, mapped: IList): IList = xs match {
      case INil() => mapped
      case ICons(hd, tl) => nestedMap(tl, ICons(f(hd), mapped))
    }
  
    reverse(nestedMap(xs, INil()))
  }

  /*
   Exercise 2: IList Reverse
   Write a reverse function that reverses the order of the given IList.
   */
  def reverse(xs: IList): IList = {
    @tailrec
    def nestedReverse(xs: IList, reversed: IList): IList = xs match {
      case INil() => reversed
      case ICons(hd, tl) => nestedReverse(tl, ICons(hd, reversed))
    }

    nestedReverse(xs, INil())
  }

  /*
   Exercise 3: Exp Calculator
   Given Exp, calculate the result of Int value.
   For each case class Add/Sub/Mul, you may interpret them as
   normal integer operators: +, -, *.
   */
  def calculate(x: Exp): Int = x match {
    case EInt(i) => i
    case EAdd(lhs, rhs) => calculate(lhs) + calculate(rhs)
    case ESub(lhs, rhs) => calculate(lhs) - calculate(rhs)
    case EMul(lhs, rhs) => calculate(lhs) * calculate(rhs)
  }
}
