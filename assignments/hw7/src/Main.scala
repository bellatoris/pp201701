package pp201701.hw7
import pp201701.hw7.Data.DataBundle._
import pp201701.hw7.Data.DynObj._

/*
 * ** The submitted code should be runnable. Before upload, you MUST check it
      whether Test.scala runs successfully. Otherwise you may get 0 points for
      all problems.

 * ** Compress the 'src' folder only. Don't put .sh files in the zip file.
      You can put the .iml file together, if you feel difficulty for some reasons.

 * ** Use only the features taught in class. Especially, don't use imperative
      features such as var, while, for, return, and so on. You may get deduction
      in your points.

 * ** Do not use equivalent built-in features in Scala. The core logic should be
      implemented by you.

 */

object Main {

  object PiDigit {
    // Pi = 3.1415926535..
    val digits = List(1,4,1,5,9,2,6,5,3,5, 8,9,7,9,3,2,3,8,4,6)
  }

  /* Generate a report that contains n items.
   * If is_prime is true, generate a report for primes that starts from 3.
   * Otherwise, generate a report for Pi digits.
   * If n > 20, generate an error report.
   * Use the "Primes" class in "Data.scala". Otherwise you may get deduction.
   */
  def get_report(is_prime:Boolean, n:Int) : Dyn[Report] = ???

  /* Check "Test.scala" for the exact format. */
  def pretty_printer(r:Dyn[Report]) : String = ???
}
