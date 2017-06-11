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

  implicit def primeIter: Iter[Primes, Int] = new Iter[Primes, Int] {
    def getValue(a: Primes) = Some(a.prime)
    def getNext(a: Primes) = a.getNext
  }

  implicit def primeReport(n: Int): Report[Primes] = 
  new Report[Primes] {
    type A = Int
    def title(r: Primes): String = {
      if (n > 20) "<Error: Too big input>\n"
      else "<Primes " + n.toString + ">\n"
    }
    def it: Iterable[Primes, A] = new Iterable[Primes, A] {
      def iter(r: Primes): Dyn2[Iter, A] = r
    }
    def keyval: KeyVal[A] = new KeyVal[A] {
      def get_key(a: A): String = {
        def indexOfPrime: Int = {
          def findIndex(primes: Primes): Int = {
            if (primes.prime == a) 1
            else 1 + findIndex(primes.getNext)
          }
          findIndex(new Primes())
        }

        val index = indexOfPrime
        if (index > n) "None"
        else " * " + index.toString + "-th prime : " 
      }
      def get_val(a: A): String = a.toString + "\n"
    }
  }

  implicit def PiReport(n: Int): Report[List[(Int, Int)]] = 
  new Report[List[(Int, Int)]] {
    type A = (Int, Int)
    def title(r: List[(Int, Int)]): String = {
      if (n > 20) "<Error: Too big input>\n"
      else "<Pi " + n.toString + ">\n"
    }
    def it: Iterable[List[(Int, Int)], A] = new Iterable[List[(Int, Int)], A] {
      def iter(r: List[(Int, Int)]): Dyn2[Iter, A] = r 
    }
    def keyval: KeyVal[A] = new KeyVal[A] {
      def get_key(a: A): String = {
        if (a._2 > n) "None"
        else " * " + a._2.toString + "-th digit : "
      }
      def get_val(a: A): String = a._1.toString + "\n"
    }
  }

  def get_report(is_prime:Boolean, n:Int) : Dyn[Report] = {
    if (is_prime) Dyn(new Primes())(primeReport(n))
    else Dyn(PiDigit.digits
                    .zipWithIndex
                    .map(pair => (pair._1, pair._2 + 1)))(PiReport(n))
  }

    /* Check "Test.scala" for the exact format. */
  def pretty_printer(r:Dyn[Report]) : String = {
    if (r.title(r.d) == "<Error: Too big input>\n") {
      r.title(r.d)
    } else {
      val cs = r.i.it.iter(r.d) // Dyn2[Iter, A]
      def printElements[I](xs: I)(implicit proxy: Iter[I, r.i.A]): String = 
      proxy.getValue(xs) match {
        case None => ""
        case Some(n) => {
          if (r.i.keyval.get_key(n) == "None") ""
          else r.i.keyval.get_key(n) + r.i.keyval.get_val(n) + 
          printElements(proxy.getNext(xs))
        }
      }
      r.title(r.d) + printElements(cs.d)(cs.i)
    }
  }
}
