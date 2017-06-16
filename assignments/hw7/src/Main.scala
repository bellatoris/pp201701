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

  implicit def primeKeyVal(implicit n: Int) = new KeyVal[Int] {
    def get_key(a: Int): String = {
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
    def get_val(a: Int): String = a.toString + "\n" 
  }

  implicit def primeReport(implicit n: Int, prime: KeyVal[Int]): Report[Primes] = 
  new Report[Primes] {
    type A = Int
    def title(r: Primes): String = {
      if (n > 20) "<Error: Too big input>\n"
      else "<Primes " + n.toString + ">\n"
    }
    def it: Iterable[Primes, A] = new Iterable[Primes, A] {
      def iter(r: Primes): Dyn2[Iter, A] = r
    }
    def keyval: KeyVal[A] = prime
  }

  implicit def PiKeyVal(implicit n: Int) = new KeyVal[(Int, Int)] {
    def get_key(a: (Int, Int)): String = {
      if (a._2 > n) "None"
      else " * " + a._2.toString + "-th digit : "
    }
    def get_val(a: (Int, Int)): String = a._1.toString + "\n"
  }

  implicit def PiReport(implicit n: Int, prime: KeyVal[(Int, Int)]): Report[List[(Int, Int)]] = 
  new Report[List[(Int, Int)]] {
    type A = (Int, Int)
    def title(r: List[(Int, Int)]): String = {
      if (n > 20) "<Error: Too big input>\n"
      else "<Pi " + n.toString + ">\n"
    }
    def it: Iterable[List[(Int, Int)], A] = new Iterable[List[(Int, Int)], A] {
      def iter(r: List[(Int, Int)]): Dyn2[Iter, A] = r 
    }
    def keyval: KeyVal[A] = prime
  }

  def get_report(is_prime:Boolean, n:Int) : Dyn[Report] = {
    implicit val a: Int = n
    if (is_prime) new Primes()
    else PiDigit.digits
                .zipWithIndex
                .map(pair => (pair._1, pair._2 + 1))
  }

    /* Check "Test.scala" for the exact format. */
  def pretty_printer(r:Dyn[Report]) : String = {
    if (r.title(r.d) == "<Error: Too big input>\n") {
      r.title(r.d)
    } else {
      val cs = r.i.it.iter(r.d) // Dyn2[Iter, A]
      def printElements[I](xs: I)(implicit proxy: Iter[I, r.i.A], keyval: KeyVal[r.i.A]): String = 
      proxy.getValue(xs) match {
        case None => ""
        case Some(n) => {
          if (keyval.get_key(n) == "None") ""
          else keyval.get_key(n) + keyval.get_val(n) + 
          printElements(proxy.getNext(xs))
        }
      }
      r.title(r.d) + printElements(cs.d)(cs.i, r.i.keyval)
    }
  }
}
